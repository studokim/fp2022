(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module VarMap = Map.Make (String)

let check_types_equality left right = left = right

open Base
open Utils
open Stdint
open Ast

type value =
  | IVInt of Int32.t
  | IVInt8 of Int8.t
  | IVInt16 of Int16.t
  | IVChar of char
  | IVString of string

type old_variable = { var_type : ctype; var_value : value option }
type variable = { var_type : ctype; var_value : value option; var_addr : int }

let type_of_value = function
  | IVInt _ -> TInt32
  | IVInt8 _ -> TInt8
  | IVInt16 _ -> TInt16
  | IVChar _ -> TChar
  | IVString _ -> TPointer TChar

let get_type_size = function
  | TChar -> 1
  | TInt8 -> 1
  | TInt16 -> 2
  | TInt32 -> 4
  | TPointer _ -> 0
  | TVoid -> 0

type context = {
  return_type : Ast.ctype;
  func_name : name;
  var_map : variable VarMap.t;
  heap : Bytes.t;
  free_byte : int;
  (* ctype field stands for the pointer type, first int filed - size of the memory block in bytes,
     second int - index of the start of the block in the heap*)
  occupied_list : (ctype * int * int) list;
  func_list : function_list;
}

let not_implemented = ExpressionExpected "Not IMPLEMENTEEED"
let init_heap = Bytes.create 1000

let main_context func_type =
  {
    func_name = "main";
    return_type = func_type;
    var_map = VarMap.empty;
    heap = init_heap;
    free_byte = 0;
    occupied_list = [];
    func_list = [];
  }

let match_var_value x var_name =
  match x.var_value with
  | Some value -> Result.return value
  | None ->
      Result.fail
        (VarWithoutValue
           ("Variable " ^ var_name ^ " is used, but doesn't have a value."))

let take_var var_name var_map =
  match VarMap.find_opt var_name var_map with
  | Some x -> (
      match x.var_value with
      | Some _ -> Result.return x
      | None ->
          Result.fail
            (VarWithoutValue
               ("Variable " ^ var_name ^ " is used, but doesn't have a value."))
      )
  | None -> Result.fail (UnknownVariable ("Unknown variable " ^ var_name ^ "."))

let take_pointer p_name var_map =
  match VarMap.find_opt p_name var_map with
  | Some var -> Result.return var
  | None -> Result.fail (UnknownVariable ("Unknown variable " ^ p_name ^ "."))

let update_var var_name value addr var_map =
  match VarMap.find_opt var_name var_map with
  | Some variable ->
      Result.return
        (VarMap.add var_name
           { variable with var_addr = addr; var_value = value }
           var_map)
  | None -> Result.fail (UnknownVariable ("Unknown variable " ^ var_name ^ "."))

module PointerInterpret : sig
  val malloc :
    ctype -> name -> int -> context -> (context, Utils.error) Result.t

  val define_pointer : ctype -> name -> context -> context
  val dereference : ctype -> int -> context -> (value, Utils.error) Result.t
  val get_i : variable -> int -> context -> (value, Utils.error) Result.t
  val cast_pointer : ctype -> variable -> (variable, Utils.error) Result.t

  val assign_string :
    name -> context -> string -> (context, Utils.error) Result.t

  val assign_dereference :
    ctype -> int -> value -> context -> (context, Utils.error) Result.t

  val assign_array_elem :
    name ->
    variable ->
    int ->
    value ->
    ctype ->
    context ->
    (context, Utils.error) Result.t

  val assign_p2p :
    name -> variable -> context -> (context, Utils.error) Result.t

  val p_arithmetics :
    variable ->
    value ->
    (int32 -> int32 -> int32) ->
    context ->
    (variable, Utils.error) Result.t

  (* val free : name -> context -> (context, Utils.error) Result.t *)
  (* val dereference : name -> context -> (value, Utils.error) Result.t
     val get_address : name -> context -> (int, Utils.error) Result.t *)
end = struct
  let define_pointer p_type p_name ctx =
    {
      ctx with
      var_map =
        VarMap.add p_name
          { var_type = p_type; var_value = None; var_addr = -1 }
          ctx.var_map;
    }

  let malloc p_type p_name p_size ctx =
    match p_type with
    | TPointer (TPointer _) | TPointer TVoid -> Result.fail not_implemented
    | TPointer m_type ->
        if ctx.free_byte + p_size < Bytes.length ctx.heap then
          let new_list =
            List.append ctx.occupied_list
              [ (TPointer m_type, p_size, ctx.free_byte) ]
          in
          let new_free_byte = ctx.free_byte + p_size in
          let new_var_map =
            update_var p_name
              (Option.some (IVInt (Int32.of_int (List.length new_list - 1))))
              ctx.free_byte ctx.var_map
          in
          Result.( >>= ) new_var_map (fun x ->
              Result.return
                {
                  ctx with
                  occupied_list = new_list;
                  free_byte = new_free_byte;
                  var_map = x;
                })
        else
          Result.fail
            (MemoryEnd "You have run out of available memory by calling malloc")
    | _ -> Result.fail not_implemented

  let dereference p_type p_addr ctx =
    match (p_type, p_addr) with
    | TPointer (TPointer _), _ | TPointer TVoid, _ ->
        Result.fail not_implemented
    | TPointer _, -1 ->
        Result.fail
          (VarWithoutValue
             "You are trying to dereference pointer that isn't initialized \
              with value yet."
             (*^ p_name
               ^ ", but it doesn't have a value")*))
    | TPointer vtype, addr -> (
        match vtype with
        | TChar ->
            if addr < Bytes.length ctx.heap then
              Result.return (IVChar (Bytes.get ctx.heap addr))
            else
              Result.fail
                (MemoryEnd
                   "You are trying to dereference pointer, but its too close \
                    to the heap bounderies")
        | TInt8 ->
            if addr < Bytes.length ctx.heap then
              Result.return (IVInt8 (Int8.of_bytes_little_endian ctx.heap addr))
            else
              Result.fail
                (MemoryEnd
                   "You are trying to dereference pointer, but its too close \
                    to the heap bounderies")
        | TInt16 ->
            if addr < Bytes.length ctx.heap - 1 then
              Result.return
                (IVInt16 (Int16.of_bytes_little_endian ctx.heap addr))
            else
              Result.fail
                (MemoryEnd
                   "You are trying to dereference pointer, but its too close \
                    to the heap bounderies")
        | TInt32 ->
            if addr < Bytes.length ctx.heap - 3 then
              Result.return (IVInt (Int32.of_bytes_little_endian ctx.heap addr))
            else
              Result.fail
                (MemoryEnd
                   "You are trying to dereference pointer, but its too close \
                    to the heap bounderies")
        | TVoid ->
            Result.fail (WrongVariableType "Attempt to deference void pointer.")
        | TPointer _ -> Result.fail not_implemented)
    | _ -> Result.fail not_implemented

  let get_i var index ctx =
    match var with
    | { var_type = TPointer (TPointer _); var_value = _; _ } ->
        Result.fail not_implemented
    | { var_type = TPointer _; var_value = None; var_addr = -1 } ->
        Result.fail
          (VarWithoutValue
             "You are trying to take the element of the uninitialized array.")
    | {
     var_type = TPointer vtype;
     var_value = Some (IVInt value);
     var_addr = addr;
    } -> (
        match List.nth ctx.occupied_list (Int32.to_int value) with
        | Some (_, p_size, _) ->
            if index < p_size / get_type_size vtype then
              match vtype with
              | TChar ->
                  Result.return
                    (IVChar
                       (Bytes.get ctx.heap
                          (addr + (get_type_size vtype * index))))
              | TInt8 ->
                  Result.return
                    (IVInt8
                       (Int8.of_bytes_little_endian ctx.heap
                          (addr + (get_type_size vtype * index))))
              | TInt16 ->
                  Result.return
                    (IVInt16
                       (Int16.of_bytes_little_endian ctx.heap
                          (addr + (get_type_size vtype * index))))
              | TInt32 ->
                  Result.return
                    (IVInt
                       (Int32.of_bytes_little_endian ctx.heap
                          (addr + (get_type_size vtype * index))))
              | TVoid ->
                  Result.fail
                    (WrongVariableType "Attempt to deference void pointer.")
              | TPointer _ -> Result.fail not_implemented
            else
              Result.fail
                (IndexOutOfRange
                   "Index of the pointer is greater than pointer size")
        | None -> Result.fail not_implemented)
    | _ -> Result.fail not_implemented

  let cast_pointer new_type var =
    match (new_type, var.var_type) with
    | TPointer (TPointer _), _
    | TPointer TVoid, _
    | _, TPointer (TPointer _)
    | _, TPointer TVoid ->
        Result.fail not_implemented
    | TPointer _, TPointer _ -> Result.return { var with var_type = new_type }
    | _ ->
        Result.fail
          (TypeMismatch
             "Error while assigning value to the pointer with unmatchable type.")

  let assign_string p_name ctx p_str =
    let possible_ctx =
      match take_pointer p_name ctx.var_map with
      | Result.Ok
          {
            var_type = TPointer TChar;
            var_value = Some (IVInt value);
            var_addr = _;
          } -> (
          match List.nth ctx.occupied_list (Int32.to_int value) with
          | Some (_, p_size, _) ->
              if p_size < String.length p_str then
                malloc (TPointer TChar) p_name (String.length p_str) ctx
              else Result.return ctx
          | None ->
              Result.fail
                (UnknownVariable
                   "Pointer isn't matched with one of the occupied memory \
                    blocks."))
      | Result.Ok _ -> malloc (TPointer TChar) p_name (String.length p_str) ctx
      | Result.Error err -> Result.fail err
    in
    match possible_ctx with
    | Result.Error err -> Result.fail err
    | Result.Ok new_ctx -> (
        let ptr = take_pointer p_name new_ctx.var_map in
        match ptr with
        | Result.Ok fp ->
            if fp.var_addr + String.length p_str < Bytes.length new_ctx.heap
            then
              let () =
                Stdlib.Bytes.blit_string p_str 0 new_ctx.heap fp.var_addr
                  (String.length p_str)
              in
              Result.return new_ctx
            else
              Result.fail
                (MemoryEnd
                   "You have run out of available memory by assigning char \
                    pointer a long string")
        | Result.Error err -> Result.fail err)

  let assign_dereference p_type p_addr p_val ctx =
    match (p_type, p_val) with
    | TPointer TChar, IVChar x ->
        if p_addr < Bytes.length ctx.heap then
          let () = Stdlib.Bytes.set ctx.heap p_addr x in
          Result.return ctx
        else
          Result.fail
            (MemoryEnd
               "You are trying to assign value to the pointer, but its too \
                close to the heap bounderies")
    | TPointer TInt8, IVInt8 x ->
        if p_addr < Bytes.length ctx.heap then
          let () = Stdlib.Bytes.set_int8 ctx.heap p_addr (Int8.to_int x) in
          Result.return ctx
        else
          Result.fail
            (MemoryEnd
               "You are trying to assign value to the pointer, but its too \
                close to the heap bounderies")
    | TPointer TInt16, IVInt16 x ->
        if p_addr < Bytes.length ctx.heap - 1 then
          let () = Stdlib.Bytes.set_int16_le ctx.heap p_addr (Int16.to_int x) in
          Result.return ctx
        else
          Result.fail
            (MemoryEnd
               "You are trying to assign value to the pointer, but its too \
                close to the heap bounderies")
    | TPointer TInt32, IVInt x ->
        if p_addr < Bytes.length ctx.heap - 3 then
          let () = Stdlib.Bytes.set_int32_le ctx.heap p_addr x in
          Result.return ctx
        else
          Result.fail
            (MemoryEnd
               "You are trying to assign value to the pointer, but its too \
                close to the heap bounderies")
    | _ ->
        Result.fail
          (TypeMismatch
             "While assigning value to dereference, dereference type does not \
              match the assigned value type.")

  let assign_array_elem array_name array_var index new_val new_type ctx =
    let open Result in
    match (array_var.var_value, array_var.var_type) with
    | Some (IVInt number), TPointer p_type -> (
        match List.nth ctx.occupied_list (Int32.to_int number) with
        | Some (_, p_size, _) ->
            if check_types_equality array_var.var_type (TPointer new_type) then
              if index * get_type_size p_type < p_size then
                assign_dereference array_var.var_type
                  (array_var.var_addr + (index * get_type_size p_type))
                  new_val ctx
                >>= fun new_ctx -> Result.return new_ctx
              else
                Result.fail
                  (IndexOutOfRange
                     ("Index of the " ^ array_name ^ " is out of range"))
            else
              Result.fail
                (TypeMismatch
                   ("Types of the " ^ array_name
                  ^ " array and its new value are different"))
        | None ->
            Result.fail
              (UnknownVariable
                 "Pointer isn't matched with one of the occupied memory blocks.")
        )
    | None, _ ->
        Result.fail
          (VarWithoutValue
             "You are trying to get an element of the not initialized array")
    | _ -> Result.fail Unreachable

  let assign_p2p left_name right_ptr ctx =
    match take_pointer left_name ctx.var_map with
    | Result.Ok left_var ->
        if check_types_equality left_var.var_type right_ptr.var_type then
          match
            update_var left_name right_ptr.var_value right_ptr.var_addr
              ctx.var_map
          with
          | Result.Ok var_map -> Result.return { ctx with var_map }
          | Result.Error err -> Result.fail err
        else
          Result.fail
            (TypeMismatch
               "While assigning value to pointer, pointer type does not match \
                the assigned value type.")
    | Result.Error err -> Result.fail err

  let p_arithmetics left_var right_val int_func ctx =
    let value_to_int32 = function
      | IVInt x -> Result.return x
      | IVInt16 x -> Result.return (Int16.to_int32 x)
      | IVInt8 x -> Result.return (Int8.to_int32 x)
      | IVChar x -> (
          match Int.to_int32 (Char.to_int x) with
          | Some number -> Result.return number
          | None -> Result.fail (TypeMismatch "Unable to convert int to int32"))
      | IVString _ ->
          Result.fail (TypeMismatch "Trying to add string to pointer.")
    in
    match value_to_int32 right_val with
    | Result.Ok int32_val ->
        let real_expr =
          Int32.to_int (int_func (Int32.of_int left_var.var_addr) int32_val)
        in
        let plus_expr =
          Int32.to_int
            (Base.Int32.( + ) int32_val (Int32.of_int left_var.var_addr))
        in
        let minus_expr =
          Int32.to_int
            (Base.Int32.( - ) int32_val (Int32.of_int left_var.var_addr))
        in
        if real_expr = plus_expr || real_expr = minus_expr then
          if
            Int32.to_int (int_func (Int32.of_int left_var.var_addr) int32_val)
            < Bytes.length ctx.heap
            && Int32.to_int
                 (int_func (Int32.of_int left_var.var_addr) int32_val)
               >= 0
          then
            Result.return
              {
                left_var with
                var_addr =
                  Int32.to_int
                    (int_func (Int32.of_int left_var.var_addr) int32_val);
              }
          else
            Result.fail
              (MemoryEnd
                 "Due to the arithmetic operations on pointers, you've reached \
                  out of memory limit")
        else
          Result.fail
            (WrongVariableType "Forbidden operations with pointers and values.")
    | Result.Error err -> Result.fail err
end

module Interpret : sig
  val func_interpreter :
    Ast.program_function -> context -> (value, Utils.error) Result.t

  val all_ops : expression -> context -> (value * ctype, error) Result.t

  val interpret_st_block :
    statement list ->
    (context, Utils.error) Result.t ->
    bool ->
    (value option * context * bool, Utils.error) Result.t

  val resolve_condition : expression -> context -> (bool, error) Result.t
  val interpret_expr : expression -> context -> (context, error) Result.t
  val const_to_value : const * ctype * name -> (value, error) Result.t
  val run : Ast.function_list -> (value, Utils.error) Result.t
end = struct
  include PointerInterpret

  let const_to_value = function
    | Ast.VInt x, TInt32, _ -> Result.return (IVInt x)
    | Ast.VInt x, TInt16, _ -> Result.return (IVInt16 (Int16.of_int32 x))
    | Ast.VInt x, TInt8, _ -> Result.return (IVInt8 (Int8.of_int32 x))
    | Ast.VInt x, TChar, _ ->
        if Int32.to_int x <= 255 && Int32.to_int x >= 0 then
          Result.return (IVChar (Char.of_int_exn (Int32.to_int x)))
        else
          Result.fail
            (TypeMismatch
               "Trying to convert int number not in char boundaries in char.")
    | Ast.VInt _, TPointer _, var_name ->
        Result.fail
          (WrongVariableType
             (var_name ^ " type and assigned value type are mismatched."))
    | Ast.VString x, TPointer TChar, _ -> Result.return (IVString x)
    | _, _, var_name ->
        Result.fail
          (WrongVariableType
             (var_name ^ " type and assigned value type are mismatched."))

  let rec all_ops expr ctx =
    let my_logic_op op x y = match op x y with true -> 1l | false -> 0l in
    let int32_and x y = match (x, y) with 0l, _ | _, 0l -> 0l | _, _ -> 1l in
    let int32_or x y = match (x, y) with 0l, 0l -> 0l | _, _ -> 1l in
    match expr with
    | Add _ | Sub _ | Mul _ | Div _ | Mod _ | Variable _ | Value _ | UnaryMin _
    | Pointer _ | ArrayElem _ | FuncCall _ | Cast _ ->
        arithmetics expr ctx
    | Or (x, y) -> interpret_bin_op x y int32_or false ctx
    | And (x, y) -> interpret_bin_op x y int32_and false ctx
    | Equal (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( = )) false ctx
    | Less (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( < )) false ctx
    | LessOrEq (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( <= )) false ctx
    | More (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( > )) false ctx
    | MoreOrEq (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( >= )) false ctx
    | NotEqual (x, y) ->
        interpret_bin_op x y (my_logic_op Base.Int32.( <> )) false ctx
    | _ -> Result.fail not_implemented

  and interpret_bin_op left right op div_flag ctx =
    let open Result in
    all_ops left ctx >>= fun x ->
    all_ops right ctx >>= fun y ->
    match (x, y) with
    | (_, TPointer (TPointer _)), (_, _) -> Result.fail not_implemented
    | (_, TPointer _), (_, TPointer _) -> Result.fail not_implemented
    | (IVInt addr, TPointer p_type), (yy, _)
    | (yy, _), (IVInt addr, TPointer p_type) -> (
        let p_var =
          {
            var_type = TPointer p_type;
            var_value = None;
            var_addr = Int32.to_int addr;
          }
        in
        match p_arithmetics p_var yy op ctx with
        | Result.Ok new_p ->
            Result.return
              (IVInt (Int32.of_int new_p.var_addr), TPointer new_p.var_type)
        | Result.Error err -> Result.fail err)
    | (IVInt xx, TInt32), (IVInt yy, TInt32) ->
        if div_flag && Base.Int32.( = ) yy 0l then
          fail (DivisionByZero "division b zero")
        else return (IVInt (op xx yy), TInt32)
    | (_, TVoid), (_, _) | (_, _), (_, TVoid) ->
        fail (TypeMismatch "Operation with void values")
    | (IVInt16 xx, TInt16), (IVInt16 yy, TInt16) ->
        if div_flag && Int16.to_int yy = 0 then
          fail (DivisionByZero "division b zero")
        else
          return
            ( IVInt16
                (Int32.to_int16 (op (Int16.to_int32 xx) (Int16.to_int32 yy))),
              TInt16 )
    | (IVInt8 xx, TInt8), (IVInt8 yy, TInt8) ->
        if div_flag && Int8.to_int yy = 0 then
          fail (DivisionByZero "division b zero")
        else
          return
            ( IVInt8 (Int32.to_int8 (op (Int8.to_int32 xx) (Int8.to_int32 yy))),
              TInt8 )
    | (IVChar xx, TChar), (IVChar yy, TChar) ->
        if div_flag && Char.to_int yy = 0 then
          fail (DivisionByZero "division b zero")
        else
          let new_val =
            Int32.to_int
              (op
                 (Int32.of_int (Char.to_int xx))
                 (Int32.of_int (Char.to_int yy)))
          in
          if new_val <= 255 && new_val >= 0 then
            return (IVChar (Char.of_int_exn new_val), TChar)
          else
            Result.fail
              (TypeMismatch
                 "Trying to convert int number not in char boundaries in char.")
    | (_, _), (_, _) -> fail (TypeMismatch "Operation with unmatchable types")

  and arithmetics expr ctx =
    let open Result in
    let cast_val old_val new_type =
      match (old_val, new_type) with
      | IVInt x, TInt32 -> return (IVInt x)
      | IVInt x, TInt16 -> return (IVInt16 (Int16.of_int32 x))
      | IVInt x, TInt8 -> return (IVInt8 (Int8.of_int32 x))
      | IVInt x, TChar ->
          if Int32.to_int x <= 255 && Int32.to_int x >= 0 then
            Result.return (IVChar (Char.of_int_exn (Int32.to_int x)))
          else
            Result.fail
              (TypeMismatch
                 "Trying to convert int number not in char boundaries in char.")
      | IVInt16 x, TInt32 -> return (IVInt (Int32.of_int16 x))
      | IVInt16 x, TInt16 -> return (IVInt16 (Int16.of_int16 x))
      | IVInt16 x, TInt8 -> return (IVInt8 (Int8.of_int16 x))
      | IVInt16 x, TChar ->
          if Int16.to_int x <= 255 && Int16.to_int x >= 0 then
            Result.return (IVChar (Char.of_int_exn (Int16.to_int x)))
          else
            Result.fail
              (TypeMismatch
                 "Trying to convert int number not in char boundaries in char.")
      | IVInt8 x, TInt32 -> return (IVInt (Int32.of_int8 x))
      | IVInt8 x, TInt16 -> return (IVInt16 (Int16.of_int8 x))
      | IVInt8 x, TInt8 -> return (IVInt8 (Int8.of_int8 x))
      | IVInt8 x, TChar ->
          if Int8.to_int x <= 255 && Int8.to_int x >= 0 then
            Result.return (IVChar (Char.of_int_exn (Int8.to_int x)))
          else
            Result.fail
              (TypeMismatch
                 "Trying to convert int number not in char boundaries in char.")
      | IVChar x, TInt32 -> return (IVInt (Int32.of_int (Char.to_int x)))
      | IVChar x, TInt16 -> return (IVInt16 (Int16.of_int (Char.to_int x)))
      | IVChar x, TInt8 -> return (IVInt8 (Int8.of_int (Char.to_int x)))
      | IVChar x, TChar -> return (IVChar x)
      | _ ->
          fail
            (TypeMismatch "Failed to convert value to new type while casting")
    in
    match expr with
    | Add (x, y) -> interpret_bin_op x y Int32.( + ) false ctx
    | Sub (x, y) -> interpret_bin_op x y Int32.( - ) false ctx
    | Mul (x, y) -> interpret_bin_op x y Int32.( * ) false ctx
    | Div (x, y) -> interpret_bin_op x y Int32.( / ) true ctx
    | Mod (x, y) -> interpret_bin_op x y Base.Int32.( % ) true ctx
    | UnaryMin x -> (
        all_ops x ctx >>= function
        | IVInt x, TInt32 -> return (IVInt (Int32.neg x), TInt32)
        | IVInt16 x, TInt16 -> return (IVInt16 (Int16.neg x), TInt16)
        | IVInt8 x, TInt8 -> return (IVInt8 (Int8.neg x), TInt8)
        | IVChar _, TChar -> fail (TypeMismatch "Tring to negate char")
        | _ -> fail not_implemented)
    | Value (VInt x) -> return (IVInt x, TInt32)
    | Value (VChar x) -> return (IVChar x, TChar)
    | Value (VString x) -> return (IVString x, TPointer TChar)
    | Variable x -> (
        take_pointer x ctx.var_map >>= fun variable ->
        match variable with
        | { var_type = TPointer p_type; var_value = _; var_addr = addr }
          when addr <> -1 ->
            Result.return (IVInt (Int32.of_int addr), TPointer p_type)
        | { var_type = _; var_value = Some _; var_addr = -1 } ->
            match_var_value variable x >>= fun var_value ->
            Result.return (var_value, variable.var_type)
        | _ ->
            Result.fail
              (WrongVariableType
                 "You are trying to get an uninitialized pointer value"))
    | Pointer (Variable x) -> (
        take_pointer x ctx.var_map >>= fun variable ->
        match variable.var_type with
        | TPointer (TPointer _) -> fail not_implemented
        | TPointer p_type ->
            dereference variable.var_type variable.var_addr ctx
            >>= fun var_value -> return (var_value, p_type)
        | _ -> fail not_implemented)
    | ArrayElem (arr_name, index_expr) -> (
        take_pointer arr_name ctx.var_map >>= fun variable ->
        all_ops index_expr ctx >>= function
        | IVInt x, TInt32 -> (
            match (variable.var_type, variable.var_value) with
            | TPointer (TPointer _), _ -> fail not_implemented
            | TPointer p_type, Some _ ->
                get_i variable (Int32.to_int x) ctx >>= fun var_value ->
                return (var_value, p_type)
            | TPointer _, None ->
                fail
                  (VarWithoutValue
                     ("You are trying to get an element of the " ^ arr_name
                    ^ " array, but it is not initialized"))
            | _ ->
                fail
                  (WrongVariableType
                     "You are trying to gen an element of the non pointer \
                      array."))
        | _ ->
            fail
              (TypeMismatch
                 ("The index of the " ^ arr_name
                ^ "element is not int or int32_t type")))
    | FuncCall (func_name, f_args) when not (String.equal func_name "malloc")
      -> (
        match
          Stdlib.List.find_opt
            (fun func -> String.equal func.function_name func_name)
            ctx.func_list
        with
        | Some func -> (
            let new_context =
              let rec add_args args args_exprs new_ctx =
                match (args, args_exprs) with
                | (arg_type, arg_name) :: others, arg_expr :: others_values ->
                    interpret_expr
                      (DefineSeq
                         [ Define (arg_type, Variable arg_name, Some arg_expr) ])
                      ctx
                    >>= fun newest_ctx ->
                    add_args others others_values newest_ctx
                | [], [] -> return new_ctx
                | _ ->
                    fail
                      (InvalidFuncCall
                         (func.function_name
                        ^ " call doesn't match its signature."))
              in
              let new_ctx =
                {
                  ctx with
                  var_map = VarMap.empty;
                  func_name = func.function_name;
                  return_type = func.function_type;
                }
              in
              add_args func.function_arguments f_args new_ctx
            in
            match new_context with
            | Result.Ok new_context ->
                func_interpreter func new_context >>= fun new_val ->
                return (new_val, func.function_type)
            | Result.Error err -> Result.fail err)
        | None -> fail (UnknownVariable "Trying to call undefined function."))
    | FuncCall ("malloc", _) ->
        fail (InvalidFuncCall "Invalid malloc func call")
    | Cast (new_type, Variable var_name) -> (
        take_pointer var_name ctx.var_map >>= fun variable ->
        match variable with
        | { var_type = TPointer _; var_value = _; var_addr = _ } ->
            cast_pointer new_type variable >>= fun new_ptr ->
            return (IVInt (Int32.of_int new_ptr.var_addr), new_ptr.var_type)
        | { var_type = var_tp; var_value = _; var_addr = _ } ->
            match_var_value variable var_name >>= fun matched_val ->
            return (matched_val, var_tp))
    | Cast (new_type, expr) ->
        all_ops expr ctx >>= fun (some_val, _) ->
        cast_val some_val new_type >>= fun casted_val ->
        return (casted_val, new_type)
    | Pointer p_expr -> (
        all_ops p_expr ctx >>= function
        | _, TPointer (TPointer _) -> fail not_implemented
        | IVInt addr, TPointer p_type ->
            dereference (TPointer p_type) (Int32.to_int addr) ctx
            >>= fun p_val -> return (p_val, p_type)
        | _ -> fail (TypeMismatch "Trying to dereference not pointer value"))
    | _ -> fail not_implemented

  and interpret_expr expr ctx =
    let open Result in
    let rec add_var def_seq ctx =
      match def_seq with
      | Define
          (TPointer x, Variable var_name, Some (Value (Ast.VString var_str)))
        :: others -> (
          match
            const_to_value
              (Ast.VString var_str, TPointer x, "Variable " ^ var_name)
          with
          | Result.Ok (IVString _) -> (
              let new_ctx = define_pointer (TPointer x) var_name ctx in
              match assign_string var_name new_ctx var_str with
              | Result.Ok nctx -> add_var others nctx
              | Result.Error err -> Result.fail err)
          | Result.Ok _ ->
              Result.fail
                (TypeMismatch
                   "You are trying to initialize a char pointer with not a \
                    string value.")
          | Result.Error err -> Result.fail err)
      | Define
          (TPointer x, Variable var_name, Some (FuncCall ("malloc", f_args)))
        :: others -> (
          match f_args with
          | expr :: [] -> (
              all_ops expr ctx >>= fun (p_size, size_type) ->
              match (p_size, size_type) with
              | IVInt p_size, TInt32 ->
                  let new_ctx = define_pointer (TPointer x) var_name ctx in
                  malloc (TPointer x) var_name (Int32.to_int p_size) new_ctx
                  >>= fun newest_ctx -> add_var others newest_ctx
              | _ ->
                  Result.fail
                    (InvalidFuncCall "Mallog size arg is not int32 type"))
          | [] ->
              Result.fail (InvalidFuncCall "Trying to call malloc without args")
          | _ ->
              Result.fail
                (InvalidFuncCall
                   "malloc function call doesn't match its signature."))
      | Define
          ( TPointer x,
            Variable var_name,
            Some (Cast (new_type, Variable right_name)) )
        :: others -> (
          let new_ctx = define_pointer (TPointer x) var_name ctx in
          take_pointer right_name new_ctx.var_map >>= fun right_var ->
          match right_var.var_type with
          | TPointer _ ->
              cast_pointer new_type right_var >>= fun new_ptr ->
              assign_p2p var_name new_ptr new_ctx >>= fun newest_ctx ->
              add_var others newest_ctx
          | _ ->
              fail
                (TypeMismatch "Trying to assign non pointer value to pointer"))
      | Define (var_type, Variable var_name, Some (Value var_value)) :: others
        -> (
          match
            const_to_value (var_value, var_type, "Variable " ^ var_name)
          with
          | Result.Ok some_val ->
              add_var others
                {
                  ctx with
                  var_map =
                    VarMap.add var_name
                      {
                        var_type;
                        var_value = Option.some some_val;
                        var_addr = -1;
                      }
                      ctx.var_map;
                }
          | Result.Error err -> Result.fail err)
      | Define (var_type, Variable var_name, Some expr) :: others ->
          all_ops expr ctx >>= fun (var_val, var_t) ->
          if check_types_equality var_type var_t then
            add_var others
              {
                ctx with
                var_map =
                  VarMap.add var_name
                    { var_type; var_value = Option.some var_val; var_addr = -1 }
                    ctx.var_map;
              }
          else
            Result.fail
              (TypeMismatch
                 "Type of the variable is not equal to its assigned value. \
                  FIXME")
      | Define (var_type, Variable var_name, None) :: others ->
          add_var others
            {
              ctx with
              var_map =
                VarMap.add var_name
                  { var_type; var_value = None; var_addr = -1 }
                  ctx.var_map;
            }
      | [] -> return ctx
      | _ -> Result.fail Unreachable
    in
    let assign_pointer left_name right_expr ctx =
      take_pointer left_name ctx.var_map >>= function
      | { var_type = TPointer _; var_value = old_value; _ } -> (
          match right_expr with
          | Variable right_name ->
              take_var right_name ctx.var_map >>= fun right_var ->
              assign_p2p left_name right_var ctx
          | Cast (new_type, Variable right_name) -> (
              take_pointer right_name ctx.var_map >>= fun right_var ->
              match right_var.var_type with
              | TPointer _ ->
                  cast_pointer new_type right_var >>= fun new_ptr ->
                  assign_p2p left_name new_ptr ctx
              | _ ->
                  fail
                    (TypeMismatch
                       "Trying to assign non pointer value to pointer"))
          | _ -> (
              all_ops right_expr ctx >>= function
              | IVInt new_addr, TPointer new_type ->
                  assign_p2p left_name
                    {
                      var_type = new_type;
                      var_value = old_value;
                      var_addr = Int32.to_int new_addr;
                    }
                    ctx
              | _ ->
                  Result.fail
                    (TypeMismatch
                       "You are trying to assign non pointer value to a \
                        pointer variable")))
      | _ -> Result.fail Unreachable
    in
    let assign_interpret var_name assign_expr ctx =
      take_pointer var_name ctx.var_map >>= fun variable ->
      match variable.var_type with
      | TPointer (TPointer _) -> Result.fail not_implemented
      | TPointer _ -> (
          match assign_expr with
          | FuncCall ("malloc", f_args) -> (
              match f_args with
              | expr :: [] -> (
                  all_ops expr ctx >>= fun (p_size, size_type) ->
                  match (p_size, size_type) with
                  | IVInt p_size, TInt32 ->
                      malloc variable.var_type var_name (Int32.to_int p_size)
                        ctx
                  | _ ->
                      Result.fail
                        (InvalidFuncCall "Mallog size arg is not int32 type"))
              | [] ->
                  Result.fail
                    (InvalidFuncCall "Trying to call malloc without args")
              | _ ->
                  Result.fail
                    (InvalidFuncCall
                       "malloc function call doesn't match its signature."))
          | _ -> assign_pointer var_name assign_expr ctx)
      | _ ->
          all_ops assign_expr ctx >>= fun (new_val, new_type) ->
          if check_types_equality variable.var_type new_type then
            update_var var_name (Option.some new_val) variable.var_addr
              ctx.var_map
            >>= fun new_var_map ->
            Result.return { ctx with var_map = new_var_map }
          else
            Result.fail
              (WrongVariableType
                 ("Variable " ^ var_name
                ^ " type and assigned value type are mismatched."))
    in
    let assign_deref p_expr assign_expr ctx =
      all_ops p_expr ctx >>= fun (p_val, p_type) ->
      match (p_val, p_type) with
      | IVInt addr, TPointer old_type -> (
          all_ops assign_expr ctx >>= fun (new_val, new_type) ->
          match new_type with
          | TPointer _ ->
              Result.Error
                (TypeMismatch
                   "You are trying to assign pointer value to dereferenced \
                    pointer value")
          | _ ->
              assign_dereference (TPointer old_type) (Int32.to_int addr) new_val
                ctx)
      | _ ->
          Result.fail
            (TypeMismatch "You are trying to dereference not pointer value")
    in
    let assign_arr_elem arr_name index_expr assign_expr ctx =
      take_pointer arr_name ctx.var_map >>= fun variable ->
      all_ops index_expr ctx >>= function
      | IVInt x, TInt32 -> (
          all_ops assign_expr ctx >>= fun (new_elem, new_type) ->
          match (variable.var_type, variable.var_value) with
          | TPointer (TPointer _), _ -> fail not_implemented
          | TPointer _, Some _ ->
              assign_array_elem arr_name variable (Int32.to_int x) new_elem
                new_type ctx
              >>= fun new_ctx -> return new_ctx
          | TPointer _, None ->
              fail
                (VarWithoutValue
                   ("You are trying to get an element of the " ^ arr_name
                  ^ " array, but it is not initialized"))
          | _ -> fail not_implemented)
      | _ ->
          fail
            (TypeMismatch
               ("The index of the " ^ arr_name
              ^ "element is not int or int32_t type"))
    in
    match expr with
    | DefineSeq def_seq -> (
        match add_var def_seq ctx with
        | Result.Ok new_ctx -> Result.return new_ctx
        | Result.Error err -> Result.fail err)
    | Assign (Variable var_name, assign_expr) ->
        assign_interpret var_name assign_expr ctx
    | Assign (Pointer x, assign_expr) ->
        assign_deref (Pointer x) assign_expr ctx
    | Assign (ArrayElem (arr_name, index_expr), assign_expr) ->
        assign_arr_elem arr_name index_expr assign_expr ctx
    | _ -> Result.fail not_implemented

  and interpret_st_block st_block ctx loop_flag =
    let open Result in
    let return_val others = function
      | None, new_context ->
          interpret_st_block others (return new_context) loop_flag
      | Some x, new_context -> return (Option.Some x, new_context, false)
    in
    match (st_block, ctx, loop_flag) with
    | Expression expr :: others, Result.Ok _, _ -> (
        let new_context = ctx >>= fun x -> interpret_expr expr x in
        match new_context with
        | Result.Ok _ -> interpret_st_block others new_context loop_flag
        | Result.Error err -> Result.fail err)
    | ( IfSeq (if_seq, Some (StatementsBlock else_opt)) :: others,
        Result.Ok unpacked_ctx,
        _ ) ->
        interpret_if_seq if_seq (Option.some else_opt) unpacked_ctx loop_flag
        >>= return_val others
    | ( While (condition, StatementsBlock st_block) :: others,
        Result.Ok unpacked_ctx,
        _ ) ->
        interpret_while condition st_block unpacked_ctx >>= return_val others
    | ( For (init_expr, condition, loop_expr, StatementsBlock st_block) :: others,
        Result.Ok unpacked_ctx,
        _ ) ->
        interpret_for init_expr condition loop_expr st_block unpacked_ctx
        >>= return_val others
    | Return my_expr :: _, Result.Ok ok_context, _ ->
        all_ops my_expr ok_context >>= fun (return_val, val_type) ->
        if check_types_equality ok_context.return_type val_type then
          Result.return (Option.some return_val, ok_context, false)
        else
          Result.fail
            (ReturnTypeMismatch
               ("Type of the " ^ ok_context.func_name
              ^ "function is not equal to its return value type"))
    | [], Result.Ok ok_context, _ ->
        Result.return (Option.None, ok_context, false)
    | _, Result.Error err, _ -> Result.fail err
    | Break :: _, Result.Ok ok_context, true ->
        Result.return (Option.None, ok_context, true)
    | Continue :: _, Result.Ok ok_context, true ->
        Result.return (Option.None, ok_context, false)
    | Break :: _, Result.Ok _, false ->
        Result.fail (StatementOutsideOfLoop "Break command is outside of loop")
    | Continue :: _, Result.Ok _, false ->
        Result.fail
          (StatementOutsideOfLoop "Continue command is outside of loop")
    | _, Result.Ok _, _ -> Result.fail not_implemented

  and resolve_condition condition ctx =
    let open Result in
    all_ops condition ctx >>= function
    | IVInt result, TInt32 ->
        if Int32.to_int result <> 0 then return true else return false
    | _ -> fail (BadCondition "Bad condition")

  and resolve_loop_condition condition ctx =
    let open Result in
    all_ops condition ctx >>= function
    | IVInt result, TInt32 ->
        if Int32.to_int result <> 0 then return (true, ctx)
        else return (false, ctx)
    | _ -> fail (BadCondition "Bad condition")

  and interpret_if_seq ifs_seq else_opt ctx loop_flag =
    let open Result in
    let rec find_true_if ifs_seq else_opt =
      match ifs_seq with
      | If (condition, StatementsBlock x) :: [] -> (
          match resolve_condition condition ctx with
          | Result.Ok false -> return else_opt
          | Result.Ok true -> return (Option.some x)
          | Result.Error err -> Result.fail err)
      | If (condition, StatementsBlock x) :: ifs -> (
          match resolve_condition condition ctx with
          | Result.Ok false -> find_true_if ifs else_opt
          | Result.Ok true -> return (Option.some x)
          | Result.Error err -> Result.fail err)
      | _ -> fail Unreachable
    in
    match find_true_if ifs_seq else_opt with
    | Result.Ok None -> return (None, ctx)
    | Result.Error err -> fail err
    | Result.Ok (Some st_block) -> (
        interpret_st_block st_block (return ctx) loop_flag
        >>= fun (ret_val, new_ctx, _) ->
        match (ret_val, new_ctx) with
        | Some ret_val, new_ctx -> return (Option.some ret_val, new_ctx)
        | None, new_ctx -> return (None, new_ctx))

  and interpret_while condition st_block ctx =
    let open Result in
    resolve_condition condition ctx >>= function
    | false -> return (None, ctx)
    | true -> (
        interpret_st_block st_block (return ctx) true
        >>= fun (ret_val, new_ctx, _) ->
        match (ret_val, new_ctx) with
        | Some r_val, _ -> return (Option.some r_val, new_ctx)
        | None, _ -> interpret_while condition st_block new_ctx)

  and interpret_for first_expr condition loop_expr st_block ctx =
    let open Result in
    (match (first_expr, condition) with
    | Some first_expr, Some condition ->
        interpret_expr first_expr ctx >>= fun new_ctx ->
        resolve_loop_condition condition new_ctx
    | None, Some condition -> resolve_loop_condition condition ctx
    | Some first_expr, None ->
        interpret_expr first_expr ctx >>= fun new_ctx -> return (true, new_ctx)
    | None, None -> return (true, ctx))
    >>= function
    | false, ctx -> return (None, ctx)
    | true, ctx -> (
        interpret_st_block st_block (return ctx) true
        >>= fun (ret_val, ctx_after_loop, _) ->
        match (ret_val, ctx_after_loop) with
        | Some r_val, _ -> return (Option.some r_val, ctx_after_loop)
        | None, _ -> (
            match loop_expr with
            | Some loop_expr ->
                interpret_expr loop_expr ctx_after_loop >>= fun final_ctx ->
                interpret_for None condition (Option.some loop_expr) st_block
                  final_ctx
            | None -> interpret_for None condition None st_block ctx_after_loop)
        )

  and func_interpreter func ctx =
    let open Result in
    match func.function_body with
    | Some (StatementsBlock st_block) -> (
        interpret_st_block st_block (Result.return ctx) false >>= function
        | Some ret_val, _, _ -> return ret_val
        | None, _, _ ->
            if check_types_equality func.function_type TVoid then
              Result.return (IVString "Void val")
            else
              Result.fail
                (ReturnTypeMismatch
                   (func.function_name
                  ^ "expected a return value, but nothing was returned")))
    | None ->
        if check_types_equality func.function_type TVoid then
          Result.return (IVString "Void val")
        else
          Result.fail
            (ReturnTypeMismatch
               (func.function_name
              ^ "expected a return value, but nothing was returned"))
    | _ -> Result.fail Unreachable

  let run func_list =
    match
      Stdlib.List.find_opt
        (fun func -> String.equal func.function_name "main")
        func_list
    with
    | Some func ->
        func_interpreter func
          { (main_context func.function_type) with func_list }
    | None -> Result.fail not_implemented
end

let parse_and_run str =
  let ans =
    match Parser.parse str with
    | Caml.Result.Ok ast -> Interpret.run ast
    | Caml.Result.Error _ ->
        Caml.Format.eprintf "Parsing error\n%!";
        Caml.exit 1
  in
  ans

let value_to_str = function
  | IVInt x -> Int32.to_string x
  | IVInt16 x -> Int16.to_string x
  | IVInt8 x -> Int8.to_string x
  | IVChar x -> Char.to_string x
  | IVString x -> x

let print_err = function
  | Base.Result.Error (UnknownVariable my_str)
  | Base.Result.Error (ParsingError my_str)
  | Base.Result.Error (WrongVariableType my_str)
  | Base.Result.Error (EmptyStatementBlock my_str)
  | Base.Result.Error (ExpressionExpected my_str)
  | Base.Result.Error (ReturnTypeMismatch my_str)
  | Base.Result.Error (TypeMismatch my_str)
  | Base.Result.Error (DivisionByZero my_str)
  | Base.Result.Error (VarWithoutValue my_str)
  | Base.Result.Error (MemoryEnd my_str)
  | Base.Result.Error (IndexOutOfRange my_str)
  | Base.Result.Error (BadCondition my_str)
  | Base.Result.Error (StatementOutsideOfLoop my_str)
  | Base.Result.Error (InvalidFuncCall my_str) ->
      print_endline my_str
  | Base.Result.Error Unreachable ->
      print_endline "It doesn't meant to be like that..."
  | Base.Result.Ok _ -> print_endline "Another result"
