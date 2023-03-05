(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

(** The error to be thrown when execution fails (wrong input). *)
exception Error of string

(** The error to be thrown when [Interpreter] behaves not as expected. *)
exception Implementation_error of string

(** Throw [Error]. *)
let fail msg = raise (Error msg)

(** Throw [Implementation.Error]. *)
let crash msg = raise (Implementation_error msg)

(** Data structures and functions used to preserve program state. *)
module Environment = struct
  module Identifier = struct
    type t = identifier

    let compare id1 id2 =
      match id1 with
      | Name nm1 ->
        (match id2 with
         | Name nm2 -> compare nm1 nm2)
    ;;
  end

  module IdentifierMap = Map.Make (Identifier)

  type env =
    { vars : literal IdentifierMap.t
    ; funcs : (args * chunk) IdentifierMap.t
    }

  (** Default empty [env]. *)
  let empty = { vars = IdentifierMap.empty; funcs = IdentifierMap.empty }

  let string_of_identifier = function
    | Name name -> name
  ;;

  let string_of_func id args =
    let args = String.concat ", " (List.map string_of_identifier args) in
    Util.concat [ string_of_identifier id; " ("; args; ")" ]
  ;;

  type id_type =
    | Variable
    | Function

  (** Check if [id] is [Variable] or [Function]. *)
  let id_type id env =
    let is_var = IdentifierMap.mem id env.vars in
    let is_func = IdentifierMap.mem id env.funcs in
    if is_var && is_func
    then crash (Util.concat [ ": ambigous identifier `"; string_of_identifier id; "`" ])
    else if is_func
    then Function
    else Variable
  ;;

  (** Find [id] among variables. *)
  let find_var id env =
    try IdentifierMap.find id env.vars with
    | Not_found -> Nil
  ;;

  (** Find [id] among functions. *)
  let find_func id env =
    try IdentifierMap.find id env.funcs with
    | Not_found ->
      fail (Util.concat [ ": function `"; string_of_identifier id; "` not declared" ])
  ;;
end

(** Built-in functions implementations, such as [print()]. *)
module rec Builtins : sig
  (** Check if there's a built-in function with this [id]. *)
  val is_builtin : Ast.identifier -> bool

  (** Execute a built-in function with this [id]. *)
  val call_builtin
    :  Ast.identifier
    -> Ast.expression list
    -> Environment.env
    -> Ast.expression * Environment.env
end = struct
  open Environment

  let show_var id value =
    let name = string_of_identifier id in
    match value with
    | Numeric num -> print_string (Util.concat [ name; " = "; string_of_float num; "\n" ])
    | String str -> print_string (Util.concat [ name; " = \""; str; "\"\n" ])
    | Bool b -> print_string (Util.concat [ name; " = "; string_of_bool b; "\n" ])
    | Nil -> print_string (name ^ " = nil\n")
  ;;

  let show_func id (args, chunk) =
    let args, _ = args, chunk in
    print_endline (string_of_func id args)
  ;;

  (** Print all variables, then all functions. *)
  let show_env env =
    print_string "-----\n";
    IdentifierMap.iter show_var env.vars;
    IdentifierMap.iter show_func env.funcs;
    print_string "-----\n";
    Literal Nil, env
  ;;

  (** Read file contents and execute as a chunk. *)
  let dofile params env =
    match params with
    | Literal (String filepath) :: [] ->
      if Sys.file_exists filepath
      then (
        let program = Util.read_file filepath in
        Chunk.execute (Parser.parse program) env)
      else fail (Util.concat [ ": file `"; filepath; "` doesn't exist" ])
    | _ -> fail ": dofile(filepath) takes exactly one string argument"
  ;;

  (** Print string representations of any number of args. *)
  let rec print args env =
    let print_one expr env =
      match expr with
      | Literal Nil -> print_string "nil"
      | Literal (Bool lit) -> print_string (string_of_bool lit)
      | Literal (Numeric lit) -> print_float lit
      | Literal (String lit) -> print_string lit
      | Identifier func ->
        let args, _ = find_func func env in
        print_string (string_of_func func args)
      | _ -> crash ": the expr should've folded to Literal or function Identifier"
    in
    match args with
    | [] ->
      print_newline ();
      Literal Nil, env
    | head :: tail ->
      let head, env = Expression.execute head env in
      print_one head env;
      if tail != [] then print_char ' ';
      print tail env
  ;;

  (** Lua's [not] operator implemented as a function. *)
  let notf params env =
    match params with
    | expr :: [] ->
      let expr, env = Expression.execute expr env in
      Literal (Bool (not (Expression.bool_of_expression expr env))), env
    | _ -> fail ": not(expr) takes exactly one argument"
  ;;

  let is_builtin id =
    match string_of_identifier id with
    | "__show_env" | "dofile" | "print" | "not" -> true
    | _ -> false
  ;;

  let call_builtin id params env =
    match string_of_identifier id with
    | "__show_env" -> show_env env
    | "dofile" -> dofile params env
    | "print" -> print params env
    | "not" -> notf params env
    | _ ->
      crash
        (Util.concat [ ": `"; string_of_identifier id; "` is not a builtin function" ])
  ;;
end

(** Interpret function definitions and calls. *)
and Function : sig
  (** Execute [Call] statement. *)
  val call : identifier -> params -> Environment.env -> expression * Environment.env
end = struct
  open Environment

  (** Assign values to function args. *)
  let introduce_params args params =
    let introduce_one arg param = Assignment (arg, param) in
    try List.map2 introduce_one args params with
    | Invalid_argument _ ->
      fail
        (Util.concat
           [ ": expected "
           ; string_of_int (List.length args)
           ; " parameters, "
           ; string_of_int (List.length params)
           ; " given"
           ])
  ;;

  let call id params env =
    if Builtins.is_builtin id
    then Builtins.call_builtin id params env
    else (
      let args, Chunk body = find_func id env in
      let chunk = Chunk (introduce_params args params @ body) in
      (* Function call can modify env if it modifies or declares new vars or funcs.
         Since we don't implement Lua's `local` keyword, all these are global *)
      Chunk.execute chunk env)
  ;;
end

(** Interpret [Expression]. *)
and Expression : sig
  val bool_of_expression : expression -> Environment.env -> bool

  (** Execute any [Expression]. *)
  val execute : expression -> Environment.env -> expression * Environment.env
end = struct
  open Environment

  let bool_of_expression expr env =
    match expr with
    | Identifier id -> IdentifierMap.mem id env.funcs
    | Literal Nil -> false
    | Literal (Bool lit) -> lit
    | Literal _ -> true
    | _ -> crash ": the condition should've folded to Literal or function Identifier"
  ;;

  (** Execute [Binop] expression. *)
  let binop left op right env =
    match op with
    | AddOp | SubOp | MulOp | DivOp ->
      (match left, right with
       | Literal (Numeric left), Literal (Numeric right) ->
         (match op with
          | AddOp -> Literal (Numeric (left +. right))
          | SubOp -> Literal (Numeric (left -. right))
          | MulOp -> Literal (Numeric (left *. right))
          | DivOp -> Literal (Numeric (left /. right))
          | _ -> crash ": `op` should've been arithmetic here")
       | _ -> fail ": only operations on numbers are allowed")
    | EqualsOp -> Literal (Bool (left = right))
    | AndOp | OrOp ->
      let left = bool_of_expression left env in
      let right = bool_of_expression right env in
      (match op with
       | AndOp -> Literal (Bool (left && right))
       | OrOp -> Literal (Bool (left || right))
       | _ -> crash ": `op` should've been `and` or `or` here")
  ;;

  let rec execute expression env =
    match expression with
    | Literal lit -> Literal lit, env
    | Identifier id ->
      (match id_type id env with
       | Variable -> Literal (find_var id env), env
       | Function -> Identifier id, env)
    | Binop (left, op, right) ->
      let left, env = execute left env in
      let right, env = execute right env in
      binop left op right env, env
    | Call (id, params) -> Function.call id params env
  ;;
end

(** Interpret [Statement]. *)
and Statement : sig
  (** Execute [Statement]. *)
  val execute : Ast.statement -> Environment.env -> expression * Environment.env
end = struct
  open Environment

  (** Execute [Assignment]. *)
  let assign id expr env =
    (* var  = func -> remove; add
       func = func -> none;   add
       var  = var  -> add;    none
       func = var  -> add;    remove
       *)
    match Expression.execute expr env with
    | Literal lit, env ->
      (match id_type id env with
       | Variable -> { env with vars = IdentifierMap.add id lit env.vars }
       | Function ->
         { vars = IdentifierMap.add id lit env.vars
         ; funcs = IdentifierMap.remove id env.funcs
         })
    (* TODO: fix implicit dependence
       Expression.execute guarantees that if expr is Identifier, it is a Function *)
    | Identifier func, env ->
      let definition = find_func func env in
      (match id_type id env with
       | Variable ->
         { vars = IdentifierMap.remove id env.vars
         ; funcs = IdentifierMap.add id definition env.funcs
         }
       | Function -> { env with funcs = IdentifierMap.add id definition env.funcs })
    | _ ->
      crash
        ": the right-hand expression should've folded to Literal or function Identifier"
  ;;

  (* TODO: fix implicit dependence
     all but Return produce Literal Nil *)
  (** Execute any [Statement]. *)
  let execute statement env =
    match statement with
    | Comment -> Literal Nil, env
    | Expression expr ->
      (* i.e. call the function that has side-effects *)
      let _, env = Expression.execute expr env in
      Literal Nil, env
    | Assignment (id, expr) ->
      if Builtins.is_builtin id
      then
        fail (Util.concat [ ": `"; string_of_identifier id; "` is a builtin function" ])
      else Literal Nil, assign id expr env
    | Branch (condition, thenpart, elsepart) ->
      let condition, env = Expression.execute condition env in
      let condition = Expression.bool_of_expression condition env in
      if condition then Chunk.execute thenpart env else Chunk.execute elsepart env
    | Definition (id, args, body) ->
      if Builtins.is_builtin id
      then
        fail (Util.concat [ ": `"; string_of_identifier id; "` is a builtin function" ])
      else (
        let env =
          { vars = IdentifierMap.remove id env.vars
          ; funcs = IdentifierMap.add id (args, body) env.funcs
          }
        in
        Literal Nil, env)
    | Return expr -> Expression.execute expr env
  ;;
end

(** Interpret [Chunk]. *)
and Chunk : sig
  (** Execute [Chunk]. *)
  val execute : Ast.chunk -> Environment.env -> expression * Environment.env
end = struct
  let rec execute chunk env =
    match chunk with
    | Chunk [] -> Literal Nil, env
    | Chunk (head :: tail) ->
      let result, env = Statement.execute head env in
      if result != Literal Nil
      then result, env
      else (
        match head with
        | Return _ -> result, env
        | _ -> execute (Chunk tail) env)
  ;;
end

(* --------------------------- *)
(* ---------- TESTS ---------- *)
(* --------------------------- *)

let ast =
  let var = Name "var" in
  let zero = Numeric 0. in
  Chunk
    [ Assignment (var, Literal zero)
    ; Assignment (var, Literal (Numeric 10.))
    ; Assignment (var, Binop (Literal (Numeric 3.), SubOp, Literal (Numeric 2.)))
    ; Assignment (Name "arg1", Literal zero)
    ; Assignment (Name "arg2", Literal (Numeric 1.))
    ; Assignment (Name "arg2", Binop (Literal (Numeric 2.), AddOp, Literal (Numeric 3.)))
    ; Expression
        (Call
           ( Name "print"
           , [ Identifier (Name "arg2"); Literal (Numeric 19.); Literal zero ] ))
    ; Expression
        (Call
           ( Name "print"
           , [ Identifier var
             ; Binop
                 ( Binop (Identifier (Name "var"), MulOp, Literal (Numeric 5.))
                 , MulOp
                 , Literal (Numeric 3.) )
             ] ))
    ; Expression (Call (Name "print", []))
    ]
;;
