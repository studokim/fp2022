(** Copyright 2021-2022, Mikhail Vyrodov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

let pp_const fmt = function
  | VInt x -> fprintf fmt "%s" (Int32.to_string x)
  | VChar x -> fprintf fmt "%c" x
  | VString x -> fprintf fmt "\"%s\"" x

let rec pp_ctype fmt = function
  | TVoid -> fprintf fmt "%s" "void"
  | TChar -> fprintf fmt "%s" "char"
  | TInt8 -> fprintf fmt "%s" "int8_t"
  | TInt16 -> fprintf fmt "%s" "int16_t"
  | TInt32 -> fprintf fmt "%s" "int"
  | TPointer x -> fprintf fmt "%a *" pp_ctype x

let get_op_sign = function
  | Add _ -> "+"
  | Sub _ -> "-"
  | Mul _ -> "*"
  | Div _ -> "/"
  | Mod _ -> "%"
  | Less _ -> "<"
  | LessOrEq _ -> "<="
  | More _ -> ">"
  | MoreOrEq _ -> ">="
  | Equal _ -> "=="
  | NotEqual _ -> "!="
  | Or _ -> "||"
  | And _ -> "&&"
  | Pointer _ -> "*"
  | Address _ -> "&"
  | UnaryMin _ -> "-"
  | UnaryPlus _ -> "+"
  | Inc _ -> "++"
  | Dec _ -> "--"
  | _ -> failwith "unreachable"

let print_args fmt args =
  pp_print_list
    ~pp_sep:(fun fmt _ -> fprintf fmt ", ")
    (fun fmt value -> pp_statement fmt value)
    fmt args

let print_funcs fmt args =
  pp_print_list
    ~pp_sep:(fun fmt _ -> fprintf fmt "")
    (fun fmt value -> pp_statement fmt value)
    fmt args

let rec pp_expression fmt =
  let print_bin_op fmt ast_op left right =
    fprintf fmt "(%a) %s (%a)" pp_expression left (get_op_sign ast_op)
      pp_expression right
  in

  let print_un_op fmt ast_op ast_val =
    match ast_op with
    | Inc _ | Dec _ ->
        fprintf fmt "(%a)%s" pp_expression ast_val (get_op_sign ast_op)
    | _ -> fprintf fmt "%s(%a)" (get_op_sign ast_op) pp_expression ast_val
  in
  let print_args fmt args =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt ", ")
      (fun fmt value -> pp_expression fmt value)
      fmt args
  in
  function
  | Define (dtype, Variable dvar_name, dexpr) -> (
      match dexpr with
      | None -> fprintf fmt "%a %s" pp_ctype dtype dvar_name
      | Some x ->
          fprintf fmt "%a %s = %a" pp_ctype dtype dvar_name pp_expression x)
  | DefineSeq def_seq ->
      pp_print_list
        ~pp_sep:(fun fmt _ -> fprintf fmt ";\n")
        (fun fmt value -> pp_expression fmt value)
        fmt def_seq
  | Assign (x, y) -> fprintf fmt "%a = %a" pp_expression x pp_expression y
  | Cast (etype, expr) ->
      fprintf fmt "(%a)(%a)" pp_ctype etype pp_expression expr
  | FuncCall (fname, fargs) -> fprintf fmt "%s(%a)" fname print_args fargs
  | ArrayElem (aname, aexpr) -> fprintf fmt "%s[%a]" aname pp_expression aexpr
  | Array (atype, aname, alen, adef) -> (
      match (alen, adef) with
      | Some alen, Some adef ->
          fprintf fmt "%a %s[%a] = {%a}" pp_ctype atype aname pp_expression alen
            print_args adef
      | Some alen, None ->
          fprintf fmt "%a %s[%a]" pp_ctype atype aname pp_expression alen
      | None, Some adef ->
          fprintf fmt "%a %s[] = {%a}" pp_ctype atype aname print_args adef
      | None, None -> fprintf fmt "%a %s[]" pp_ctype atype aname)
  | Add (x, y) -> print_bin_op fmt (Add (x, y)) x y
  | Sub (x, y) -> print_bin_op fmt (Sub (x, y)) x y
  | Mul (x, y) -> print_bin_op fmt (Mul (x, y)) x y
  | Div (x, y) -> print_bin_op fmt (Div (x, y)) x y
  | Mod (x, y) -> print_bin_op fmt (Mod (x, y)) x y
  | And (x, y) -> print_bin_op fmt (And (x, y)) x y
  | Or (x, y) -> print_bin_op fmt (Or (x, y)) x y
  | More (x, y) -> print_bin_op fmt (More (x, y)) x y
  | MoreOrEq (x, y) -> print_bin_op fmt (MoreOrEq (x, y)) x y
  | Less (x, y) -> print_bin_op fmt (Less (x, y)) x y
  | LessOrEq (x, y) -> print_bin_op fmt (LessOrEq (x, y)) x y
  | Equal (x, y) -> print_bin_op fmt (Equal (x, y)) x y
  | NotEqual (x, y) -> print_bin_op fmt (NotEqual (x, y)) x y
  | UnaryMin x -> print_un_op fmt (UnaryMin x) x
  | UnaryPlus x -> print_un_op fmt (UnaryPlus x) x
  | Inc x -> print_un_op fmt (Inc x) x
  | Dec x -> print_un_op fmt (Dec x) x
  | Pointer x -> print_un_op fmt (Pointer x) x
  | Address x -> print_un_op fmt (Address x) x
  | Variable x -> fprintf fmt "%s" x
  | Value x -> pp_const fmt x
  | _ -> failwith "Unreachable"

let rec pp_statement fmt =
  let print_args fmt args =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt "")
      (fun fmt value -> pp_statement fmt value)
      fmt args
  in
  function
  | Expression expr -> fprintf fmt "%a;\n" pp_expression expr
  | StatementsBlock stblock -> fprintf fmt "{\n%a}\n" print_args stblock
  | IfSeq (if_list, else_opt) -> (
      match else_opt with
      | Some else_opt ->
          fprintf fmt "%aelse %a" print_args if_list pp_statement else_opt
      | None -> fprintf fmt "%a" print_args if_list)
  | If (condition, stblock) ->
      fprintf fmt "if (%a) %a" pp_expression condition pp_statement stblock
  | While (condition, stblock) ->
      fprintf fmt "while (%a) %a" pp_expression condition pp_statement stblock
  | For (first_expr, condition, loop_expr, stblock) -> (
      match (first_expr, condition, loop_expr) with
      | Some first_expr, Some condition, Some loop_expr ->
          fprintf fmt "for (%a; %a; %a) %a" pp_expression first_expr
            pp_expression condition pp_expression loop_expr pp_statement stblock
      | Some first_expr, Some condition, None ->
          fprintf fmt "for (%a; %a;) %a" pp_expression first_expr pp_expression
            condition pp_statement stblock
      | Some first_expr, None, Some loop_expr ->
          fprintf fmt "for (%a; ;%a) %a" pp_expression first_expr pp_expression
            loop_expr pp_statement stblock
      | None, Some condition, Some loop_expr ->
          fprintf fmt "for (; %a; %a) %a" pp_expression condition pp_expression
            loop_expr pp_statement stblock
      | None, None, Some loop_expr ->
          fprintf fmt "for (; ; %a) %a" pp_expression loop_expr pp_statement
            stblock
      | None, Some condition, None ->
          fprintf fmt "for (; %a;) %a" pp_expression condition pp_statement
            stblock
      | Some first_expr, None, None ->
          fprintf fmt "for (%a; ;) %a" pp_expression first_expr pp_statement
            stblock
      | None, None, None -> fprintf fmt "for (; ;) %a" pp_statement stblock)
  | Break -> fprintf fmt "%s;" "break;\n"
  | Continue -> fprintf fmt "%s;" "continue;\n"
  | Return expr -> fprintf fmt "return %a;\n" pp_expression expr

let pp_function_list fmt func_list =
  let pp_function fmt func =
    let print_args fmt args =
      let print_arg fmt = function x, y -> fprintf fmt "%a %s" pp_ctype x y in
      pp_print_list
        ~pp_sep:(fun fmt _ -> fprintf fmt ", ")
        (fun fmt value -> print_arg fmt value)
        fmt args
    in
    match func.function_body with
    | Some x ->
        fprintf fmt "%a %s(%a) %a" pp_ctype func.function_type
          func.function_name print_args func.function_arguments pp_statement x
    | None ->
        fprintf fmt "%a %s(%a) { }" pp_ctype func.function_type
          func.function_name print_args func.function_arguments
  in
  let print_funcs fmt args =
    pp_print_list
      ~pp_sep:(fun fmt _ -> fprintf fmt "\n")
      (fun fmt value -> pp_function fmt value)
      fmt args
  in
  fprintf fmt "%a" print_funcs func_list

let%expect_test _ =
  printf "%a" pp_expression
  @@ FuncCall ("Insert", [ Variable "table"; Variable "coeff" ]);
  [%expect {|
  Insert(table, coeff)
  |}]

let%expect_test _ =
  printf "%a" pp_expression
  @@ Array
       ( TInt32,
         "coeff",
         Some (Value (VInt 5l)),
         Some [ Add (Value (VInt 1l), Value (VInt 3l)); Variable "var" ] );
  [%expect {|
  int coeff[5] = {(1) + (3), var}
  |}]

let%expect_test _ =
  printf "%a" pp_function_list
  @@ [
       {
         function_type = TVoid;
         function_name = "Helper";
         function_arguments = [ (TInt32, "value") ];
         function_body =
           Some
             (StatementsBlock
                [
                  Return
                    (Mul
                       ( Variable "value",
                         Add (Variable "value", Value (VInt 1l)) ));
                ]);
       };
       {
         function_type = TInt32;
         function_name = "main";
         function_arguments = [];
         function_body =
           Some
             (StatementsBlock
                [
                  Expression
                    (DefineSeq
                       [
                         Define (TInt32, Variable "n", None);
                         Define (TInt32, Variable "i", None);
                       ]);
                  Expression
                    (DefineSeq
                       [
                         Define (TInt32, Variable "fact", Some (Value (VInt 1l)));
                       ]);
                  Expression
                    (FuncCall
                       ("printf", [ Value (VString "Enter an integer: ") ]));
                  Expression
                    (FuncCall
                       ( "scanf",
                         [ Value (VString "%d"); Address (Variable "n") ] ));
                  IfSeq
                    ( [
                        If
                          ( Less (Variable "n", Value (VInt 0l)),
                            StatementsBlock
                              [
                                Expression
                                  (FuncCall
                                     ( "printf",
                                       [
                                         Value
                                           (VString
                                              "Error! Factorial of a negative \
                                               number doesn't exist.");
                                       ] ));
                              ] );
                      ],
                      Some
                        (StatementsBlock
                           [
                             For
                               ( Some (Assign (Variable "i", Value (VInt 1l))),
                                 Some (LessOrEq (Variable "i", Variable "n")),
                                 Some
                                   (Assign
                                      ( Variable "i",
                                        Add (Variable "i", Value (VInt 1l)) )),
                                 StatementsBlock
                                   [
                                     Expression
                                       (Assign
                                          ( Variable "fact",
                                            Mul (Variable "fact", Variable "i")
                                          ));
                                   ] );
                             Expression
                               (FuncCall
                                  ( "printf",
                                    [
                                      Value (VString "Factorial of %d = %llu");
                                      Variable "n";
                                      Variable "fact";
                                    ] ));
                           ]) );
                  Return (Value (VInt 0l));
                ]);
       };
     ];
  [%expect
    {| 
        void Helper(int value) {
        return (value) * ((value) + (1));
        }
    
        int main() {
        int n;
        int i;
        int fact = 1;
        printf("Enter an integer: ");
        scanf("%d", &(n));
        if ((n) < (0)) {
        printf("Error! Factorial of a negative number doesn't exist.");
        }
        else {
        for (i = 1; (i) <= (n); i = (i) + (1)) {
        fact = (fact) * (i);
        }
        printf("Factorial of %d = %llu", n, fact);
        }
        return 0;
        } |}]
