(** Copyright 2022-2023, Kseniia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type command =
  | SimpleCommand of string * expr list
  | Compound of compound
  | VarAssignment of arr
  | Expression of expr
[@@deriving show { with_path = false }]

(* Any variable may be used as an indexed array *)
and arr =
  | SimpleVar of var * expr
  | Array of string * expr list
  | AssociativeArray of string * (string * expr) list
[@@deriving show { with_path = false }]

and pipeline =
  | CommandList of command list (* cmd; cmd; cmd; *)
  | Pipe of command list (* command | command | command *)
  | AndList of pipeline list (* pipe1 && pipe2 *)
  | OrList of pipeline list (* pipe1 || pipe2 *)
[@@deriving show { with_path = false }]

and const =
  | Int of int
  | String of string
  | Bool of bool
[@@deriving show { with_path = false }]

and const_lst = const list [@@deriving show { with_path = false }]

and expr =
  | Const of const
  | Binop of binop
  | Var of var
  | Assignment of var * expr
  | SingleQuotes of string
  | DoubleQuotes of expr list
  | BraceExpansion of expr list
  | ParameterExpansion of parameter_expansion
  | ArithmeticExpansion of expr
  | CommandSubstitution of command
  | Redirect of redirect
[@@deriving show { with_path = false }]

and binop =
  | Mul of expr * expr (* Multiplication *)
  | Add of expr * expr (* Addition *)
  | Sub of expr * expr (* Subtraction *)
  | Div of expr * expr (* Division *)
  | Eq of expr * expr (* Equal *)
  | Ne of expr * expr (* Not equal *)
  | Gt of expr * expr (* Greater than *)
  | Ge of expr * expr (* Greater or equal *)
  | Lt of expr * expr (* Less than *)
  | Le of expr * expr (* Less or equal *)
[@@deriving show { with_path = false }]

and var = string * string [@@deriving show { with_path = false }]

and compound =
  | IfElse of
      expr * command list * command list option (* if {expression} {block} else {block} *)
  | While of expr * command list (* while {expr} {block} *)
  | For of expr * expr * expr * command list
    (* for ({expression}; {expression}; {expression}) do {block} ... *)
  | ForIn of
      expr * expr list * command list (* for {var} in {expressions} do {block} done *)
  | Case of expr * (expr list * command list) list
    (* case {expr} in {expr1}) {block1};; {expr2}) {block2}... *)
[@@deriving show { with_path = false }]

and parameter_expansion =
  (* https://learntutorials.net/ru/bash/topic/502/расширение-параметра-bash *)
  | Offset of var * expr (* ${parameter:offset} *)
  | OffsetLen of var * expr * expr (* ${parameter:offset:length} *)
  | Length of var (* ${#parameter} *)
  | ReplFirst of var * expr * expr (* $ {parameter/pattern/string} *)
  | ReplAll of var * expr * expr (* ${parameter//pattern/string} *)
  | ReplBeg of var * expr * expr (* ${parameter/#pattern/string} *)
  | ReplEnd of var * expr * expr (* ${parameter/%pattern/string} *)
  | RemShortFromBeg of var * expr (* ${parameter#pattern} *)
  | RemLargFromBeg of var * expr (* ${parameter##pattern} *)
  | RemShortFromEnd of var * expr (* ${parameter%pattern} *)
  | RemLargFromEnd of var * expr (* ${parameter %% pattern} *)
  | UseDefValue of var * expr (* ${parameter:-word} *)
  | SetDefValue of var * expr (* ${parameter:=word} *)
  | UseAlterValue of var * expr (* ${parameter:+word} *)
[@@deriving show { with_path = false }]

and redirect =
  | RedirectInput of expr (* < SOURCE *)
  | RedirectOutput of expr (* > TARGET *)
  | AppendRedirOutput of expr (* >> TARGET *)
[@@deriving show { with_path = false }]

and funcn = string * command list [@@deriving show { with_path = false }]

type declaration =
  | Funcn of funcn
  | Pipeline of pipeline
[@@deriving show { with_path = false }]

type ast = Declarations of declaration list [@@deriving show { with_path = false }]
