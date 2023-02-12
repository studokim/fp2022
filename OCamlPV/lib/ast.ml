(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving eq, show { with_path = false }]

type const =
  | CInt of int (** Represents integer numbers: ..., -1, 0, 1, ... *)
  | CBool of bool (** Represents boolean values: false and true *)
  | CNil (** Represents empty list: [] *)
  | CString of string (** Represents string values: "Hello, world!" *)
  | CUnit (** Repsenets () *)
[@@deriving eq, show { with_path = false }]

type bin_op =
  | Plus (** Addition: + *)
  | Minus (** Subtraction:  - *)
  | Mult (** Multiplication: * *)
  | Divide (** Division: / *)
  | Mod (** Module: % *)
  | And (** Boolean operator: && *)
  | Or (** Boolean operator: || *)
  | Eq (** Comparison operator: = *)
  | Neq (** Comparison operator: <> *)
  | Gt (** Comparison operator: > *)
  | Lt (** Comparison operator: < *)
  | Gtq (** Comparison operator: >= *)
  | Ltq (** Comparison operator: <= *)
  | ConsConcat (** Add element to the head of the list: 1 :: [2; 3] = [1; 2; 3]*)
[@@deriving eq, show { with_path = false }]

type pattern =
  | PConst of const (** Represents constant patterns *)
  | PVar of id (** Represents varuable patterns *)
  | PTuple of pattern list (** Repsenets tuple pattern: (a, b) *)
  | PCons of pattern * pattern (** Represents the head :: tail pattern *)
  | PWild (** Representa pattern that shows that each case is an appropriate: _ *)
  | PPolyVariant of id * pattern list
      (** Polymorphic variants. Number of the elements in the list
          represents the number of arguments which contructor takes*)
[@@deriving eq, show { with_path = false }]

(** Represents parameters at type declaration: type 'a 'b my_typ = ...*)
and parameters = id list

and expr =
  | EConst of const (** Represents constasnts *)
  | EIfThenElse of expr * expr * expr
      (** Represents condition statement: if expr then expr else expr *)
  | ELet of id * expr (** Represents let declaration: let id = expr *)
  | ELetIn of id * expr * expr (** Represents let in declaraion: let id = expr in expr *)
  | ELetRec of id * expr (** Represents let rec declaration: let rec id = expr *)
  | ELetRecIn of id * expr * expr
      (** Represents let rec in declaration: let rec id = expr in expr *)
  | EMatch of expr * (pattern * expr) list
      (** Represents pattern matching: match expr with (pattern * expr) list *)
  | EBinOp of bin_op (** Represents binaty operations: +, -, *, /, ... *)
  | EVar of id (** Represents variables *)
  | EFun of pattern * expr (** Represents anonymous function: fun pattern -> expr*)
  | EApply of expr * expr
      (** Represents function and binary operation application to the arguments*)
  | EList of expr * expr (** Represents list: expr :: expr *)
  | ETuple of expr list (* Represents tuple: [expr; expr; expr] = (expr, expr, expr) *)
  | EPolyVariant of id * expr list
      (** Polymorphic variants, where id represents constructor and expr -- arguments *)
[@@deriving show { with_path = false }]

(** Represents the sequence of expr *)
and statements = expr list [@@deriving show { with_path = false }]
