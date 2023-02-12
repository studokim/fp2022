(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type 'a binop =
  | EmptyBinOp of Ast.bin_op
      (** Represents binary operation with no arguments applied: + *)
  | PartialBinOp of 'a * Ast.bin_op
      (** Represents binaty operation with one argument applied: 4 + *)

type value =
  | VInt of int (** Represents integer values: 1, 2, ... *)
  | VBool of bool (** Represents boolean values: false, true *)
  | VString of string (** Represents string values: "Ocaml is cool!" *)
  | VTuple of value list (** Represents tuples: (value1, value2, ... ) *)
  | VList of value list (** Represents lists: [value1; value2; ... ]*)
  | VFun of Ast.pattern * Ast.expr * (Ast.id * value) list
      (** Represents function: fun x -> x *)
  | VBinOp of value binop
      (** Represents binaty operation with no arguments applied and one argument applied *)
  | VPolyVariant of Ast.id * value list
      (** Represent polimorphic variant, id is a constructor and value list is constructor args*)
  | VUnit (** Represents unit: () *)
  | VNil (** Represents empty list: [] *)

type ierror
type environment

val pp_ierror : Format.formatter -> ierror -> unit

(** Return empty environment for interpret *)
val empty : environment

(** Interpret ast *)
val run : ?env:environment -> Ast.expr list -> (environment * value, ierror) result
