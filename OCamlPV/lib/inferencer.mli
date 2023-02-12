(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Represents errors that may occur when checking types *)

type error =
  [ `Occurs_check
  | `No_variable of string
  | `Unification_failed of Typedtree.ty * Typedtree.ty
  | `Empty_pattern
  | `Empty_input
  ]

val pp_error : Format.formatter -> error -> unit

type environment

val empty : environment
val get_available_bindings : environment -> (string, Typedtree.ty) Base.List.Assoc.t

(** Check types of input expressions *)
val check_types
  :  ?env:environment
  -> Ast.expr list
  -> (environment * Typedtree.ty, error) result
