(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error

val pp_error : Format.formatter -> error -> unit
val parse : string -> (Ast.expr list, error) result
