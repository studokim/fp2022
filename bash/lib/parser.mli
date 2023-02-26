(** Copyright 2022-2023, Kseniia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val ast_parser : unit -> Ast.ast Angstrom.t
val parse : 'a Angstrom.t -> string -> ('a, string) result
