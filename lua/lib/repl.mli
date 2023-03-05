(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Wraps [Parser] and [Interpeter] together. *)
val execute : string -> Interpreter.Environment.env -> Interpreter.Environment.env

(** Recursively iterates: prompt -> execute -> prompt.
    Handles [Parser.Error] and [Interpreter.Error], i.e. wrong user input. *)
val iterate : Interpreter.Environment.env -> 'a
