(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Lua_lib

(** Reads file contents and executes them as Repl does;
    fails if the file doesn't exist. *)
let interpret_file filepath =
  if Sys.file_exists filepath
  then (
    let program = Util.read_file filepath in
    let _ = Repl.execute program Interpreter.Environment.empty in
    ())
  else failwith "file doesn't exist"
;;

(** Entry point.
    Parses args, then runs repl-mode or single-file-mode.*)
let main =
  if Array.length Sys.argv = 2
  then (
    let filepath = Sys.argv.(1) in
    interpret_file filepath)
  else (
    print_endline "Hello from Lua!";
    Repl.iterate Interpreter.Environment.empty)
;;

let () = main
