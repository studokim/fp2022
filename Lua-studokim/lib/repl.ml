(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Prompts to input many lines. Returns when an empty line is given. *)
let read_input () =
  let rec next_line acc =
    match read_line () with
    | "" -> acc
    | line ->
      print_string ">> ";
      next_line (acc @ [ String.trim line ])
  in
  print_string ">  ";
  String.concat " " (next_line [])
;;

(** Wraps [Parser] and [Interpeter] together. *)
let execute input env =
  let _, env = Interpreter.Chunk.execute (Parser.parse input) env in
  env
;;

(** Recursively iterates: prompt -> execute -> prompt.
    Handles [Parser.Error] and [Interpreter.Error], i.e. wrong user input. *)
let rec iterate env =
  try
    let input = read_input () in
    let env = execute input env in
    iterate env
  with
  | End_of_file -> exit 0
  | Parser.Error m ->
    print_endline ("parser error" ^ m);
    iterate env
  | Interpreter.Error m ->
    print_endline ("interpreter error" ^ m);
    iterate env
;;
