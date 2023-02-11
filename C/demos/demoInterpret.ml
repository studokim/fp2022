(** Copyright 2022-2023, Mikhail Vyrodov *)

(** SPDX-License-Identifier: CC0-1.0 *)

open C_lib

let () =
  let s = Stdio.In_channel.input_all Caml.stdin in
  match Interpret.parse_and_run s with
  | Result.Ok ans -> print_endline (Interpret.value_to_str ans)
  | Error err -> Interpret.print_err (Error err)
