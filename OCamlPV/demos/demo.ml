(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: CC0-1.0 *)

let _ =
  let program = Stdio.In_channel.input_all Caml.stdin in
  Ocaml_pv_lib.Printer.print_program program
;;
