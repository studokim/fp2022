(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: CC0-1.0 *)

open Ocaml_pv_lib

let _ =
  let tenv, ienv = Utils.load_stdlib Inferencer.empty Interpret.empty Stdlib.std_lib in
  let program = Stdio.In_channel.input_all Caml.stdin in
  Ocaml_pv_lib.Printer.print_program ~env_ty:tenv ~env_inter:ienv program
;;
