(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ocaml_pv_lib
open Format

let print_prompt () = printf "\n# %!"
let pp_res ppf (name, ty) = fprintf ppf "\t%s: %a" name Pprinttypedtree.pp_typ_letter ty

let pp_stdlib ppf types =
  fprintf ppf "%a" (fun ppf -> Pprinttypedtree.pp_list pp_res ppf "\n") types
;;

let print_hello types =
  let fstline = "Hello! It's an OCaml interpreter. Stdlib is already loaded!" in
  let sndline = "You may use the following functions:" in
  let hello = sprintf "\n%s\n%s\n" fstline sndline in
  printf "%s %a" hello pp_stdlib types
;;

let print_exit () =
  printf "\tThank you for using my OCaml interpret! Hope you enjoy it! :)\n%!"
;;

let run_input typ_env interpret_env input =
  match Parser.parse input with
  | Ok ast ->
    (match Inferencer.check_types ~env:typ_env ast with
     | Ok (typ_env, typ) ->
       (match Interpret.run ~env:interpret_env ast with
        | Ok (interpret_env, res) ->
          printf "%a%!" Printer.pp_result (res, typ);
          typ_env, interpret_env
        | Error e ->
          printf "%a%!" Interpret.pp_ierror e;
          typ_env, interpret_env)
     | Error e ->
       printf "%a%!" Inferencer.pp_error e;
       typ_env, interpret_env)
  | Error e ->
    printf "%a%!" Parser.pp_error e;
    typ_env, interpret_env
;;

let rec repl tenv ienv =
  print_prompt ();
  let input = read_line () in
  if Base.String.strip input = "exit"
  then print_exit ()
  else (
    let tenv, env = run_input tenv ienv input in
    repl tenv env)
;;

let () =
  let tenv, ienv = Utils.load_stdlib Inferencer.empty Interpret.empty Stdlib.std_lib in
  let available_types = Inferencer.get_available_bindings tenv in
  print_hello available_types;
  repl tenv ienv
;;
