(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let load_stdlib typ_env interpret_env input =
  match Parser.parse input with
  | Ok ast ->
    (match Inferencer.check_types ~env:typ_env ast with
     | Ok (typ_env, _) ->
       (match Interpret.run ~env:interpret_env ast with
        | Ok (interpret_env, _) -> typ_env, interpret_env
        | Error _ -> typ_env, interpret_env)
     | Error _ -> typ_env, interpret_env)
  | Error _ -> typ_env, interpret_env
;;
