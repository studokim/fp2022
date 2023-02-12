(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Format

let pp_result ppf (value, typ) =
  fprintf ppf "- : %a = %a" Pprinttypedtree.pp_typ_letter typ Pprintvalue.pp_value value
;;

let print_program ?(env_ty = Inferencer.empty) ?(env_inter = Interpret.empty) program =
  match Parser.parse program with
  | Ok ast ->
    (match Inferencer.check_types ~env:env_ty ast with
     | Ok (_, typ) ->
       (match Interpret.run ~env:env_inter ast with
        | Ok (_, res) -> printf "%a" pp_result (res, typ)
        | Error e -> printf "%a" Interpret.pp_ierror e)
     | Error e -> printf "%a" Inferencer.pp_error e)
  | Error e -> printf "%a" Parser.pp_error e
;;
