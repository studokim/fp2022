(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Interpret
open Format

let pp_binop ppf = function
  | EmptyBinOp _ -> fprintf ppf "<binop>"
  | PartialBinOp (_, _) -> fprintf ppf "<binop>"
;;

let rec pp_value ppf v =
  let pp_list l sep =
    pp_print_list ~pp_sep:(fun ppf _ -> fprintf ppf sep) (fun ppf ty -> pp_value ppf ty) l
  in
  match v with
  | VInt i -> fprintf ppf "%d" i
  | VBool b -> fprintf ppf "%b" b
  | VString s -> fprintf ppf "\"%s\"" s
  | VTuple t -> fprintf ppf "(%a)" (fun ppf -> pp_list ppf ", ") t
  | VList l -> fprintf ppf "[%a]" (fun ppf -> pp_list ppf "; ") l
  | VFun (_, _, _) -> fprintf ppf "<fun>"
  | VBinOp binop -> fprintf ppf "%a" pp_binop binop
  | VPolyVariant (id, values) ->
    fprintf ppf "%s (%a)" id (fun ppf -> pp_list ppf ", ") values
  | VUnit -> fprintf ppf "()"
  | VNil -> fprintf ppf "[]"
;;
