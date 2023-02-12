(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type binder = int [@@deriving eq, show { with_path = false }]

module VarSetInit = struct
  include Caml.Set.Make (Int)

  let pp ppf s =
    Format.fprintf ppf "[ ";
    iter (Format.fprintf ppf "%d; ") s;
    Format.fprintf ppf "]"
  ;;
end

type id = string [@@deriving eq, show { with_path = false }]

(** Represent polymorphic variant where id -- constructor and ty list -- types of arguments.
    Empty list means that there are no arguments *)
type pv = id * ty list [@@deriving eq, show { with_path = false }]

(** Represent the type of expression *)
and ty =
  | Prim of string (** Represents available ground types *)
  | Ty_var of binder (** Represents 'a, 'b types *)
  | Arrow of ty * ty (** Represents function type: 'a -> 'a *)
  | List of ty (** Represents list type: int list *)
  | Tuple of ty list (** Represents typle type: [int, string] means (int, string) *)
  | MoreTags of binder * pv list
      (** Represents any type that contains at least all polyvariants from pv list  *)
  | LessTags of binder * pv list
      (** Represents any type that contains all or less elemets from pv list *)
(* I have come up to the conclusion that the simplest way to deal
  with polymorphic variant is to add binder to identify and substity them in the inferencer.
  It's seems to me like a bad way but anothers are much worse... *)
[@@deriving show { with_path = false }]

(** Type constructors *)

let arrow l r = Arrow (l, r)
let int_typ = Prim "int"
let bool_typ = Prim "bool"
let var_typ x = Ty_var x
let list_typ x = List x
let tuple_typ x = Tuple x
let moretags_typ b pv = MoreTags (b, pv)
let lesstags_typ b pv = LessTags (b, pv)
let ( @-> ) = arrow
