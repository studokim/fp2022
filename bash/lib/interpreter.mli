(** Copyright 2022-2023, Kseniia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

type iconst =
  | Int of int
  | Bool of bool
  | String of string
  | StringList of string list
[@@deriving show]

module IMap : Map.S with type key = int

module ConstMap : sig
  type t [@@deriving show]

  val find_opt : string -> t -> iconst option
  val replace : string -> iconst -> t -> t
  val empty : t
  val from_val : iconst -> t
end

module FuncMap : sig
  type t

  val find_opt : string -> t -> Ast.command list option
  val replace : string -> Ast.command list -> t -> t
  val empty : t
end

module VarsMap : sig
  type t [@@deriving show { with_path = false }]

  val find_opt : string -> t -> ConstMap.t option
  val replace : string -> ConstMap.t -> t -> t
  val empty : t
end

type context =
  { vars : VarsMap.t
  ; functions : FuncMap.t
  ; retcode : int
  ; chs : Unix.file_descr IMap.t
  ; last_exec : iconst
  }

module Result : MonadFail with type 'a t = ('a, string) result

module Interpret (M : MonadFail) : sig
  val empty_ctx : context
  val interpret_bash : context -> Ast.declaration list -> context M.t
end
