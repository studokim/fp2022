(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The error to be thrown when execution fails (wrong input). *)
exception Error of string

(** The error to be thrown when [Interpreter] behaves not as expected. *)
exception Implementation_error of string

(** Throw [Error]. *)
val fail : string -> 'a

(** Throw [Implementation.Error]. *)
val crash : string -> 'a

(** Data structures and functions used to preserve program state. *)
module Environment : sig
  module Identifier : sig
    type t = Ast.identifier

    val compare : Ast.identifier -> Ast.identifier -> int
  end

  module IdentifierMap : sig
    type 'a t = 'a Map.Make(Identifier).t
  end

  type env =
    { vars : Ast.literal IdentifierMap.t
    ; funcs : (Ast.args * Ast.chunk) IdentifierMap.t
    }

  (** Default empty [env]. *)
  val empty : env
end

(** Built-in functions implementations, such as [print()]. *)
module rec Builtins : sig
  (** Check if there's a built-in function with this [id]. *)
  val is_builtin : Ast.identifier -> bool

  (** Execute a built-in function with this [id]. *)
  val call_builtin
    :  Ast.identifier
    -> Ast.expression list
    -> Environment.env
    -> Ast.expression * Environment.env
end

(** Interpret function definitions and calls. *)
and Function : sig
  (** Execute [Call] statement. *)
  val call
    :  Ast.identifier
    -> Ast.params
    -> Environment.env
    -> Ast.expression * Environment.env
end

(** Interpret [Expression]. *)
and Expression : sig
  (** Execute any [Expression]. *)
  val execute : Ast.expression -> Environment.env -> Ast.expression * Environment.env
end

(** Interpret [Statement]. *)
and Statement : sig
  (** Execute any [Statement]. *)
  val execute : Ast.statement -> Environment.env -> Ast.expression * Environment.env
end

(** Interpret [Chunk]. *)
and Chunk : sig
  (** Execute [Chunk]. *)
  val execute : Ast.chunk -> Environment.env -> Ast.expression * Environment.env
end
