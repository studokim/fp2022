(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** The error to be thrown when parsing fails (wrong input). *)
exception Error of string

(** Unpacks [Result.Ok|Error]. *)
val unpack : ('a, string) Result.t -> 'a

(** Functions needed to parse [Identifier]. *)
module Identifier : sig
  (** [Parser.Identifier] entry point. Parses [Name] ast node. *)
  val name : Ast.identifier Angstrom.t
end

(** Functions needed to parse [Literal]. *)
module Literal : sig
  (** Parses [Nil] ast node. *)
  val nil : Ast.literal Angstrom.t

  (** Parses [Bool] ast node. *)
  val bool : Ast.literal Angstrom.t

  (** Parses [Numeric] ast node. *)
  val numeric : Ast.literal Angstrom.t

  (** Parses [String] ast node. *)
  val string : Ast.literal Angstrom.t

  (** [Parser.Literal] entry point. Parses any literal. *)
  val literal : Ast.literal Angstrom.t
end

(** [Parser.Literal] entry point. Parses any literal. *)
module Expression : sig
  (** Parses [Identifier] ast node. *)
  val identifier : Ast.expression Angstrom.t

  (** Parses [Literal] ast node. *)
  val literal : Ast.expression Angstrom.t

  (** [Parser.Expression] entry point. Parses [Binop], [Call] and other expressions. *)
  val expression : Ast.expression Angstrom.t
end

(** Functions needed to parse [Statement]. *)
module Statement : sig
  (** Parses [Comment] ast node. *)
  val comment : Ast.statement Angstrom.t

  (** Parses [Expression] ast node. *)
  val expression : Ast.statement Angstrom.t

  (** Parses [Assignment] ast node. *)
  val assignment : Ast.statement Angstrom.t

  (** [Parser.Statement] entry point. Parses [Branch], [Definition], [Return] and other statements. *)
  val statement : Ast.statement Angstrom.t
end

(** Functions needed to parse [Chunk]. *)
module Chunk : sig
  (** [Parser.Chunk] entry point. Parses [Chunk] ast node. *)
  val chunk : Ast.chunk Angstrom.t
end

(** Parser entry point. *)
val parse : string -> Ast.chunk
