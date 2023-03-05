(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type identifier = Name of string

type literal =
  | Nil
  | Bool of bool
  | Numeric of float
  | String of string

type operator =
  | AddOp
  | SubOp
  | MulOp
  | DivOp
  | EqualsOp
  | AndOp
  | OrOp

type expression =
  | Identifier of identifier
  | Literal of literal
  | Binop of expression * operator * expression
  | Call of identifier * params

and params = expression list

type statement =
  | Comment
  | Expression of expression
  | Assignment of identifier * expression
  | Branch of expression * chunk * chunk
  | Definition of identifier * args * chunk
  | Return of expression

and args = identifier list
and chunk = Chunk of statement list
