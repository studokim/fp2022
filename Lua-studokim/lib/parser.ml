(** Copyright 2022-2023, studokim and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(** The error to be thrown when parsing fails (wrong input). *)
exception Error of string

(** Unpacks [Result.Ok|Error]. *)
let unpack = function
  | Result.Ok ast_node -> ast_node
  | Result.Error e -> raise (Error e)
;;

let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'
;;

let is_alpha c =
  let code = Char.code c in
  (code >= Char.code 'A' && code <= Char.code 'Z')
  || (code >= Char.code 'a' && code <= Char.code 'z')
;;

(** Consumes 0..inf whitespaces. *)
let whitespace =
  let is_whitespace = function
    | ' ' | '\t' | '\r' | '\n' -> true
    | _ -> false
  in
  take_while is_whitespace
;;

(** Returns any parser enclosed to  parentheses. *)
let parens p = char '(' *> whitespace *> p <* whitespace <* char ')'

(** Functions needed to parse [Identifier]. *)
module Identifier = struct
  (** Registered Lua keywords, may not be used as identifiers. *)
  let keywords =
    [ "function"
    ; "return"
    ; "if"
    ; "then"
    ; "else"
    ; "end"
    ; "and"
    ; "or"
    ; "true"
    ; "false"
    ; "nil"
    ]
  ;;

  (** [Parser.Identifier] entry point. Parses [Name] ast node. *)
  let name =
    let is_name c = is_alpha c || is_digit c || c = '_' in
    let first =
      peek_char
      >>= function
      | Some c when is_alpha c || c = '_' -> advance 1 *> return c
      | _ -> fail "alpha or underscore expected"
    in
    first
    >>= fun first ->
    take_while is_name
    >>= fun rest ->
    let name = Char.escaped first ^ rest in
    if List.mem name keywords then fail (name ^ "is a keyword") else return (Name name)
  ;;
end

(** Functions needed to parse [Literal]. *)
module Literal = struct
  (** Parses [Nil] ast node. *)
  let nil = string "nil" *> return Nil

  (** Parses [Bool] ast node. *)
  let bool = string "true" *> return (Bool true) <|> string "false" *> return (Bool false)

  (** Parses [Numeric] ast node. *)
  let numeric =
    (* TODO: there could be exponents https://www.lua.org/pil/2.3.html *)
    let sign =
      peek_char
      >>= function
      | Some '-' -> advance 1 *> return "-"
      | Some '+' -> advance 1 *> return "+"
      | Some c when is_digit c -> return "+"
      | _ -> fail "sign or digit expected"
    in
    sign
    >>= fun sign ->
    take_while1 is_digit
    >>= fun integer ->
    peek_char
    >>= function
    | Some '.' ->
      advance 1 *> take_while1 is_digit
      >>= fun fraction ->
      return (Numeric (float_of_string (Util.concat [ sign; integer; "."; fraction ])))
    | _ -> return (Numeric (float_of_string (sign ^ integer)))
  ;;

  (** Parses [String] ast node. *)
  let string =
    (* TODO: there could be escaped quotes *)
    let is_quote c = c = '"' in
    char '"' *> take_till is_quote <* char '"' >>= fun s -> return (String s)
  ;;

  (** [Parser.Literal] entry point. Parses any literal. *)
  let literal = choice [ nil; bool; numeric; string ]
end

(** Functions needed to parse [Expression]. *)
module Expression = struct
  (** Parses [Identifier] ast node. *)
  let identifier = Identifier.name >>= fun result -> return (Identifier result)

  (** Parses [Literal] ast node. *)
  let literal = Literal.literal >>= fun result -> return (Literal result)

  let operator =
    choice
      [ string "+" *> return AddOp
      ; string "-" *> return SubOp
      ; string "*" *> return MulOp
      ; string "/" *> return DivOp
      ; string "==" *> return EqualsOp
      ; string "and" *> return AndOp
      ; string "or" *> return OrOp
      ; fail "arithmetic or logical operator expected"
      ]
  ;;

  (** [Parser.Expression] entry point. Parses [Binop], [Call] and other expressions. *)
  let expression =
    fix (fun expression ->
      let binop =
        let binop_constructor left op right = Binop (left, op, right) in
        lift3
          binop_constructor
          (whitespace *> expression <* whitespace)
          operator
          (whitespace *> expression <* whitespace)
      in
      let call =
        let call_constructor id params = Call (id, params) in
        lift2
          call_constructor
          (Identifier.name <* whitespace)
          (parens (sep_by (char ',') (whitespace *> expression <* whitespace)))
      in
      choice [ call; identifier; literal; parens binop ])
  ;;
end

(** Functions needed to parse [Statement]. *)
module Statement = struct
  (** Parses [Comment] ast node. *)
  let comment =
    let start = string "--[[" in
    let finish = string "--]]" in
    let content =
      let is_dash c = c = '-' in
      fix (fun content -> take_till is_dash <* (finish <|> advance 1 *> content))
    in
    start *> content *> return Comment
  ;;

  (** Parses [Expression] ast node. *)
  let expression = Expression.expression >>= fun result -> return (Expression result)

  (** Parses [Assignment] ast node. *)
  let assignment =
    let operator = string "=" <|> fail "assignment operator expected" in
    let assignment_constructor id expr = Assignment (id, expr) in
    lift2
      assignment_constructor
      (Identifier.name <* whitespace <* operator)
      (whitespace *> Expression.expression)
  ;;

  let separator = string ";" <|> whitespace

  (** [Parser.Statement] entry point. Parses [Branch], [Definition], [Return] and other statements. *)
  let statement =
    fix (fun statement ->
      let chunk =
        sep_by separator (whitespace *> statement <* whitespace)
        >>= fun result -> return (Chunk result)
      in
      let branch =
        let b2_constructor condition thenpart = Branch (condition, thenpart, Chunk []) in
        let b2 =
          lift2
            b2_constructor
            (string "if" *> whitespace *> Expression.expression
            <* whitespace
            <* string "then")
            (whitespace *> chunk <* whitespace <* string "end")
        in
        let b3_constructor condition thenpart elsepart =
          Branch (condition, thenpart, elsepart)
        in
        let b3 =
          lift3
            b3_constructor
            (string "if" *> whitespace *> Expression.expression
            <* whitespace
            <* string "then")
            (whitespace *> chunk <* whitespace <* string "else")
            (whitespace *> chunk <* whitespace <* string "end")
        in
        b2 <|> b3
      in
      let definition =
        let definition_constructor id args body = Definition (id, args, body) in
        lift3
          definition_constructor
          (string "function" *> whitespace *> Identifier.name <* whitespace)
          (parens (sep_by (char ',') (whitespace *> Identifier.name <* whitespace)))
          (whitespace *> chunk <* whitespace <* string "end")
      in
      let return =
        string "return" *> whitespace *> (Expression.expression <|> return (Literal Nil))
        >>= fun result -> return (Return result)
      in
      choice [ comment; assignment; branch; definition; return; expression ])
  ;;
end

(** Functions needed to parse [Chunk]. *)
module Chunk = struct
  (** [Parser.Chunk] entry point. Parses [Chunk] ast node. *)
  let chunk =
    sep_by Statement.separator (whitespace *> Statement.statement <* whitespace)
    >>= fun result -> return (Chunk result)
  ;;
end

(** Parser entry point. *)
let parse string = unpack (parse_string ~consume:All Chunk.chunk string)
