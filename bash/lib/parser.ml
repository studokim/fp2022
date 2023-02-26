(** Copyright 2022-2023, Kseniia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

(* Reserved words are words that have special meaning to the shell *)

let reserved =
  [ "if"
  ; "then"
  ; "else"
  ; "fi"
  ; "for"
  ; "in"
  ; "while"
  ; "do"
  ; "done"
  ; "case"
  ; "esac"
  ; "function"
  ; "{"
  ; "}"
  ]
;;

(* Helper function from https://ocaml.org/p/angstrom/0.9.0 *)
let chainl1 e op =
  let rec go acc = lift2 (fun f x -> f acc x) op e >>= go <|> return acc in
  e >>= fun init -> go init
;;

(* Simple parsers and helpers *)
let parse p s = parse_string ~consume:Consume.All p s

let is_whitespace = function
  | '\x20' | '\x09' -> true
  | _ -> false
;;

let is_newline = function
  | '\x0a' | '\x0d' -> true
  | _ -> false
;;

let is_special_character = function
  | '|' | '&' | ';' | '(' | ')' | '<' | '>' -> true
  | c when is_whitespace c -> true
  | _ -> false
;;

let is_letter = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_string = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '.' -> true
  | _ -> false
;;

let sign =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> "-"
  | Some '+' -> advance 1 >>| fun () -> "+"
  | Some c when is_digit c -> return "+"
  | _ -> fail "Unexpected char"
;;

let whitespace = take_while is_whitespace
let newline = take_while is_newline
let trim_whitespaces p = whitespace *> p <* whitespace
let parenthesis p = char '(' *> trim_whitespaces p <* char ')'

(* Variable parser *)
let is_valid_var_first_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_valid_var_char = function
  | '_' | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let name =
  whitespace *> peek_char
  >>= function
  | Some c when is_valid_var_first_char c ->
    take_while is_valid_var_char
    >>= fun nm ->
    (match List.mem nm reserved with
     | false -> return nm
     | true -> fail "Reserved word")
  | _ -> fail "Invalid symbol"
;;

let variable_parser : var t =
  name
  >>= fun n ->
  char '[' *> take_till (( = ) ']') <* char ']' <|> return "0" >>| fun s -> n, s
;;

(* Binary operators according to the Arithmetic Expansion *)
let plus = char '+' *> return (fun x y -> Binop (Add (x, y)))
let minus = char '-' *> return (fun x y -> Binop (Sub (x, y)))
let mul = char '*' *> return (fun x y -> Binop (Mul (x, y)))
let div = char '/' *> return (fun x y -> Binop (Div (x, y)))
let equal = string "==" *> return (fun x y -> Binop (Eq (x, y)))
let nequal = string "!=" *> return (fun x y -> Binop (Ne (x, y)))
let greater_than = char '>' *> return (fun x y -> Binop (Gt (x, y)))
let greater_equal = string ">=" *> return (fun x y -> Binop (Ge (x, y)))
let less_than = char '<' *> return (fun x y -> Binop (Lt (x, y)))
let less_equal = string "<=" *> return (fun x y -> Binop (Le (x, y)))
let mul_div = mul <|> div <* whitespace
let plus_minus = whitespace *> plus <|> minus <* whitespace

let compare =
  less_equal
  <|> less_than
  <|> greater_than
  <|> greater_equal
  <|> nequal
  <|> equal
  <* whitespace
;;

let int_parser =
  sign
  >>= fun sign -> take_while1 is_digit >>= fun num -> return (int_of_string (sign ^ num))
;;

let number_parser =
  sign
  >>= fun sign ->
  take_while1 is_digit >>= fun num -> return (Const (Int (int_of_string (sign ^ num))))
;;

let binary_operator_parser =
  let integer = int_parser >>| fun n -> Const (Int n) in
  let variable = variable_parser >>| fun n -> Var n in
  let func_name_parser = take_while1 is_valid_var_char in
  let var =
    char '$' *> func_name_parser
    >>= fun name ->
    char '[' *> take_till (( = ) ']')
    <* char ']'
    <|> return "0"
    >>= fun index -> return (Var (name, index))
  in
  fix (fun expr ->
    let factor = parenthesis expr <|> integer <|> variable <|> var in
    let term = chainl1 factor (trim_whitespaces mul_div) in
    let muldiv = chainl1 term (trim_whitespaces plus_minus) in
    let compare = chainl1 muldiv (trim_whitespaces compare) in
    let assignment =
      variable_parser
      >>= fun x -> trim_whitespaces (char '=') *> compare >>| fun y -> Assignment (x, y)
    in
    assignment <|> compare)
;;

(* Parameter expansion *)

let parameter_expansion_parser () =
  let offset =
    char '$' *> char '{' *> variable_parser
    >>= fun var ->
    char ':' *> number_parser
    >>= fun offset -> char '}' *> return (ParameterExpansion (Offset (var, offset)))
  in
  let offset_length =
    char '$' *> char '{' *> variable_parser
    >>= fun var ->
    char ':' *> number_parser
    >>= fun offset ->
    char ':' *> number_parser
    >>= fun length ->
    char '}' *> return (ParameterExpansion (OffsetLen (var, offset, length)))
  in
  let length =
    char '$' *> char '{' *> char '#' *> skip_while is_whitespace *> variable_parser
    <* char '}'
    >>| fun result -> ParameterExpansion (Length result)
  in
  let replace_first =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    char '/'
    *> take_till (function
         | '/' -> true
         | _ -> false)
    >>= fun pattern ->
    char '/'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun str ->
    ParameterExpansion (ReplFirst (parameter, Const (String pattern), Const (String str)))
  in
  let replace_all =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string "//"
    *> take_till (function
         | '/' -> true
         | _ -> false)
    >>= fun pattern ->
    char '/'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun str ->
    ParameterExpansion (ReplAll (parameter, Const (String pattern), Const (String str)))
  in
  let replace_beginning =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string "/#"
    *> take_till (function
         | '/' -> true
         | _ -> false)
    >>= fun pattern ->
    char '/'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun str ->
    ParameterExpansion (ReplBeg (parameter, Const (String pattern), Const (String str)))
  in
  let replace_end =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string "/%"
    *> take_till (function
         | '/' -> true
         | _ -> false)
    >>= fun pattern ->
    char '/'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun str ->
    ParameterExpansion (ReplEnd (parameter, Const (String pattern), Const (String str)))
  in
  let remove_shortest_beg =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    char '#'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun pattern ->
    ParameterExpansion (RemShortFromBeg (parameter, Const (String pattern)))
  in
  let remove_largest_beg =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string "##"
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun pattern ->
    ParameterExpansion (RemLargFromBeg (parameter, Const (String pattern)))
  in
  let remove_shortest_end =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    char '%'
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun pattern ->
    ParameterExpansion (RemShortFromEnd (parameter, Const (String pattern)))
  in
  let remove_largest_end =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string "%%"
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun pattern ->
    ParameterExpansion (RemLargFromEnd (parameter, Const (String pattern)))
  in
  let default_value =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string ":-"
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun word -> ParameterExpansion (UseDefValue (parameter, Const (String word)))
  in
  let set_default_value =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string ":="
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun word -> ParameterExpansion (SetDefValue (parameter, Const (String word)))
  in
  let use_alt_value =
    char '$' *> char '{' *> variable_parser
    >>= fun parameter ->
    string ":+"
    *> take_till (function
         | '}' -> true
         | _ -> false)
    <* char '}'
    >>| fun word -> ParameterExpansion (UseAlterValue (parameter, Const (String word)))
  in
  offset
  <|> offset_length
  <|> length
  <|> replace_all
  <|> replace_beginning
  <|> replace_end
  <|> replace_first
  <|> remove_largest_beg
  <|> remove_shortest_beg
  <|> remove_largest_end
  <|> remove_shortest_end
  <|> default_value
  <|> set_default_value
  <|> use_alt_value
;;

(* Expression parser *)

let rec expression_parser () =
  fix (fun _ ->
    double_quotes_parser ()
    <|> brace_expansion_parser ()
    <|> parameter_expansion_parser ()
    <|> arithmetic_expansion_parser
    <|> command_substitution_parser ()
    <|> single_quotes_parser
    <|> array_name_parser
    <|> var_name_parser
    <|> string_parser)

and brace_expansion_parser () =
  take_till (function
    | '{' -> true
    | _ -> false)
  >>= fun s_beg ->
  char '{' *> sep_by (char ',') (expression_parser ())
  <* char '}'
  >>= fun expressions ->
  take_till (function
    | '}' -> true
    | _ -> false)
  >>= fun s_end ->
  return (BraceExpansion (Const (String s_beg) :: Const (String s_end) :: expressions))

and string_parser =
  take_while1 is_string
  >>= fun s ->
  match List.mem s reserved with
  | true -> fail "Reserved word"
  | false -> return (Const (String s))

and single_quotes_parser =
  char '\''
  *> take_till (function
       | '\'' -> true
       | _ -> false)
  >>= fun s -> char '\'' *> return (SingleQuotes s)

and double_quotes_parser () =
  char '\"' *> many1 (words_parser <|> dq_symbols_parser ())
  <* char '\"'
  >>= fun s -> return (DoubleQuotes s)

and words_parser =
  take_while1 (function
    | '$' | '`' | '\\' | '\"' -> false
    | _ -> true)
  >>= fun s -> return (Const (String s))

and dq_symbols_parser () =
  peek_char
  >>= (function
        | Some '$' | Some '`' -> expression_parser ()
        | None -> fail "End of input"
        | _ -> fail "Unexpected char")
  <|> char '\x5c' *> escape_parser ()

and escape_parser () =
  peek_char
  >>= function
  | Some '$' -> escape_words_parser "$"
  | Some '`' -> escape_words_parser "`"
  | Some '\"' -> escape_words_parser "\""
  | Some '\x5c' -> escape_words_parser "\x5c"
  | _ -> escape_words_parser ""

and escape_words_parser c =
  string c
  *> take_while1 (function
       | '$' | '`' | '\\' | '\"' -> false
       | _ -> true)
  >>= fun s -> return (Const (String (c ^ s)))

and arithmetic_expansion_parser =
  string "$((" *> trim_whitespaces binary_operator_parser
  <* string "))"
  >>| fun result -> ArithmeticExpansion result

and command_substitution_parser () =
  char '`' *> command_parser ()
  <* char '`'
  >>= (fun res -> return (CommandSubstitution res))
  <|> (string "$(" *> command_parser ()
      <* char ')'
      >>= fun res -> return (CommandSubstitution res))

and array_name_parser =
  let var_name_parser = take_while1 is_valid_var_char in
  char '$' *> char '{' *> var_name_parser
  >>= fun name ->
  char '[' *> take_till (( = ) ']')
  <* char ']'
  <|> return "0"
  <* char '}'
  >>= fun index -> return (Var (name, index))

and var_name_parser =
  let var_name_parser = take_while1 is_valid_var_char in
  char '$' *> var_name_parser
  >>= fun name ->
  char '[' *> take_till (( = ) ']')
  <* char ']'
  <|> return "0"
  >>= fun index -> return (Var (name, index))

(* Compound *)
and compound_parser () =
  if_else () <|> while_loop () <|> for_iter () <|> for_in () <|> case () <* newline

and if_else () =
  string "if"
  *> whitespace
  *> string "(("
  *> whitespace
  *> (binary_operator_parser <|> expression_parser ())
  >>= fun condition ->
  whitespace
  *> string "))"
  *> char ';'
  *> whitespace
  *> string "then"
  *> whitespace
  *> newline
  *> block_parser ()
  >>= fun then_block ->
  whitespace
  *> newline
  *> option
       None
       (string "else" *> whitespace *> newline *> whitespace *> block_parser ()
       >>| fun else_block -> Some else_block)
  >>= fun else_block -> string "fi" *> return (IfElse (condition, then_block, else_block))

and while_loop () =
  string "while"
  *> trim_whitespaces (string "((")
  *> trim_whitespaces (binary_operator_parser <|> expression_parser ())
  >>= fun condition ->
  string "))" *> trim_whitespaces (string "do") *> newline *> block_parser ()
  <* trim_whitespaces (string "done")
  >>| fun while_block -> While (condition, while_block)

and for_iter () =
  string "for" *> trim_whitespaces (string "((") *> binary_operator_parser
  >>= fun expr1 ->
  char ';' *> trim_whitespaces binary_operator_parser
  >>= fun expr2 ->
  char ';'
  *> (name
     <* string "++"
     >>= (fun expr3 ->
           string "))" *> trim_whitespaces (string "do") *> newline *> block_parser ()
           <* newline
           <* whitespace
           <* string "done"
           >>| fun for_block ->
           For
             ( expr1
             , expr2
             , Assignment ((expr3, "0"), Binop (Add (Var (expr3, "0"), Const (Int 1))))
             , for_block ))
     <|> (name
         <* string "--"
         >>= fun expr3 ->
         string "))" *> trim_whitespaces (string "do") *> newline *> block_parser ()
         <* newline
         <* whitespace
         <* string "done"
         >>| fun for_block ->
         For
           ( expr1
           , expr2
           , Assignment ((expr3, "0"), Binop (Add (Var (expr3, "0"), Const (Int 1))))
           , for_block )))

and for_in () =
  trim_whitespaces (string "for") *> variable_parser
  >>= fun var ->
  whitespace *> string "in" *> many (whitespace *> expression_parser ())
  <* trim_whitespaces (string "do")
  >>= fun expr_list ->
  newline *> block_parser ()
  <* trim_whitespaces (string "done")
  >>= fun for_block -> return (ForIn (Var var, expr_list, for_block))

and case () =
  string "case" *> whitespace *> var_name_parser
  <* whitespace
  >>= fun expr ->
  string "in" *> newline *> many1 (pattern_parser () <* newline)
  <* string "esac"
  >>= fun ptrns -> return (Case (expr, ptrns))

and pattern_parser () =
  whitespace *> sep_by1 (whitespace *> char '|' <* whitespace) (expression_parser ())
  <* char ')' *> newline
  >>= fun pattern ->
  block_parser () <* newline <* whitespace <* string ";;" >>| fun block -> pattern, block

(* Command parser *)

and block_parser () =
  many (whitespace *> newline *> command_parser () <* many (char ';') <* newline)

and command_parser () =
  array_assignment_parser ()
  <|> associative_array_assignment_parser ()
  <|> simple_var_assignment_parser ()
  <|> simple_command_parser ()
  <|> (expression_parser () >>= fun res -> return (Expression res))
  <|> (compound_parser () >>= fun res -> return (Compound res))

and associative_array_assignment_parser () =
  take_till (function
    | '=' -> true
    | _ -> false)
  >>= fun name ->
  char '='
  *> (char '('
      *> whitespace
      *> sep_by
           (char ' ')
           (char '[' *> take_while is_string
           <* char ']'
           <* char '='
           >>= fun key -> expression_parser () >>= fun expr -> return (key, expr))
     <* char ')'
     >>= fun value -> return (VarAssignment (AssociativeArray (name, value))))

and array_assignment_parser () =
  take_till (function
    | '=' -> true
    | _ -> false)
  >>= fun name ->
  char '='
  *> (char '(' *> whitespace *> sep_by (char ' ') (expression_parser ())
     >>= fun values ->
     whitespace *> char ')' *> newline *> return (VarAssignment (Array (name, values))))

and simple_var_assignment_parser () =
  variable_parser
  >>= fun name ->
  char '=' *> expression_parser ()
  >>= fun value -> return (VarAssignment (SimpleVar (name, value)))

and simple_command_parser () =
  let arguments_parser () =
    redirect_parser ()
    >>= (fun red -> return (Redirect red))
    <|> (expression_parser () >>= fun arg -> return arg)
  in
  let cmd_name = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '{' -> true
    | _ -> false
  in
  whitespace *> take_while1 cmd_name
  >>= fun name ->
  match
    Base.String.exists
      ~f:
        (function
         | '{' -> true
         | _ -> false)
      name
  with
  | true -> fail "Forbidden char"
  | false ->
    (match List.mem name reserved with
     | true -> fail "Reserved word"
     | false ->
       many (trim_whitespaces (arguments_parser ()))
       >>= fun args -> return (SimpleCommand (name, args)))

(* Redirect *)
and redirect_parser () =
  redirect_input () <|> redirect_output () <|> redirect_append_output ()

and redirect_input () =
  trim_whitespaces (string "<") *> expression_parser ()
  >>| fun target -> RedirectInput target

and redirect_output () =
  trim_whitespaces (string ">") *> expression_parser ()
  >>| fun target -> RedirectOutput target

and redirect_append_output () =
  trim_whitespaces (string ">>") *> expression_parser ()
  >>| fun target -> AppendRedirOutput target

(* Functions *)

and wo_spec_word_parser () =
  let fun_name_helper = function
    | '(' | '\n' -> false
    | _ -> true
  in
  take_while1 fun_name_helper
  >>= fun name ->
  whitespace
  *> string "()"
  *> whitespace
  *> newline
  *> whitespace
  *> char '{'
  *> newline
  *> block_parser ()
  <* newline
  >>= fun body ->
  newline *> trim_whitespaces newline *> char '}' *> return (Funcn (name, body))

and spec_word_parser () =
  let fun_name_helper = function
    | ' ' | '\n' -> false
    | _ -> true
  in
  string "function" *> trim_whitespaces (take_while1 fun_name_helper)
  >>= fun name ->
  whitespace *> newline *> trim_whitespaces (char '{') *> newline *> block_parser ()
  <* whitespace
  <* newline
  <* whitespace
  >>= fun body -> char '}' *> return (Funcn (name, body))

and functions_parser () = spec_word_parser () <|> wo_spec_word_parser ()

(* Pipeline parser*)
and pipeline_parser () =
  let command_list_parser () =
    many1 (whitespace *> command_parser () <* whitespace)
    >>= fun command_list -> return (CommandList command_list)
  in
  let pipe_parser () =
    sep_by1 (char '|') (whitespace *> command_parser () <* whitespace)
    >>= fun pipe ->
    match List.length pipe with
    | 0 -> fail "Not pipeline"
    | _ -> return (Pipe pipe)
  in
  let and_pipeline_parser () =
    sep_by1 (string "&&") (pipe_parser ())
    >>= fun pipe ->
    match List.length pipe with
    | 1 -> fail "Not pipeline list"
    | _ -> return (AndList pipe)
  in
  let or_pipeline_list () =
    sep_by1 (string "||") (pipe_parser ())
    >>= fun pipe ->
    match List.length pipe with
    | 1 -> fail "Not pipeline list"
    | _ -> return (OrList pipe)
  in
  and_pipeline_parser ()
  <|> or_pipeline_list ()
  <|> pipe_parser ()
  <|> command_list_parser ()

(* Ast parser*)

and ast_parser () =
  many1
    (newline *> functions_parser ()
    >>= (fun f -> newline *> return f)
    <|> (pipeline_parser ()
        <* newline
        <* many (char ';')
        <* newline
        >>= fun p -> return (Pipeline p)))
  >>= fun ast -> return (Declarations ast)
;;

(* Tests *)
let%test _ = parse variable_parser "if" = Error ": Reserved word"
let%test _ = parse variable_parser "42" = Error ": Invalid symbol"
let%test _ = parse variable_parser "If" = Ok ("If", "0")
let%test _ = parse variable_parser "var[3]" = Ok ("var", "3")

(* Binary operator *)
let%test _ =
  parse binary_operator_parser "2+3" = Ok (Binop (Add (Const (Int 2), Const (Int 3))))
;;

let%test _ = parse binary_operator_parser "(2+3" = Error ": char '$'"

let%test _ =
  parse binary_operator_parser "(2+3)" = Ok (Binop (Add (Const (Int 2), Const (Int 3))))
;;

let%test _ =
  parse binary_operator_parser "(2+3)*5"
  = Ok (Binop (Mul (Binop (Add (Const (Int 2), Const (Int 3))), Const (Int 5))))
;;

let%test _ =
  parse binary_operator_parser "2+3*5"
  = Ok (Binop (Add (Const (Int 2), Binop (Mul (Const (Int 3), Const (Int 5))))))
;;

let%test _ =
  parse binary_operator_parser "2+3*5-(-1)"
  = Ok
      (Binop
         (Sub
            ( Binop (Add (Const (Int 2), Binop (Mul (Const (Int 3), Const (Int 5)))))
            , Const (Int (-1)) )))
;;

let%test _ =
  parse binary_operator_parser "2+3*5-(-1)/0"
  = Ok
      (Binop
         (Sub
            ( Binop (Add (Const (Int 2), Binop (Mul (Const (Int 3), Const (Int 5)))))
            , Binop (Div (Const (Int (-1)), Const (Int 0))) )))
;;

let%test _ =
  parse binary_operator_parser "(2+3*5-(-1))/0"
  = Ok
      (Binop
         (Div
            ( Binop
                (Sub
                   ( Binop
                       (Add (Const (Int 2), Binop (Mul (Const (Int 3), Const (Int 5)))))
                   , Const (Int (-1)) ))
            , Const (Int 0) )))
;;

let%test _ =
  parse binary_operator_parser "i+3" = Ok (Binop (Add (Var ("i", "0"), Const (Int 3))))
;;

let%test _ =
  parse binary_operator_parser "i=3" = Ok (Assignment (("i", "0"), Const (Int 3)))
;;

let%test _ =
  parse binary_operator_parser "i+3<(10+4)"
  = Ok
      (Binop
         (Lt
            ( Binop (Add (Var ("i", "0"), Const (Int 3)))
            , Binop (Add (Const (Int 10), Const (Int 4))) )))
;;

let%test _ =
  parse binary_operator_parser "4!=(10+4)"
  = Ok (Binop (Ne (Const (Int 4), Binop (Add (Const (Int 10), Const (Int 4))))))
;;

(* Parameter expansion *)

let%test _ = parse (parameter_expansion_parser ()) "Hello" = Error ": char '$'"

let%test _ =
  parse (parameter_expansion_parser ()) "${#var}"
  = Ok (ParameterExpansion (Length ("var", "0")))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var:14}"
  = Ok (ParameterExpansion (Offset (("var", "0"), Const (Int 14))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${var:num}" = Error ": string"
let%test _ = parse (parameter_expansion_parser ()) "${var:}" = Error ": string"
let%test _ = parse (parameter_expansion_parser ()) "${var:num:num}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${var:727:42}"
  = Ok (ParameterExpansion (OffsetLen (("var", "0"), Const (Int 727), Const (Int 42))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var/pat/str}"
  = Ok
      (ParameterExpansion
         (ReplFirst (("var", "0"), Const (String "pat"), Const (String "str"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var/ 42 / 727 }"
  = Ok
      (ParameterExpansion
         (ReplFirst (("var", "0"), Const (String " 42 "), Const (String " 727 "))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${var//42/727" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${var//42/727}"
  = Ok
      (ParameterExpansion
         (ReplAll (("var", "0"), Const (String "42"), Const (String "727"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var/# pat / str}"
  = Ok
      (ParameterExpansion
         (ReplBeg (("var", "0"), Const (String " pat "), Const (String " str"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var/ % pat / str}"
  = Ok
      (ParameterExpansion
         (ReplFirst (("var", "0"), Const (String " % pat "), Const (String " str"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var/% pat / str}"
  = Ok
      (ParameterExpansion
         (ReplEnd (("var", "0"), Const (String " pat "), Const (String " str"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var#pat}"
  = Ok (ParameterExpansion (RemShortFromBeg (("var", "0"), Const (String "pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var#}"
  = Ok (ParameterExpansion (RemShortFromBeg (("var", "0"), Const (String ""))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var##pat}"
  = Ok (ParameterExpansion (RemLargFromBeg (("var", "0"), Const (String "pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var# #pat}"
  = Ok (ParameterExpansion (RemShortFromBeg (("var", "0"), Const (String " #pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var%pat}"
  = Ok (ParameterExpansion (RemShortFromEnd (("var", "0"), Const (String "pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var%%pat}"
  = Ok (ParameterExpansion (RemLargFromEnd (("var", "0"), Const (String "pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${var% %pat}"
  = Ok (ParameterExpansion (RemShortFromEnd (("var", "0"), Const (String " %pat"))))
;;

let%test _ =
  parse (parameter_expansion_parser ()) "${param:-word}"
  = Ok (ParameterExpansion (UseDefValue (("param", "0"), Const (String "word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param: -word}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${param:- word}"
  = Ok (ParameterExpansion (UseDefValue (("param", "0"), Const (String " word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param :- word}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${param:=word}"
  = Ok (ParameterExpansion (SetDefValue (("param", "0"), Const (String "word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param: =word}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${param:= word}"
  = Ok (ParameterExpansion (SetDefValue (("param", "0"), Const (String " word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param := word}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${param:+word}"
  = Ok (ParameterExpansion (UseAlterValue (("param", "0"), Const (String "word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param: +word}" = Error ": string"

let%test _ =
  parse (parameter_expansion_parser ()) "${param:+ word}"
  = Ok (ParameterExpansion (UseAlterValue (("param", "0"), Const (String " word"))))
;;

let%test _ = parse (parameter_expansion_parser ()) "${param :+ word}" = Error ": string"

(* Expression parser *)

let%test _ =
  parse (expression_parser ()) "{a,b,c}"
  = Ok
      (BraceExpansion
         [ Const (String "")
         ; Const (String "")
         ; Const (String "a")
         ; Const (String "b")
         ; Const (String "c")
         ])
;;

let%test _ =
  parse (expression_parser ()) "hel{a,b,c}o"
  = Ok
      (BraceExpansion
         [ Const (String "hel")
         ; Const (String "o")
         ; Const (String "a")
         ; Const (String "b")
         ; Const (String "c")
         ])
;;

let%test _ =
  parse (expression_parser ()) "hel {a,b,c} o"
  = Ok
      (BraceExpansion
         [ Const (String "hel ")
         ; Const (String " o")
         ; Const (String "a")
         ; Const (String "b")
         ; Const (String "c")
         ])
;;

let%test _ =
  parse (expression_parser ()) "\"$(command)\""
  = Ok (DoubleQuotes [ CommandSubstitution (SimpleCommand ("command", [])) ])
;;

let%test _ =
  parse (expression_parser ()) "\"$(command arg arg)$(cmd)\""
  = Ok
      (DoubleQuotes
         [ CommandSubstitution
             (SimpleCommand ("command", [ Const (String "arg"); Const (String "arg") ]))
         ; CommandSubstitution (SimpleCommand ("cmd", []))
         ])
;;

let%test _ =
  parse (expression_parser ()) "\"string\""
  = Ok (DoubleQuotes [ Const (String "string") ])
;;

let%test _ =
  parse (expression_parser ()) "\"string with spaces\""
  = Ok (DoubleQuotes [ Const (String "string with spaces") ])
;;

let%test _ =
  parse (expression_parser ()) "\"There is a $var here\""
  = Ok
      (DoubleQuotes
         [ Const (String "There is a "); Var ("var", "0"); Const (String " here") ])
;;

let%test _ =
  parse (expression_parser ()) "\"There is no \\$var here\""
  = Ok (DoubleQuotes [ Const (String "There is no "); Const (String "$var here") ])
;;

let%test _ =
  parse (expression_parser ()) "\"`cmd`\""
  = Ok (DoubleQuotes [ CommandSubstitution (SimpleCommand ("cmd", [])) ])
;;

let%test _ =
  parse (expression_parser ()) "$((2+3))"
  = Ok (ArithmeticExpansion (Binop (Add (Const (Int 2), Const (Int 3)))))
;;

let%test _ =
  parse (expression_parser ()) "$((2+3<2))"
  = Ok
      (ArithmeticExpansion
         (Binop (Lt (Binop (Add (Const (Int 2), Const (Int 3))), Const (Int 2)))))
;;

let%test _ =
  parse (expression_parser ()) "$(cmd arg arg)"
  = Ok
      (CommandSubstitution
         (SimpleCommand ("cmd", [ Const (String "arg"); Const (String "arg") ])))
;;

let%test _ =
  parse (expression_parser ()) "`cmd arg arg`"
  = Ok
      (CommandSubstitution
         (SimpleCommand ("cmd", [ Const (String "arg"); Const (String "arg") ])))
;;

(* Compound parser *)
let%test _ =
  parse (compound_parser ()) "if ((var)); then\n    echo 1 2 3 \n    echo 1 \n    fi"
  = Ok
      (IfElse
         ( Var ("var", "0")
         , [ SimpleCommand
               ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ; SimpleCommand ("echo", [ Const (String "1") ])
           ]
         , None ))
;;

let%test _ =
  parse
    (compound_parser ())
    "if ((2 < 3)); then\n  echo 1 2 3 \nelse \n  echo 123\n  echo 1 2 3\nfi"
  = Ok
      (IfElse
         ( Binop (Lt (Const (Int 2), Const (Int 3)))
         , [ SimpleCommand
               ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ]
         , Some
             [ SimpleCommand ("echo", [ Const (String "123") ])
             ; SimpleCommand
                 ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
             ] ))
;;

let%test _ =
  parse (compound_parser ()) "if ((var)); then echo 1 2 3 echo else echo 12 fi"
  = Ok
      (IfElse
         ( Var ("var", "0")
         , [ SimpleCommand
               ( "echo"
               , [ Const (String "1")
                 ; Const (String "2")
                 ; Const (String "3")
                 ; Const (String "echo")
                 ] )
           ]
         , Some [ SimpleCommand ("echo", [ Const (String "12") ]) ] ))
;;

let%test _ =
  parse (compound_parser ()) "if ((var)) then echo 1 2 3 echo else echo 12 fi"
  = Error ": string"
;;

let%test _ =
  parse (compound_parser ()) "while ((true)) do \n    echo true\n   done"
  = Ok (While (Var ("true", "0"), [ SimpleCommand ("echo", [ Const (String "true") ]) ]))
;;

let%test _ =
  parse (compound_parser ()) "for ((i=1;i<3;i++)) do\necho 1 2 3\ndone"
  = Ok
      (For
         ( Assignment (("i", "0"), Const (Int 1))
         , Binop (Lt (Var ("i", "0"), Const (Int 3)))
         , Assignment (("i", "0"), Binop (Add (Var ("i", "0"), Const (Int 1))))
         , [ SimpleCommand
               ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ] ))
;;

let%test _ =
  parse (compound_parser ()) "for ((i=1;i<3;i++)) do echo 1 2 3 done"
  = Ok
      (For
         ( Assignment (("i", "0"), Const (Int 1))
         , Binop (Lt (Var ("i", "0"), Const (Int 3)))
         , Assignment (("i", "0"), Binop (Add (Var ("i", "0"), Const (Int 1))))
         , [ SimpleCommand
               ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ] ))
;;

let%test _ =
  parse (compound_parser ()) "for number in 1 2 3 4 5 do \n  echo $number \ndone"
  = Ok
      (ForIn
         ( Var ("number", "0")
         , [ Const (String "1")
           ; Const (String "2")
           ; Const (String "3")
           ; Const (String "4")
           ; Const (String "5")
           ]
         , [ SimpleCommand ("echo", [ Var ("number", "0") ]) ] ))
;;

let%test _ =
  parse
    (compound_parser ())
    "for number in one two three four five do \n      echo $number \n    done"
  = Ok
      (ForIn
         ( Var ("number", "0")
         , [ Const (String "one")
           ; Const (String "two")
           ; Const (String "three")
           ; Const (String "four")
           ; Const (String "five")
           ]
         , [ SimpleCommand ("echo", [ Var ("number", "0") ]) ] ))
;;

let%test _ =
  parse (compound_parser ()) "for number in one two three four five do echo $number done"
  = Ok
      (ForIn
         ( Var ("number", "0")
         , [ Const (String "one")
           ; Const (String "two")
           ; Const (String "three")
           ; Const (String "four")
           ; Const (String "five")
           ]
         , [ SimpleCommand ("echo", [ Var ("number", "0") ]) ] ))
;;

let%test _ =
  parse
    (compound_parser ())
    "case $var in\n\npat1 | pat2)\necho 1\n;;\n\npat3)\necho 2 3\n;;\n\nesac"
  = Ok
      (Case
         ( Var ("var", "0")
         , [ ( [ Const (String "pat1"); Const (String "pat2") ]
             , [ SimpleCommand ("echo", [ Const (String "1") ]) ] )
           ; ( [ Const (String "pat3") ]
             , [ SimpleCommand ("echo", [ Const (String "2"); Const (String "3") ]) ] )
           ] ))
;;

(* Command parser *)

let%test _ = parse (command_parser ()) "arr=()" = Ok (VarAssignment (Array ("arr", [])))

let%test _ =
  parse (command_parser ()) "arr=( 1 2 3 )"
  = Ok
      (VarAssignment
         (Array ("arr", [ Const (String "1"); Const (String "2"); Const (String "3") ])))
;;

let%test _ =
  parse (command_parser ()) "arr=( odin dva tri )"
  = Ok
      (VarAssignment
         (Array
            ("arr", [ Const (String "odin"); Const (String "dva"); Const (String "tri") ])))
;;

let%test _ =
  parse (command_parser ()) "arr=('1' \"2\")"
  = Ok
      (VarAssignment
         (Array ("arr", [ SingleQuotes "1"; DoubleQuotes [ Const (String "2") ] ])))
;;

let%test _ =
  parse (command_parser ()) "var=var"
  = Ok (VarAssignment (SimpleVar (("var", "0"), Const (String "var"))))
;;

let%test _ = parse (command_parser ()) "var= var" = Error ": end_of_input"
let%test _ = parse (command_parser ()) "var =var" = Error ": end_of_input"

let%test _ =
  parse (command_parser ()) "var=$(cmd)"
  = Ok
      (VarAssignment
         (SimpleVar (("var", "0"), CommandSubstitution (SimpleCommand ("cmd", [])))))
;;

let%test _ =
  parse (command_parser ()) "echo 1 2 3 "
  = Ok
      (SimpleCommand
         ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ]))
;;

let%test _ =
  parse (command_parser ()) "echo $var"
  = Ok (SimpleCommand ("echo", [ Var ("var", "0") ]))
;;

let%test _ = parse (command_parser ()) "echo if" = Error ": end_of_input"

let%test _ =
  parse (command_parser ()) "echo 'if'"
  = Ok (SimpleCommand ("echo", [ SingleQuotes "if" ]))
;;

let%test _ =
  parse (block_parser ()) "echo 1;; echo 1"
  = Ok
      [ SimpleCommand ("echo", [ Const (String "1") ])
      ; SimpleCommand ("echo", [ Const (String "1") ])
      ]
;;

let%test _ =
  parse (block_parser ()) "echo 1;;\n\necho 1"
  = Ok
      [ SimpleCommand ("echo", [ Const (String "1") ])
      ; SimpleCommand ("echo", [ Const (String "1") ])
      ]
;;

(* Redirection (and command) parser *)
let%test _ =
  parse (command_parser ()) "echo hello > myfile"
  = Ok
      (SimpleCommand
         ( "echo"
         , [ Const (String "hello"); Redirect (RedirectOutput (Const (String "myfile"))) ]
         ))
;;

let%test _ =
  parse (command_parser ()) "echo hello > myfile"
  = Ok
      (SimpleCommand
         ( "echo"
         , [ Const (String "hello"); Redirect (RedirectOutput (Const (String "myfile"))) ]
         ))
;;

let%test _ =
  parse (command_parser ()) "cmd < myfile"
  = Ok (SimpleCommand ("cmd", [ Redirect (RedirectInput (Const (String "myfile"))) ]))
;;

(* Functions *)

let%test _ = parse (functions_parser ()) "func1()\n{\n\n}" = Ok (Funcn ("func1", []))

let%test _ =
  parse (functions_parser ()) "func1()\n{\n      func2\n  }"
  = Ok (Funcn ("func1", [ SimpleCommand ("func2", []) ]))
;;

let%test _ =
  parse
    (functions_parser ())
    "func1()\n\
    \    {\n\
    \          func 1 2 3\n\
    \          if ((var)); then\n\
    \            echo 1 2 3\n\
    \        fi\n\
    \      }"
  = Ok
      (Funcn
         ( "func1"
         , [ SimpleCommand
               ("func", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ; Compound
               (IfElse
                  ( Var ("var", "0")
                  , [ SimpleCommand
                        ( "echo"
                        , [ Const (String "1"); Const (String "2"); Const (String "3") ]
                        )
                    ]
                  , None ))
           ] ))
;;

let%test _ =
  parse (functions_parser ()) "function func {echo 1 2 3}"
  = Ok
      (Funcn
         ( "func"
         , [ SimpleCommand
               ("echo", [ Const (String "1"); Const (String "2"); Const (String "3") ])
           ] ))
;;

(* Pipelines *)

let%test _ =
  parse (pipeline_parser ()) "cmd | cmd | cmd"
  = Ok
      (Pipe
         [ SimpleCommand ("cmd", [])
         ; SimpleCommand ("cmd", [])
         ; SimpleCommand ("cmd", [])
         ])
;;

let%test _ =
  parse (pipeline_parser ()) "cmd | cmd && cmd | cmd"
  = Ok
      (AndList
         [ Pipe [ SimpleCommand ("cmd", []); SimpleCommand ("cmd", []) ]
         ; Pipe [ SimpleCommand ("cmd", []); SimpleCommand ("cmd", []) ]
         ])
;;

let%test _ =
  parse (pipeline_parser ()) "cmd | cmd || cmd | cmd"
  = Ok
      (OrList
         [ Pipe [ SimpleCommand ("cmd", []); SimpleCommand ("cmd", []) ]
         ; Pipe [ SimpleCommand ("cmd", []); SimpleCommand ("cmd", []) ]
         ])
;;

let%test _ =
  parse (ast_parser ()) "cmd; cmd"
  = Ok
      (Declarations
         [ Pipeline (Pipe [ SimpleCommand ("cmd", []) ])
         ; Pipeline (Pipe [ SimpleCommand ("cmd", []) ])
         ])
;;

let%test _ =
  parse (ast_parser ()) "cmd 1 2 3; cmd 1 2 3"
  = Ok
      (Declarations
         [ Pipeline
             (Pipe
                [ SimpleCommand
                    ("cmd", [ Const (String "1"); Const (String "2"); Const (String "3") ])
                ])
         ; Pipeline
             (Pipe
                [ SimpleCommand
                    ("cmd", [ Const (String "1"); Const (String "2"); Const (String "3") ])
                ])
         ])
;;
