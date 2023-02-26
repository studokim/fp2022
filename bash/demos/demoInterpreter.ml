open Bash_lib.Parser
open Bash_lib.Interpreter
open Bash_lib.Interpreter.Interpret (Result)
open Bash_lib.Ast

let run ctx =
  let interpret_parsed ast =
    let inner_ast = function
      | Declarations a -> a
    in
    match interpret_bash ctx (inner_ast ast) with
    | Error e -> print_endline ("INTERPRETER ERROR: " ^ e)
    | Ok new_ctx ->
      print_endline ("Succeeded with retcode " ^ string_of_int new_ctx.retcode)
  in
  let input = parse (ast_parser ()) (Stdio.In_channel.input_all Caml.stdin) in
  match input with
  | Error e -> print_endline ("PARSER ERROR: " ^ e)
  | Ok b -> interpret_parsed b
;;

let () = run empty_ctx
