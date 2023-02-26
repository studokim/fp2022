open Bash_lib
open Parser
open Interpreter.Interpret (Interpreter.Result)
open Ast

let rec run ctx =
  let interpret_parsed ast =
    let inner_ast = function
      | Declarations a -> a
    in
    match interpret_bash ctx (inner_ast ast) with
    | Error e ->
      print_endline ("INTERPRETER ERROR: " ^ e);
      run ctx
    | Ok new_ctx -> run new_ctx
  in
  let input =
    print_string "$ ";
    parse (ast_parser ()) (read_line ())
  in
  match input with
  | Error e ->
    print_endline ("PARSER ERROR: " ^ e);
    run ctx
  | Ok b -> interpret_parsed b
;;

let () =
  print_endline "\nWelcome to bash";
  run empty_ctx
;;
