open Bash_lib.Parser
open Angstrom

let parse_print s =
  match parse_string ~consume:Consume.All (ast_parser ()) s with
  | Error e -> print_endline ("ERROR" ^ e)
  | Ok b -> print_endline (Bash_lib.Ast.show_ast b)
;;

let () = parse_print (Stdio.In_channel.input_all Caml.stdin)
