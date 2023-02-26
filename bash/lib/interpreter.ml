(** Copyright 2022-2023, Kseniia Kuzmina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MonadFail = sig
  include Base.Monad.Infix

  val return : 'a -> 'a t
  val fail : string -> 'a t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module Result : MonadFail with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) result

  let return = Result.ok
  let fail = Result.error
  let ( >>= ) = Result.bind
  let ( >>| ) f g = f >>= fun x -> return (g x)
  let ( *> ) f g = f >>= fun _ -> return g
  let ( <* ) f g = f >>= fun e -> return g >>= fun _ -> return e
  let ( let* ) = ( >>= )

  let ( <|> ) f g =
    match f with
    | Ok _ -> f
    | Error _ -> g
  ;;
end

type iconst =
  | Int of int
  | Bool of bool
  | String of string
  | StringList of string list
[@@deriving show]

module ConstMap : sig
  type t [@@deriving show]

  val find_opt : string -> t -> iconst option
  val replace : string -> iconst -> t -> t
  val empty : t
  val from_val : iconst -> t
end = struct
  module M = Map.Make (String)

  type t = iconst M.t

  let pp ppf t =
    Format.fprintf ppf "[";
    M.iter (fun k v -> Format.fprintf ppf "[%s] = %a" k pp_iconst v) t;
    Format.fprintf ppf "]"
  ;;

  let show m = Format.asprintf "%a" pp m
  let find_opt = M.find_opt
  let replace k v t = M.update k (fun _ -> Some v) t
  let empty = M.empty
  let from_val init = M.singleton "0" init
end

let c2icl = function
  | Ast.Int x -> Int x
  | Ast.Bool x -> Bool x
  | Ast.String x -> String x
;;

module VarsMap : sig
  type t [@@deriving show { with_path = false }]

  val find_opt : string -> t -> ConstMap.t option
  val replace : string -> ConstMap.t -> t -> t
  val empty : t
end = struct
  module M = Map.Make (String)

  type t = ConstMap.t M.t

  let pp ppf t =
    Format.fprintf ppf "[";
    M.iter (fun k v -> Format.fprintf ppf "[%s] = %a" k ConstMap.pp v) t;
    Format.fprintf ppf "]"
  ;;

  let show m = Format.asprintf "%a" pp m
  let find_opt = M.find_opt
  let replace k v t = M.update k (fun _ -> Some v) t
  let empty = M.empty
end

type block = command list [@@deriving show { with_path = false }]

module FuncMap : sig
  type t [@@deriving show]

  val find_opt : string -> t -> Ast.command list option
  val replace : string -> Ast.command list -> t -> t
  val empty : t
end = struct
  module M = Map.Make (String)

  type t = Ast.command list M.t

  let pp ppf t =
    Format.fprintf ppf "[";
    M.iter (fun k _ -> Format.fprintf ppf "[%s()]" k) t;
    Format.fprintf ppf "]"
  ;;

  let show m = Format.asprintf "%a" pp m
  let find_opt = M.find_opt
  let replace k v t = M.update k (fun _ -> Some v) t
  let empty = M.empty
end

module type PpOrderedType = sig
  include Map.OrderedType

  val pp_t : Format.formatter -> t -> unit
end

module TMap (T : PpOrderedType) = struct
  include Map.Make (T)

  let of_list l = of_seq (List.to_seq l)
  let add_list l = add_seq (List.to_seq l)

  let pp pp_v ppf m =
    let open Format in
    fprintf ppf "@[[@[";
    iter (fun k v -> fprintf ppf "@[%a: %a@],@\n" T.pp_t k pp_v v) m;
    fprintf ppf "@]]@]"
  ;;
end

module IMap = TMap (struct
  include Int

  let pp_t = Format.pp_print_int
end)

type context =
  { vars : VarsMap.t
  ; functions : FuncMap.t
  ; retcode : int
  ; chs : Unix.file_descr IMap.t
       [@printer fun fmt m -> IMap.pp (fun fmt _ -> fprintf fmt "[...]") fmt m]
  ; last_exec : iconst
  }
[@@deriving show { with_path = false }]

let show_ctx = show_context

module Interpret (M : MonadFail) = struct
  open M

  let empty_ctx =
    { vars = VarsMap.empty
    ; functions = FuncMap.empty
    ; retcode = 0
    ; chs = Unix.(IMap.of_list [ 0, stdin; 1, stdout; 2, stderr ])
    ; last_exec = Int 0
    }
  ;;

  let matching_iconst_string = function
    | Bool s -> return (string_of_bool s)
    | Int s -> return (string_of_int s)
    | String s -> return s
    | _ -> return ""
  ;;

  (** Finds a variable by name *)
  let rec get_var name ctx = VarsMap.find_opt name ctx.vars

  (** Adds a variable to the current context *)
  and set_var ctx name index value =
    let ret_wv x c = return { c with vars = VarsMap.replace name x c.vars } in
    interpret_expression value ctx
    >>= fun nctx ->
    match VarsMap.find_opt name nctx.vars with
    | Some v -> ret_wv (ConstMap.replace index nctx.last_exec v) nctx
    | None -> ret_wv (ConstMap.replace index nctx.last_exec ConstMap.empty) nctx

  (** Adds an indexed array to the current context *)
  and set_array ctx name value args_list =
    let ret_wv x c = return { c with vars = VarsMap.replace name x c.vars } in
    let to_string = function
      | Int i -> return (string_of_int i)
      | Bool b -> return (string_of_bool b)
      | String s -> return s
      | _ -> fail "Wrong pattern"
    in
    match value with
    | hd :: tl ->
      interpret_expression hd ctx
      >>= fun new_ctx ->
      to_string new_ctx.last_exec
      >>= fun str ->
      (match VarsMap.find_opt name ctx.vars with
       | Some v ->
         ret_wv
           (ConstMap.replace (string_of_int (List.length args_list)) (String str) v)
           ctx
       | None ->
         ret_wv
           (ConstMap.replace
              (string_of_int (List.length args_list))
              (String str)
              ConstMap.empty)
           ctx)
      >>= fun nctx -> set_array nctx name tl (List.append args_list [ str ])
    | [] -> return ctx

  (** Adds an associative array to the current context *)
  and set_associative_array ctx name value args_list =
    let ret_wv x c = return { c with vars = VarsMap.replace name x c.vars } in
    let to_string = function
      | Int i -> return (string_of_int i)
      | Bool b -> return (string_of_bool b)
      | String s -> return s
      | _ -> fail "Wrong pattern"
    in
    match value with
    | hd :: tl ->
      (match hd with
       | key, elem ->
         interpret_expression elem ctx
         >>= fun new_ctx ->
         to_string new_ctx.last_exec
         >>= fun str ->
         (match VarsMap.find_opt name ctx.vars with
          | Some v -> ret_wv (ConstMap.replace key (String str) v) ctx
          | None -> ret_wv (ConstMap.replace key (String str) ConstMap.empty) ctx)
         >>= fun nctx ->
         set_associative_array nctx name tl (List.append args_list [ str ]))
    | [] -> return ctx

  and set_funcn name block ctx =
    return { ctx with functions = FuncMap.replace name block ctx.functions }

  and get_funcn name ctx = FuncMap.find_opt name ctx.functions

  (** Interprets expressions like expansions, variables and arithmetics *)
  and interpret_expression expr ctx =
    match expr with
    | Const const -> return { ctx with last_exec = c2icl const }
    | Binop binop -> interpret_binop binop ctx
    | Var var -> interpret_variable ctx var
    | Assignment ((name, index), value) -> set_var ctx name index value
    | SingleQuotes sin_q -> return { ctx with last_exec = String sin_q }
    | DoubleQuotes double_q -> interpret_double_quotes double_q ctx ""
    | BraceExpansion bexp -> interpret_brace_expansion ctx bexp
    | ParameterExpansion pexp -> interpret_param_expansion pexp ctx
    | ArithmeticExpansion aexp ->
      interpret_expression aexp ctx
      >>= fun nctx -> return { ctx with last_exec = nctx.last_exec }
    | CommandSubstitution cmdsb ->
      interpret_command_substtitution cmdsb ctx
      >>= fun nctx -> return { ctx with last_exec = nctx.last_exec }
    | _ -> return ctx

  (** Interprets redirections *)
  and interpret_redirection ctx =
    let redirect file flags descr =
      return { ctx with chs = IMap.add descr Unix.(openfile file flags 0o640) ctx.chs }
    in
    function
    | hd :: tl ->
      (match hd with
       | RedirectInput (Const (String in_redir)) -> redirect in_redir [ O_RDONLY ] 0
       | RedirectOutput (Const (String out_redir)) ->
         redirect out_redir [ O_CREAT; O_WRONLY ] 1
       | AppendRedirOutput (Const (String app_redir)) ->
         redirect app_redir [ O_CREAT; O_WRONLY; O_APPEND ] 1
       | _ -> return ctx)
      >>= fun new_ctx -> interpret_redirection new_ctx tl
    | [] -> return ctx

  (** Interprets command substitution *)
  and interpret_command_substtitution cmd ctx =
    let read_end, write_end = Unix.pipe () in
    let oldch = ctx.chs in
    let rstrip = Base.String.rstrip in
    let pctx = { ctx with chs = IMap.add 1 write_end ctx.chs } in
    interpret_command pctx cmd
    >>= fun nctx ->
    Unix.close write_end;
    let res = In_channel.input_all (Unix.in_channel_of_descr read_end) in
    Unix.close read_end;
    return { nctx with last_exec = String (rstrip res); chs = oldch }

  (** Interprets bash brace expansion *)
  and interpret_brace_expansion ctx bexp =
    let beg =
      match bexp with
      | Const (String str) :: _ -> str
      | _ -> ""
    in
    let ending =
      match bexp with
      | _ :: Const (String str) :: _ -> str
      | _ -> ""
    in
    let rec interpret_bexp ctx bexp list =
      match bexp with
      | hd :: tl ->
        (match hd with
         | Const (String str) ->
           return (List.append list [ Format.sprintf "%s%s%s" beg str ending ])
           >>= fun list -> interpret_bexp ctx tl list
         | _ -> interpret_bexp ctx tl list)
      | [] -> return { ctx with last_exec = StringList list }
    in
    match bexp with
    | _ :: _ :: tl -> interpret_bexp ctx tl [] >>= fun ctx -> return ctx
    | _ -> return ctx

  (** Interprets bash "weak" quoting *)
  and interpret_double_quotes double_q ctx str =
    match double_q with
    | hd :: tl ->
      interpret_expression hd ctx
      >>= fun new_ctx ->
      matching_iconst_string new_ctx.last_exec
      >>= fun new_str -> interpret_double_quotes tl new_ctx (str ^ new_str)
    | _ -> return { ctx with last_exec = String str }

  (** Interprets var by name and index trying to replace the type where possible *)
  and interpret_variable ctx =
    let interpret_value = function
      | Int n -> return { ctx with last_exec = Int n }
      | Bool b -> return { ctx with last_exec = Bool b }
      | String s ->
        (match int_of_string_opt s with
         | Some num -> return { ctx with last_exec = Int num }
         | None ->
           (match bool_of_string_opt s with
            | Some b -> return { ctx with last_exec = Bool b }
            | None -> return { ctx with last_exec = String s }))
      | _ -> return ctx
    in
    function
    | name, i ->
      (match get_var name ctx with
       | Some v ->
         (match ConstMap.find_opt i v with
          | Some v -> interpret_value v
          | None ->
            (match ConstMap.find_opt "" v with
             | Some v ->
               (match v with
                | StringList ls ->
                  (match int_of_string_opt i with
                   | Some i ->
                     if i > List.length ls
                     then return { ctx with last_exec = String "" }
                     else return (List.nth ls i) >>= fun v -> interpret_value (String v)
                   | None ->
                     return (List.nth ls 0) >>= fun v -> interpret_value (String v))
                | _ -> return ctx)
             | None -> return ctx))
       | None -> return ctx)

  (** Interperts bash parameter expansion *)
  and interpret_param_expansion pexp ctx =
    let var_to_str var =
      interpret_variable ctx var
      >>= fun new_ctx -> matching_iconst_string new_ctx.last_exec
    in
    let removing reg_size ~all ~beg var pat s =
      var_to_str var
      >>= fun str ->
      match str, Base.String.for_all str ~f:(fun c -> c = '*') with
      | "", true -> return { ctx with last_exec = String "" }
      | _ ->
        let cond g =
          let c =
            match beg with
            | None -> true
            | Some true -> Re.Group.start g 0 = 0
            | Some false -> Re.Group.stop g 0 = String.length str
          in
          if c then s else Re.Group.get g 0
        in
        let re = Re.Glob.glob pat |> reg_size |> Re.compile in
        return
          { ctx with
            last_exec = String (Re.replace ~all:(all || beg = Some false) re ~f:cond str)
          }
    in
    let turn_regexp pat = return (Str.regexp pat) in
    let return_param icnst = return { ctx with last_exec = icnst } in
    match pexp with
    | Offset (var, Const (Int offset)) ->
      var_to_str var
      >>= fun s -> return_param (String (String.sub s offset (String.length s - offset)))
    | OffsetLen (var, Const (Int offset), Const (Int len)) ->
      var_to_str var >>= fun s -> return_param (String (String.sub s offset len))
    | Length var -> var_to_str var >>= fun s -> return_param (Int (String.length s))
    | ReplFirst (var, Const (String pat), Const (String str)) ->
      var_to_str var
      >>= fun s ->
      turn_regexp pat >>= fun pat -> return_param (String (Str.replace_first pat str s))
    | ReplAll (var, Const (String pat), Const (String str)) ->
      var_to_str var
      >>= fun s ->
      turn_regexp pat >>= fun pat -> return_param (String (Str.global_replace pat str s))
    | ReplBeg (var, Const (String pat), Const (String str)) ->
      let* s = var_to_str var in
      let* re = turn_regexp pat in
      if Str.string_match re s 0
      then return_param (String (Str.replace_first re str s))
      else return_param (String s)
    | ReplEnd (var, Const (String pat), Const (String str)) ->
      let* str_to_rep = var_to_str var in
      let* re = turn_regexp pat in
      let* pos = return (Str.search_backward re str_to_rep (String.length str_to_rep)) in
      if pos >= String.length str_to_rep - String.length pat
      then
        return_param
          (String
             (String.sub str_to_rep 0 pos
             ^ Str.replace_first
                 re
                 str
                 (String.sub str_to_rep pos (String.length str_to_rep - pos))))
      else return_param (String str_to_rep)
    | RemShortFromBeg (var, Const (String pat)) ->
      removing Re.shortest ~all:false ~beg:(Some true) var pat ""
    | RemLargFromBeg (var, Const (String pat)) ->
      removing Re.longest ~all:false ~beg:(Some true) var pat ""
    | RemShortFromEnd (var, Const (String pat)) ->
      removing Re.shortest ~all:false ~beg:(Some false) var pat ""
    | RemLargFromEnd (var, Const (String pat)) ->
      removing Re.longest ~all:false ~beg:(Some false) var pat ""
    | UseDefValue ((name, index), Const (String word)) ->
      (match get_var name ctx with
       | Some v ->
         (match ConstMap.find_opt index v with
          | Some _ -> interpret_variable ctx (name, index)
          | None -> return_param (String word))
       | None -> return_param (String word))
    | SetDefValue ((name, index), Const (String word)) ->
      (match get_var name ctx with
       | Some v ->
         (match ConstMap.find_opt index v with
          | Some _ ->
            interpret_variable ctx (name, index)
            >>= fun new_ctx ->
            (match new_ctx.last_exec with
             | String "" -> return_param (String word)
             | _ -> return new_ctx)
          | None -> return_param (String word))
       | None -> return_param (String word))
    | UseAlterValue ((name, index), Const (String word)) ->
      (match get_var name ctx with
       | Some v ->
         (match ConstMap.find_opt index v with
          | Some _ ->
            interpret_variable ctx (name, index)
            >>= fun new_ctx ->
            (match new_ctx.last_exec with
             | String "" -> return new_ctx
             | _ -> return_param (String word))
          | None -> return ctx)
       | None -> return ctx)
    | _ -> return ctx

  (** Interperts arithmetic and comparison *)
  and interpret_binop operator ctx =
    let interpret_operator left right operator ctx =
      interpret_expression left ctx
      >>= fun lctx ->
      interpret_expression right lctx
      >>= fun rctx ->
      match lctx.last_exec, rctx.last_exec with
      | Int _, Int y when y = 0 && operator = ( / ) -> fail "Division by zero"
      | Int x, Int y -> return { ctx with last_exec = Int (operator x y) }
      | _, _ -> fail "Operation not supported for non-int values"
    in
    let interpret_compare left right operator ctx =
      interpret_expression left ctx
      >>= fun lctx ->
      interpret_expression right lctx
      >>= fun rctx ->
      match lctx.last_exec, rctx.last_exec with
      | Int x, Int y -> return { ctx with last_exec = Bool (operator x y) }
      | _, _ -> fail "Operation not supported for non-bool values"
    in
    match operator with
    | Add (left, right) -> interpret_operator left right ( + ) ctx
    | Sub (left, right) -> interpret_operator left right ( - ) ctx
    | Mul (left, right) -> interpret_operator left right ( * ) ctx
    | Div (left, right) -> interpret_operator left right ( / ) ctx
    | Eq (left, right) -> interpret_compare left right ( = ) ctx
    | Ne (left, right) -> interpret_compare left right ( != ) ctx
    | Gt (left, right) -> interpret_compare left right ( > ) ctx
    | Ge (left, right) -> interpret_compare left right ( >= ) ctx
    | Lt (left, right) -> interpret_compare left right ( < ) ctx
    | Le (left, right) -> interpret_compare left right ( <= ) ctx

  (* Compound *)

  (** Interprets conditional expressions *)
  and interpret_ifelse ctx cond bl else_bl =
    interpret_expression cond ctx
    >>= fun cond_ctx ->
    if cond_ctx.last_exec = Bool true
    then interpret_command_list cond_ctx bl
    else (
      match else_bl with
      | Some block -> interpret_command_list cond_ctx block
      | None -> return { cond_ctx with retcode = 0 })

  (** Interprets while loop *)
  and interpret_while ctx cond block =
    interpret_expression cond ctx
    >>= fun new_ctx ->
    match new_ctx.last_exec with
    | Bool true ->
      interpret_command_list new_ctx block >>= fun ctx -> interpret_while ctx cond block
    | Bool false -> return { ctx with retcode = 0 }
    | _ -> fail "Incorrect condition"

  (** Interprets for (expression form) *)
  and interpret_for ctx var cond incr block =
    let rec interpret_for_body ctx cond incr block =
      interpret_expression cond ctx
      >>= fun nctx ->
      match nctx.last_exec with
      | Bool true ->
        interpret_command_list nctx block
        >>= fun nectx ->
        interpret_expression incr nectx
        >>= fun new_ctx -> interpret_for_body new_ctx cond incr block
      | Bool false -> return { ctx with retcode = 0 }
      | _ -> fail "Incorrect condition"
    in
    match var with
    | Assignment ((name, index), value) ->
      set_var ctx name index value
      >>= fun new_ctx -> interpret_for_body new_ctx cond incr block
    | _ -> return ctx

  (** Interprets for (list form) *)
  and interpret_for_in ctx name index list block =
    match list with
    | hd :: tl ->
      set_var ctx name index hd
      >>= fun new_ctx ->
      interpret_command_list new_ctx block
      >>= fun nctx -> interpret_for_in nctx name index tl block
    | [] -> return ctx

  (** Interprets bash case *)
  and interpret_case ctx variable list =
    let to_string = function
      | Int i -> return (string_of_int i)
      | Bool b -> return (string_of_bool b)
      | String s -> return s
      | _ -> fail "Wrong pattern"
    in
    let rec interpret_pattern ctx = function
      | hd :: tl ->
        interpret_expression hd ctx
        >>= fun pat_ctx ->
        if to_string pat_ctx.last_exec = to_string ctx.last_exec
        then return hd
        else interpret_pattern ctx tl
      | [] -> return (Const (String ""))
    in
    match variable with
    | Var var ->
      interpret_variable ctx var
      >>= fun new_ctx ->
      (match list with
       | hd :: tl ->
         (match hd with
          | pattern, block ->
            interpret_pattern new_ctx pattern
            >>= (function
            | Const (String "") -> interpret_case new_ctx variable tl
            | _ -> interpret_command_list new_ctx block))
       | [] -> return ctx)
    | _ -> return ctx

  and interpret_compound ctx = function
    | IfElse (condition, block, else_block) ->
      interpret_ifelse ctx condition block else_block
    | While (condition, block) -> interpret_while ctx condition block
    | For (var, cond, incr, block) -> interpret_for ctx var cond incr block
    | ForIn (Var (name, index), list, block) -> interpret_for_in ctx name index list block
    | Case (expr, list) -> interpret_case ctx expr list
    | _ -> return ctx

  (* Commands *)

  and interpret_var_assignment ctx = function
    | SimpleVar ((var, index), value) -> set_var ctx var index value
    | Array (name, value) -> set_array ctx name value []
    | AssociativeArray (name, value) -> set_associative_array ctx name value []

  and interpret_simple_command ctx name args =
    match FuncMap.find_opt name ctx.functions with
    | Some _ -> interpret_func ctx name args
    | None -> interpret_bash_command ctx name args

  (** Interprets shell commands *)
  and interpret_bash_command ctx name arguments =
    let open Unix in
    let interpret_program program args ctx =
      (* Replacing myself by a program*)
      let dup2pipes n fd =
        match n with
        | 0 -> dup2 fd stdin
        | 1 -> dup2 fd stdout
        | 2 -> dup2 fd stderr
        | _ -> ()
      in
      let interpret_p p args =
        let () = IMap.iter dup2pipes ctx.chs in
        try execvpe p (Array.of_list args) (environment ()) with
        | Unix_error (ENOENT, _, _) ->
          print_endline (List.hd args ^ ": command not found");
          exit 127
        | Unix_error (ENOEXEC, _, _) | Unix_error (EUNKNOWNERR 26, _, _) ->
          print_endline (List.hd args ^ ": command not executable");
          exit 126
      in
      (* Waiting for execution *)
      let rec wait_process pid =
        let return_rc = function
          | WEXITED r -> return { ctx with retcode = r }
          | WSIGNALED r | WSTOPPED r -> return { ctx with retcode = 128 + r }
        in
        let kill_pid pid =
          try kill pid Sys.sigint with
          | Unix_error (EACCES, _, _) -> print_endline "Cannot interrupt current process"
        in
        match Unix.waitpid [] pid with
        | _, status -> return_rc status
        | exception Sys.Break ->
          kill_pid pid;
          wait_process pid
      in
      (* Forking *)
      match fork () with
      | 0 -> interpret_p program args
      | pid ->
        Sys.catch_break true;
        let res = wait_process pid in
        Sys.catch_break false;
        res
    in
    let rec interpret_sl sl str =
      match sl with
      | hd :: tl -> interpret_sl tl (Format.sprintf "%s%s " str hd)
      | [] -> str
    in
    let to_string = function
      | Int i -> string_of_int i
      | Bool b -> string_of_bool b
      | String s -> s
      | StringList sl -> interpret_sl sl ""
    in
    let oldchs = ctx.chs in
    let rec interpret_args ctx args str_args redir_args =
      match args with
      | hd :: tl ->
        (match hd with
         | Redirect redir ->
           return (List.append redir_args [ redir ])
           >>= fun redirects -> interpret_args ctx tl str_args redirects
         | _ ->
           interpret_expression hd ctx
           >>= fun new_ctx ->
           return new_ctx.last_exec
           >>= fun ic ->
           return (to_string ic)
           >>= fun str ->
           (interpret_args new_ctx tl (Array.append str_args [| str |])) redir_args)
      | [] -> return (str_args, redir_args)
    in
    interpret_args ctx arguments [||] []
    >>= fun (arg_arr, redir_list) ->
    interpret_redirection ctx redir_list
    >>= fun new_ctx ->
    interpret_program name (name :: Array.to_list arg_arr) new_ctx
    >>= fun nctx -> return { nctx with chs = oldchs }

  (** Interprets user function *)
  and interpret_func ctx name args =
    let args_length = List.length args in
    let rec interpret_args ctx = function
      | hd :: tl ->
        set_var ctx (string_of_int (args_length - List.length tl)) "0" hd
        >>= fun new_ctx -> interpret_args new_ctx tl
      | [] -> return ctx
    in
    match FuncMap.find_opt name ctx.functions with
    | Some block ->
      interpret_args ctx args >>= fun new_ctx -> interpret_command_list new_ctx block
    | None -> return ctx

  and interpret_command ctx cmd =
    let interpret_args = function
      | Bool s -> return (string_of_bool s) >>= fun s -> return (s, [])
      | Int s -> return (string_of_int s) >>= fun s -> return (s, [])
      | StringList sl ->
        (match sl with
         | hd :: tl -> return (hd, tl)
         | [] -> return ("", []))
      | String s ->
        return (Str.split (Str.regexp " ") s)
        >>= (function
        | hd :: tl -> return (hd, tl)
        | [] -> return ("", []))
    in
    let rec expr_of_str str_list help_list =
      match str_list with
      | hd :: tl ->
        return (Const (String hd))
        >>= fun smth ->
        return (List.append help_list [ smth ])
        >>= fun new_list -> expr_of_str tl new_list
      | [] -> return help_list
    in
    match cmd with
    | SimpleCommand (name, args) -> interpret_simple_command ctx name args
    | Compound compound -> interpret_compound ctx compound
    | VarAssignment var -> interpret_var_assignment ctx var
    | Expression expr ->
      interpret_expression expr ctx
      >>= fun new_ctx ->
      interpret_args new_ctx.last_exec
      >>= fun (name, args) ->
      expr_of_str args [] >>= fun args -> interpret_simple_command new_ctx name args

  (** Interprets pipeline, connecting stdin and stdout *)
  and interpret_pipe ctx pipe =
    let rec process_pipe cl_read ctx cmd = function
      | hd :: tl ->
        let read_end, write_end = Unix.pipe () in
        interpret_command { ctx with chs = IMap.add 1 write_end ctx.chs } cmd
        >>= fun _ ->
        if cl_read then Unix.close (IMap.find 0 ctx.chs);
        Unix.close write_end;
        process_pipe true { ctx with chs = IMap.add 0 read_end ctx.chs } hd tl
      | [] ->
        interpret_command ctx cmd
        >>= fun nctx ->
        if cl_read then Unix.close (IMap.find 0 ctx.chs);
        return nctx.retcode
    in
    match pipe with
    | cmd :: [] -> interpret_command ctx cmd
    | cmd :: rest ->
      process_pipe false ctx cmd rest >>= fun retcode -> return { ctx with retcode }
    | [] -> fail "More than one command should be in a pipe"

  (** Interprets and-pipeline (continues only if the left side is true) *)
  and interpret_and_pipes ctx = function
    | hd :: tl ->
      (match hd with
       | Pipe pipe ->
         interpret_pipe ctx pipe
         >>= fun ctx -> if ctx.retcode = 0 then interpret_and_pipes ctx tl else return ctx
       | _ -> return ctx)
    | [] -> return ctx

  (** Interprets or-pipeline (when the left side is true, pipeline finishes) *)
  and interpret_or_pipes ctx = function
    | hd :: tl ->
      (match hd with
       | Pipe pipe ->
         interpret_pipe ctx pipe
         >>= fun ctx -> if ctx.retcode <> 0 then interpret_or_pipes ctx tl else return ctx
       | _ -> return ctx)
    | [] -> return ctx

  (** Interprets command list, command after command *)
  and interpret_command_list ctx = function
    | hd :: tl ->
      interpret_command ctx hd >>= fun new_ctx -> interpret_command_list new_ctx tl
    | [] -> return ctx

  and interpret_pipeline ctx = function
    | Pipe pipe -> interpret_pipe ctx pipe
    | AndList andpipe -> interpret_and_pipes ctx andpipe
    | OrList orpipe -> interpret_or_pipes ctx orpipe
    | CommandList command_list -> interpret_command_list ctx command_list

  (** Interprets declarations (pipelines or user functions) *)
  and interpret_declaration ctx = function
    | Pipeline pipe -> interpret_pipeline ctx pipe
    | Funcn (name, block) -> set_funcn name block ctx

  (** Interprets all script *)
  and interpret_bash ctx = function
    | [] -> return ctx
    | h :: t -> interpret_declaration ctx h >>= fun new_ctx -> interpret_bash new_ctx t
  ;;
end
