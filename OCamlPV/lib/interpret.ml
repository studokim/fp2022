(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Base

module type FailMonad = sig
  include Base.Monad.S2

  val run : ('a, 'e) t -> ok:('a -> ('b, 'e) t) -> err:('e -> ('b, 'e) t) -> ('b, 'e) t
  val fail : 'e -> ('a, 'e) t
  val ( let* ) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
end

type ierror =
  [ `DivisionByZero
  | `UnboundValue of string
  | `TypeMismatch
  | `UnsupportedOperation
  | `PatternMismatch
  | `EmptyInput
  | `Unreachable
  ]

let pp_ierror ppf : ierror -> unit =
  let open Format in
  function
  | `DivisionByZero -> fprintf ppf "Runtime error: division by zero"
  | `UnboundValue s -> fprintf ppf "Runtime error: unbind variable %s" s
  | `TypeMismatch -> fprintf ppf "Runtime error: "
  | `UnsupportedOperation -> fprintf ppf "Runtime error: unsupported operation"
  | `PatternMismatch -> fprintf ppf "Runtime error: pattern mismatch"
  | `EmptyInput -> fprintf ppf "Runtime error: empty input to interpret"
  | `Unreachable ->
    fprintf
      ppf
      "Runtime error: oops... This was never supposed to happen. Report the bug, please."
;;

type 'a binop =
  | EmptyBinOp of bin_op
  | PartialBinOp of 'a * bin_op
[@@deriving show { with_path = false }]

type value =
  | VInt of int
  | VBool of bool
  | VString of string
  | VTuple of value list
  | VList of value list
  | VFun of pattern * expr * (id * value) list
  | VBinOp of value binop
  | VPolyVariant of id * value list
  | VUnit
  | VNil
[@@deriving show { with_path = false }]

type environment = (id, value, String.comparator_witness) Map.t

(** value type constructors *)

let cvint i = VInt i
let cvbool b = VBool b
let cvstring s = VString s
let cvtuple values = VTuple values
let cvlist values = VList values
let cvfun p e subst = VFun (p, e, subst)
let cvbinop op = VBinOp op
let cvpolyvariant id values = VPolyVariant (id, values)
let cvunit = VUnit
let cvnil = VNil

module Env (M : FailMonad) = struct
  open M

  let empty = Base.Map.empty (module Base.String)

  let extend_by_one id value env =
    match Map.add env ~key:id ~data:value with
    | `Ok env -> env
    | `Duplicate ->
      Map.mapi env ~f:(fun ~key:name ~data:old_value ->
        if Poly.( = ) id name then value else old_value)
  ;;

  let extend env bindings =
    return
    @@ List.fold ~f:(fun env (id, value) -> extend_by_one id value env) bindings ~init:env
  ;;

  let find map key =
    match Map.find map key with
    | None -> fail (`UnboundValue key)
    | Some value -> return value
  ;;
end

module Interpret (M : FailMonad) : sig
  val run : ?env:environment -> expr list -> (environment * value, ierror) M.t
end = struct
  open M
  open Env (M)

  let eval_binop arg =
    let eval_comparison = function
      | Eq -> return Poly.( = )
      | Neq -> return Poly.( <> )
      | Gt -> return Poly.( > )
      | Lt -> return Poly.( < )
      | Gtq -> return Poly.( >= )
      | Ltq -> return Poly.( <= )
      | _ -> fail `Unreachable
    in
    function
    | EmptyBinOp op ->
      (match op, arg with
       | Plus, VInt _
       | Minus, VInt _
       | Mult, VInt _
       | Divide, VInt _
       | Mod, VInt _
       | And, VBool _
       | Or, VBool _
       | ConsConcat, _
       | Eq, _
       | Neq, _
       | Gt, _
       | Lt, _
       | Gtq, _
       | Ltq, _ -> return @@ cvbinop @@ PartialBinOp (arg, op)
       | _ -> fail `Unreachable)
    | PartialBinOp (farg, op) ->
      (match op, farg, arg with
       | Plus, VInt i1, VInt i2 -> return @@ cvint (i1 + i2)
       | Minus, VInt i1, VInt i2 -> return @@ cvint (i1 - i2)
       | Mult, VInt i1, VInt i2 -> return @@ cvint (i1 * i2)
       | Divide, VInt _, VInt i2 when i2 = 0 -> fail `DivisionByZero
       | Divide, VInt i1, VInt i2 -> return @@ cvint (i1 / i2)
       | Mod, VInt _, VInt i2 when i2 = 0 -> fail `DivisionByZero
       | Mod, VInt i1, VInt i2 -> return @@ cvint (i1 % i2)
       | And, VBool b1, VBool b2 -> return @@ cvbool (b1 && b2)
       | Or, VBool b1, VBool b2 -> return @@ cvbool (b1 || b2)
       | ConsConcat, _, VList list -> return @@ cvlist (farg :: list)
       | ConsConcat, _, VNil -> return @@ VList (farg :: [])
       | op, VInt i1, VInt i2 ->
         let* comp = eval_comparison op in
         return @@ cvbool @@ comp i1 i2
       | op, VBool b1, VBool b2 ->
         let* comp = eval_comparison op in
         return @@ cvbool @@ comp b1 b2
       | op, VString s1, VString s2 ->
         let* comp = eval_comparison op in
         return @@ cvbool @@ comp s1 s2
       | _ -> fail `Unreachable)
  ;;

  let rec eval_pattern =
    let eval_lists pt vt =
      let pat =
        List.fold2 pt vt ~init:(return []) ~f:(fun acc p v ->
          let* evaled = eval_pattern (p, v) in
          let* acc = acc in
          return (evaled @ acc))
      in
      match pat with
      | Ok res -> res
      | Unequal_lengths -> fail `PatternMismatch
    in
    function
    | PWild, _ -> return []
    | PVar name, value -> return [ name, value ]
    | PConst const, cvalue ->
      (match const, cvalue with
       | CBool e1, VBool e2 when Poly.( = ) e1 e2 -> return []
       | CInt e1, VInt e2 when Poly.( = ) e1 e2 -> return []
       | CString e1, VString e2 when Poly.( = ) e1 e2 -> return []
       | CUnit, VUnit -> return []
       | CNil, VList v ->
         (match v with
          | [] -> return []
          | _ -> fail `PatternMismatch)
       | _ -> fail `PatternMismatch)
    | (PCons _ as pl), VList vl ->
      (match pl, vl with
       | PCons (h, t), hd :: tl ->
         let* evaledhd = eval_pattern (h, hd) in
         let* evaledtl = eval_pattern (t, VList tl) in
         return @@ evaledhd @ evaledtl
       | _ -> fail `PatternMismatch)
    | PTuple pt, VTuple vt -> eval_lists pt vt
    | PPolyVariant (constr1, args1), VPolyVariant (constr2, args2) ->
      if Poly.( = ) constr1 constr2 then eval_lists args1 args2 else fail `PatternMismatch
    | _ -> fail `PatternMismatch
  ;;

  let rec eval expr env substs : (value, ierror) t =
    let eval_list l =
      let* list =
        List.fold l ~init:(return []) ~f:(fun l e ->
          let* l = l in
          let* evaled = eval e env [] in
          return @@ (evaled :: l))
      in
      return (List.rev list)
    in
    match expr with
    | EConst const ->
      (match const with
       | CInt i -> return @@ cvint i
       | CBool b -> return @@ cvbool b
       | CString s -> return @@ cvstring s
       | CNil -> return cvnil
       | CUnit -> return cvunit)
    | EFun (pat, expr) -> return @@ cvfun pat expr substs
    | EVar var -> find env var
    | EApply (func, arg) ->
      let* evaled_fun = eval func env [] in
      let* evaled_arg = eval arg env [] in
      (match evaled_fun with
       | VFun (pat, expr, substs) ->
         let* evaled_pat = eval_pattern (pat, evaled_arg) in
         let* env = extend env substs in
         let* env = extend env evaled_pat in
         eval expr env (evaled_pat @ substs)
       | VBinOp op -> eval_binop evaled_arg op
       | _ -> fail `Unreachable)
    | EBinOp op -> return @@ cvbinop @@ EmptyBinOp op
    | EIfThenElse (c, t, e) ->
      let* evaled_condtion = eval c env [] in
      (match evaled_condtion with
       | VBool res -> if res then eval t env [] else eval e env []
       | _ -> fail `Unreachable)
    | EMatch (matched, patterns) ->
      let* evaled_match = eval matched env [] in
      let rec eval_match_expr = function
        | [] -> fail `PatternMismatch
        | (p, e) :: tl ->
          let res = eval_pattern (p, evaled_match) in
          run
            res
            ~ok:(fun res ->
              let* env = extend env res in
              eval e env [])
            ~err:(fun _res -> eval_match_expr tl)
      in
      eval_match_expr patterns
    | ELet (_, expr) -> eval expr env []
    | ELetRec (_, expr) -> eval expr env []
    | ELetIn (name, expr1, expr2) ->
      let* evaled_expr1 = eval expr1 env [] in
      let* env = extend env [ name, evaled_expr1 ] in
      eval expr2 env []
    | ELetRecIn (name, expr1, expr2) ->
      let* evaled1 = eval expr1 env [] in
      let* env = extend env [ name, evaled1 ] in
      eval expr2 env []
    | EList (h, t) ->
      let* evaled = eval h env [] in
      let rec helper acc expr =
        match expr with
        | EConst CNil -> acc
        | EList (hd, tl) ->
          let* acc = acc in
          let* evaled = eval hd env [] in
          helper (return (evaled :: acc)) tl
        | _ ->
          let* acc = acc in
          let* evaled = eval expr env [] in
          return (evaled :: acc)
      in
      let* res = helper (return [ evaled ]) t in
      let res = cvlist (List.rev res) in
      return res
    | ETuple els ->
      let* vls = eval_list els in
      return (cvtuple vls)
    | EPolyVariant (id, exprs) ->
      let* vls = eval_list exprs in
      return (cvpolyvariant id vls)
  ;;

  let run_expr env expr : (environment * value, ierror) t =
    let* value = eval expr env [] in
    match expr with
    | ELet (name, _) | ELetIn (name, _, _) | ELetRec (name, _) | ELetRecIn (name, _, _) ->
      let* env = extend env [ name, value ] in
      return (env, value)
    | _ -> return (env, value)
  ;;

  let run ?(env : environment = empty) program : (environment * value, ierror) t =
    let rec helper env = function
      | [] -> fail `EmptyInput
      | hd :: [] ->
        let* env, value = run_expr env hd in
        return (env, value)
      | hd :: tl ->
        let* env, _ = run_expr env hd in
        helper env tl
    in
    helper env program
  ;;
end

module InterpretResult = Interpret (struct
  include Base.Result

  let run x ~ok ~err =
    match x with
    | Ok v -> ok v
    | Error e -> err e
  ;;

  let ( let* ) monad f = bind monad ~f
end)

let run = InterpretResult.run
let empty : environment = Base.Map.empty (module Base.String)

let run_test t =
  match run t with
  | Result.Ok (_, ty) -> Result.Ok ty
  | Result.Error e -> Result.Error e
;;

(* Tests for interpretator *)

(* (1 + 1) * (5 - 2) * (42 / 6) *)
let test =
  [ EApply
      ( EApply
          ( EBinOp Mult
          , EApply
              ( EApply
                  ( EBinOp Mult
                  , EApply (EApply (EBinOp Plus, EConst (CInt 1)), EConst (CInt 1)) )
              , EApply (EApply (EBinOp Minus, EConst (CInt 5)), EConst (CInt 2)) ) )
      , EApply (EApply (EBinOp Divide, EConst (CInt 42)), EConst (CInt 6)) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 42) -> true
  | _ -> false
;;

(* Increment function *)
let test =
  [ ELetIn
      ( "inc"
      , EFun (PVar "x", EApply (EApply (EBinOp Plus, EConst (CInt 1)), EVar "x"))
      , EApply (EVar "inc", EConst (CInt 5)) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 6) -> true
  | _ -> false
;;

(* Sum of two variables *)
let test =
  [ ELetIn
      ( "sum"
      , EFun (PVar "a", EFun (PVar "b", EApply (EApply (EBinOp Plus, EVar "a"), EVar "b")))
      , EApply (EApply (EVar "sum", EConst (CInt 2)), EConst (CInt 3)) )
  ]
;;

(* Sum of the first n natural numbers *)
let test =
  [ ELetRecIn
      ( "sumn"
      , EFun
          ( PVar "x"
          , EIfThenElse
              ( EApply (EApply (EBinOp Eq, EConst (CInt 1)), EVar "x")
              , EConst (CInt 1)
              , EApply
                  ( EApply (EBinOp Plus, EVar "x")
                  , EApply
                      ( EVar "sumn"
                      , EApply (EApply (EBinOp Minus, EVar "x"), EConst (CInt 1)) ) ) ) )
      , EApply (EVar "sumn", EConst (CInt 100)) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 5050) -> true
  | _ -> false
;;

(* Fibonacci function *)
let test =
  [ ELetRecIn
      ( "fib"
      , EFun
          ( PVar "n"
          , EIfThenElse
              ( EApply (EApply (EBinOp Gtq, EConst (CInt 1)), EVar "n")
              , EConst (CInt 1)
              , EApply
                  ( EApply
                      ( EBinOp Plus
                      , EApply
                          ( EVar "fib"
                          , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                  , EApply
                      ( EVar "fib"
                      , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 2)) ) ) ) )
      , EApply (EVar "fib", EConst (CInt 15)) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 987) -> true
  | _ -> false
;;

(* Factorial function *)
let test =
  [ ELetRecIn
      ( "fac"
      , EFun
          ( PVar "n"
          , EIfThenElse
              ( EApply (EApply (EBinOp Eq, EConst (CInt 1)), EVar "n")
              , EConst (CInt 1)
              , EApply
                  ( EApply
                      ( EBinOp Mult
                      , EApply
                          ( EVar "fac"
                          , EApply (EApply (EBinOp Minus, EVar "n"), EConst (CInt 1)) ) )
                  , EVar "n" ) ) )
      , EApply (EVar "fac", EConst (CInt 5)) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 120) -> true
  | _ -> false
;;

(* Sum of the list *)
let test =
  [ ELetIn
      ( "list_sum"
      , EFun
          ( PVar "list"
          , ELetRecIn
              ( "helper"
              , EFun
                  ( PVar "x"
                  , EFun
                      ( PVar "acc"
                      , EMatch
                          ( EVar "x"
                          , [ PConst CNil, EVar "acc"
                            ; ( PCons (PVar "head", PVar "tail")
                              , EApply
                                  ( EApply (EVar "helper", EVar "tail")
                                  , EApply (EApply (EBinOp Plus, EVar "acc"), EVar "head")
                                  ) )
                            ] ) ) )
              , EApply (EApply (EVar "helper", EVar "list"), EConst (CInt 0)) ) )
      , EApply
          ( EVar "list_sum"
          , EList
              ( EConst (CInt 1)
              , EList (EConst (CInt 2), EList (EConst (CInt 3), EConst CNil)) ) ) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 6) -> true
  | _ -> false
;;

(* List.map function *)
let test =
  [ ELetRecIn
      ( "list_map"
      , EFun
          ( PVar "f"
          , EFun
              ( PVar "list"
              , EMatch
                  ( EVar "list"
                  , [ PConst CNil, EConst CNil
                    ; ( PCons (PVar "head", PVar "tail")
                      , EApply
                          ( EApply (EBinOp ConsConcat, EApply (EVar "f", EVar "head"))
                          , EApply (EApply (EVar "list_map", EVar "f"), EVar "tail") ) )
                    ] ) ) )
      , EApply
          ( EApply
              ( EVar "list_map"
              , EFun (PVar "x", EApply (EApply (EBinOp Mult, EVar "x"), EConst (CInt 2)))
              )
          , EList
              ( EConst (CInt 1)
              , EList (EConst (CInt 2), EList (EConst (CInt 3), EConst CNil)) ) ) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VList [ VInt 2; VInt 4; VInt 6 ]) -> true
  | _ -> false
;;

(* List.fold function *)
let test =
  [ ELetRec
      ( "list_fold"
      , EFun
          ( PVar "list"
          , EFun
              ( PVar "acc"
              , EFun
                  ( PVar "f"
                  , EMatch
                      ( EVar "list"
                      , [ PConst CNil, EVar "acc"
                        ; ( PCons (PVar "head", PVar "tail")
                          , EApply
                              ( EApply
                                  ( EApply (EVar "list_fold", EVar "tail")
                                  , EApply (EApply (EVar "f", EVar "acc"), EVar "head") )
                              , EVar "f" ) )
                        ] ) ) ) ) )
  ; ELet
      ( "list_sum"
      , EApply
          ( EApply
              ( EApply
                  ( EVar "list_fold"
                  , EList
                      ( EConst (CInt 1)
                      , EList (EConst (CInt 2), EList (EConst (CInt 3), EConst CNil)) ) )
              , EConst (CInt 0) )
          , EFun
              ( PVar "accum"
              , EFun (PVar "x", EApply (EApply (EBinOp Plus, EVar "accum"), EVar "x")) )
          ) )
  ]
;;

let%test _ =
  match run_test test with
  | Base.Result.Ok (VInt 6) -> true
  | _ -> false
;;

(* List.nth_opt function *)
let test n =
  [ ELet
      ( "nth_opt"
      , EFun
          ( PVar "list"
          , EFun
              ( PVar "number"
              , ELetRecIn
                  ( "helper"
                  , EFun
                      ( PVar "l"
                      , EFun
                          ( PVar "n"
                          , EMatch
                              ( EVar "l"
                              , [ PConst CNil, EPolyVariant ("`None", [])
                                ; ( PCons (PVar "hd", PVar "tl")
                                  , EIfThenElse
                                      ( EApply
                                          ( EApply
                                              ( EBinOp Eq
                                              , EApply
                                                  ( EApply (EBinOp Plus, EVar "n")
                                                  , EConst (CInt 1) ) )
                                          , EVar "number" )
                                      , EPolyVariant ("`Some", [ EVar "hd" ])
                                      , EApply
                                          ( EApply (EVar "helper", EVar "tl")
                                          , EApply
                                              ( EApply (EBinOp Plus, EVar "n")
                                              , EConst (CInt 1) ) ) ) )
                                ] ) ) )
                  , EApply (EApply (EVar "helper", EVar "list"), EConst (CInt 0)) ) ) ) )
  ; ELet
      ( "res"
      , EApply
          ( EApply
              ( EVar "nth_opt"
              , EList
                  ( EConst (CInt 1)
                  , EList (EConst (CInt 2), EList (EConst (CInt 3), EConst CNil)) ) )
          , EConst (CInt n) ) )
  ]
;;

let%test _ =
  match run_test @@ test 2 with
  | Base.Result.Ok (VPolyVariant ("`Some", [ VInt 2 ])) -> true
  | _ -> false
;;

let%test _ =
  match run_test @@ test 6 with
  | Base.Result.Ok (VPolyVariant ("`None", [])) -> true
  | _ -> false
;;

(* List.find_opt function *)

let test n =
  [ ELet
      ( "find_opt"
      , EFun
          ( PVar "f"
          , EFun
              ( PVar "list"
              , ELetRecIn
                  ( "helper"
                  , EFun
                      ( PVar "l"
                      , EMatch
                          ( EVar "l"
                          , [ PConst CNil, EPolyVariant ("`None", [])
                            ; ( PCons (PVar "hd", PVar "tl")
                              , EIfThenElse
                                  ( EApply (EVar "f", EVar "hd")
                                  , EPolyVariant ("`Some", [ EVar "hd" ])
                                  , EApply (EVar "helper", EVar "tl") ) )
                            ] ) )
                  , EApply (EVar "helper", EVar "list") ) ) ) )
  ; ELet
      ( "res"
      , EApply
          ( EApply
              ( EVar "find_opt"
              , EFun (PVar "x", EApply (EApply (EBinOp Eq, EConst (CInt n)), EVar "x")) )
          , EList
              ( EConst (CInt 1)
              , EList (EConst (CInt 2), EList (EConst (CInt 3), EConst CNil)) ) ) )
  ]
;;

let%test _ =
  match run_test @@ test 3 with
  | Base.Result.Ok (VPolyVariant ("`Some", [ VInt 3 ])) -> true
  | _ -> false
;;

let%test _ =
  match run_test @@ test 6 with
  | Base.Result.Ok (VPolyVariant ("`None", [])) -> true
  | _ -> false
;;

let test name constr =
  [ ELet
      ( "transform_res"
      , EFun
          ( PVar "res"
          , EMatch
              ( EVar "res"
              , [ ( PPolyVariant ("`None", [])
                  , EPolyVariant
                      ("`Error", [ EConst (CString "Failed to get the result") ]) )
                ; PPolyVariant ("`Some", [ PVar "x" ]), EPolyVariant ("`Ok", [ EVar "x" ])
                ] ) ) )
  ; ELet ("res", EApply (EVar "transform_res", EPolyVariant (name, constr)))
  ]
;;

let%test _ =
  match run_test @@ test "`None" [] with
  | Base.Result.Ok (VPolyVariant ("`Error", [ VString "Failed to get the result" ])) ->
    true
  | _ -> false
;;

let%test _ =
  let constr = [ EConst (CString "Success") ] in
  match run_test @@ test "`Some" constr with
  | Base.Result.Ok (VPolyVariant ("`Ok", [ VString "Success" ])) -> true
  | _ -> false
;;
