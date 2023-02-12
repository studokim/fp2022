(** Copyright 2022-2023, Ilya Pankratov *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let std_lib =
  {|
  let max a b = if a > b then a else b;;

  let min a b = if a < b then a else b;;

  let list_rev list =
    let rec helper acc l =
      match l with
      | [] -> acc
      | hd :: tl -> helper (hd :: acc) tl
      in helper [] list;;
  
  let rec list_map f list =
    match list with
    | [] -> []
    | hd :: tl -> (f hd) :: (list_map f tl);;
  
  let rec list_fold list acc f =
    let rec helper l acc = 
    match l with
    | [] -> acc
    | hd :: tl -> helper tl (f acc hd)
    in helper list acc;;

  let list_append l1 l2 =
    let rec helper l = 
    match l with
    | [] -> l2
    | hd :: tl -> hd :: (helper tl)
    in helper l1;;

  let list_concat list =
    let rec helper l = 
    match l with
    | [] -> []
    | hd :: tl -> list_append hd (helper tl)
    in helper list;;

  let list_filter list f =
    let rec helper l = 
    match l with
    | [] -> []
    | hd :: tl -> if f hd then hd :: (helper tl) else helper tl
    in helper list;;

  let list_nth_opt list number =
    let rec helper l n =
    match l with
    | [] -> `None
    | hd :: tl -> if (n + 1) = number then `Some hd else helper tl (n + 1)
    in helper list 0;;
  
  let list_find_opt f list =
    let rec helper l =
    match l with
    | [] -> `None
    | hd :: tl -> if f hd then `Some hd else helper tl
    in helper list;;

  let pair_fst (f, s) = f;;

  let pair_snd (f, s) = s;;

  let list_assoc_opt el list =
    let rec helper l =
      match l with
      | [] -> `None
      | hd :: tl -> (match hd with | (f, s) -> 
        if el = f then `Some s else (helper tl))
      in helper list;;

  let list_split list =
    let rec helper l (f1, s1) = 
      match l with
        | [] -> let f1 = list_rev f1 in 
                let s1 = list_rev s1 in 
                (f1, s1)
        | hd :: tl -> 
          (match hd with 
          | (a, b) -> 
          let f1 = a :: f1 in 
          let f2 = b :: s1 in 
      helper tl (f1, f2))
    in helper list ([], [])
  
|}
;;
