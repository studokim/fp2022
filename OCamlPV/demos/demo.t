Plus
  $ ./demo.exe <<- EOF
  > let x = 2 + 40
  > EOF
  - : int = 42
Product
  $ ./demo.exe <<- EOF
  > 7 + 77
  > EOF
  - : int = 84
Arithmetic expression
  $ ./demo.exe <<- EOF
  > (1 + 2) * (10 - 5) / 5
  > EOF
  - : int = 3
Sum
  $ ./demo.exe <<- EOF
  > let sum x y = x + y in sum 2 2
  > EOF
  - : int = 4
Apply
  $ ./demo.exe <<- EOF
  > let apply f x = f x
  > EOF
  - : (('a -> 'b) -> ('a -> 'b)) = <fun>
Factorial
  $ ./demo.exe <<- EOF
  > let rec fact n = if n = 1 then 1 else n * (fact (n - 1))
  > in fact 5
  > EOF
  - : int = 120
Fibonacci
  $ ./demo.exe <<- EOF
  > let rec fib n = if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2));;
  > let result = fib 5
  > EOF
  - : int = 8
Sum of the first n elements
  $ ./demo.exe <<- EOF
  > let rec sumn n = if n = 1 then 1 else n + (sumn (n - 1));;
  > let result = sumn 100
  - : int = 5050
List.Rev
  $ ./demo.exe <<- EOF
  > let list_rev list =
  >   let rec helper acc l =
  >     match l with
  >       | [] -> acc
  >       | hd :: tl -> helper (hd :: acc) tl
  > in helper [] list;;
  > let result = list_rev [1; 2; 3; 4; 5]
  > EOF
  - : int list = [5; 4; 3; 2; 1]
List.Map
  $ ./demo.exe <<- EOF
  > let rec list_map f list =
  >     match list with
  >       | [] -> []
  >       | hd :: tl -> (f hd) :: (list_map f tl);;
  > let result = list_map (fun x -> x * x) [1; 2; 3; 4; 5]
  > EOF
  - : int list = [1; 4; 9; 16; 25]
List.Fold
  $ ./demo.exe <<- EOF
  > let rec list_fold list acc f =
  >   let rec helper l acc =
  >     match l with
  >       | [] -> acc
  >       | hd :: tl -> helper tl (f acc hd)
  >   in helper list acc;;
  > let result = list_fold [1; 2; 3; 4; 5] 0 (fun acc el -> acc + el)
  > EOF
  - : int = 15
List.append
  $ ./demo.exe <<- EOF
  > let list_append l1 l2 =
  >   let rec helper l =
  >     match l with
  >       | [] -> l2
  >       | hd :: tl -> hd :: (helper tl)
  >   in helper l1;;
  > let result = list_append [1; 2; 3] [4; 5; 6]
  > EOF
  - : int list = [1; 2; 3; 4; 5; 6]
List.concat
  $ ./demo.exe <<- EOF
  > let list_concat list =
  >   let rec concat2 l1 l2 =
  >     match l1 with
  >       | [] -> l2
  >       | hd :: tl -> hd :: (concat2 tl l2) in
  >   let rec helper l =
  >     match l with
  >       | [] -> []
  >       | hd :: tl -> concat2 hd (helper tl)
  >   in helper list;;
  > let result = list_concat [[1; 2; 3]; [4; 5; 6]]
  > EOF
  - : int list = [1; 2; 3; 4; 5; 6]
List.filter
  $ ./demo.exe <<- EOF
  > let list_filter list f =
  >   let rec helper l =
  >     match l with
  >       | [] -> []
  >       | hd :: tl -> if (f hd) then hd :: (helper tl) else helper tl
  >   in helper list;;
  > let result = list_filter [1; 2; 3; 4; 5; 6; 7] (fun x -> x > 5)
  > EOF
  - : int list = [6; 7]
List.nth_opt
  $ ./demo.exe <<- EOF
  > let nth_opt list number =
  >   let rec helper l n =
  >     match l with
  >     | [] -> \`None
  >     | hd :: tl -> if (n + 1) = number then \`Some hd else helper tl (n + 1)
  >   in
  >   helper list 0;;
  > let res = nth_opt [1;2;3;4;5] 3
  > EOF
  - : [> `None | `Some of int ] = `Some (3)
List.find_opt
  $ ./demo.exe <<- EOF
  > let find_opt f list =
  >   let rec helper l =
  >     match l with
  >     | [] -> \`None
  >     | hd :: tl -> if f hd then \`Some hd else helper tl
  >   in
  >   helper list;;
  > let res = find_opt (fun x -> x * x = 2 * x) [1;2;3;4;5]
  > EOF
  - : [> `None | `Some of int ] = `Some (2)
List.assoc_opt
  $ ./demo.exe <<- EOF
  > let assoc_opt el list =
  >   let rec helper l =
  >     match l with
  >     | [] -> \`None
  >     | hd :: tl -> (match hd with | (f, s) ->
  >         if el = f then \`Some s else (helper tl))
  >   in
  >   helper list;;
  > let res = assoc_opt "i-pankrat" [("expression", 1); ("interpret", 2); ("ocaml", 3); ("spbu", 4); ("kakadu", 5); ("i-pankrat", 6)]
  > EOF
  - : [> `None | `Some of int ] = `Some (6)
List.rev_split
  $ ./demo.exe <<- EOF
  > let rev_split list =
  >   let rec helper l (f1, s1) =
  >     match l with
  >     | [] -> (f1, s1)
  >     | hd :: tl ->
  >       (match hd with
  >         | (a, b) ->
  >         let f1 = a :: f1 in
  >         let f2 = b :: s1 in helper tl (f1, f2))
  >   in helper list ([], []);;
  > let res = rev_split [("expression", 1); ("interpret", 2); ("ocaml", 3); ("spbu", 4); ("kakadu", 5); ("i-pankrat", 6)]
  > EOF
  - : (string list * int list) = (["i-pankrat"; "kakadu"; "spbu"; "ocaml"; "interpret"; "expression"], [6; 5; 4; 3; 2; 1])
transform_res
  $ ./demo.exe <<- EOF
  > let transform_res res =
  >   match res with
  >     | \`None -> \`Error "Failed to get the result"
  >     | \`Some x -> \`Ok x
  > EOF
  - : ([< `Some of 'a | `None ] -> [> `Error of string | `Ok of 'a ]) = <fun>
fst
  $ ./demo.exe <<- EOF
  > let fst (f, s) = f;;
  > let result = fst ("Dmitry", "Kosarev")
  > EOF
  - : string = "Dmitry"
snd
  $ ./demo.exe <<- EOF
  > let snd (f, s) = s;;
  > let result = snd ("Dmitry", "Kosarev")
  > EOF
  - : string = "Kosarev"
Wildcard
  $ ./demo.exe <<- EOF
  > let f x k = match x with
  >     | \`None -> \`Error "Failed to get the result"
  >     | x -> k x
  > EOF
  - : ([> `None ] -> (([> `None ] -> [> `Error of string ]) -> [> `Error of string ])) = <fun>
  $ ./demo.exe <<- EOF
  > let f x k = match x with
  >     | \`None -> \`Error "Failed to get the result"
  >     | _ -> k x
  > EOF
  - : ([> `None ] -> (([> `None ] -> [> `Error of string ]) -> [> `Error of string ])) = <fun>
  $ ./demo.exe <<- EOF
  > let f x k = match x with
  >     | \`None -> \`Error "Failed to get the result"
  >     | _ -> k x;;
  > f \`Something (fun x -> match x with | _ -> \`Ok)
  > EOF
  - : [> `Error of string | `Ok ] = `Ok ()
