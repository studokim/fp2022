Max
  $ ./demostdlib.exe <<- EOF
  > max 3 5
  > EOF
  - : int = 5
Min
  $ ./demostdlib.exe <<- EOF
  > min 3 5
  > EOF
  - : int = 3
list_rev
  $ ./demostdlib.exe <<- EOF
  > list_rev ["a"; "b"; "c"; "d"; "e"; "f"]
  > EOF
  - : string list = ["f"; "e"; "d"; "c"; "b"; "a"]
list_map
  $ ./demostdlib.exe <<- EOF
  > list_map (fun x -> 10 * x) [1; 2; 3; 4; 5; 6]
  > EOF
  - : int list = [10; 20; 30; 40; 50; 60]
list_fold
  $ ./demostdlib.exe <<- EOF
  > list_fold [1; 2; 3; 4; 5; 6] 1 (fun acc x -> acc * x)
  > EOF
  - : int = 720
list_append
  $ ./demostdlib.exe <<- EOF
  > list_append [1; 2; 3; 4; 5; 6] [7; 8; 9; 10; 11; 12]
  > EOF
  - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
list_concat
  $ ./demostdlib.exe <<- EOF
  > list_concat [[1; 2; 3; 4; 5; 6]; [7; 8; 9; 10; 11; 12]; [13; 14; 15; 16; 17]]
  > EOF
  - : int list = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17]
list_filter
  $ ./demostdlib.exe <<- EOF
  > list_filter [1; 2; 3; 4; 5; 6; 7; 8; 9; 10] (fun x -> (x % 2) = 0)
  > EOF
  - : int list = [2; 4; 6; 8; 10]
list_nth_opt
  $ ./demostdlib.exe <<- EOF
  > list_nth_opt ["F"; "E"; "D"; "C"; "B"; "A" ] 6
  > EOF
  - : [> `None | `Some of string ] = `Some ("A")
  $ ./demostdlib.exe <<- EOF
  > list_nth_opt ["F"; "E"; "D"; "C"; "B"; "A" ] 7
  > EOF
  - : [> `None | `Some of string ] = `None ()
list_find_opt
  $ ./demostdlib.exe <<- EOF
  > list_find_opt (fun x -> (x % 2) = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  > EOF
  - : [> `None | `Some of int ] = `Some (2)
  $ ./demostdlib.exe <<- EOF
  > list_find_opt (fun x -> (x % 11) = 0) [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
  > EOF
  - : [> `None | `Some of int ] = `None ()
pair_fst
  $ ./demostdlib.exe <<- EOF
  > pair_fst (["OCaml"; "FP"], ["C#"; "OOP"])
  > EOF
  - : string list = ["OCaml"; "FP"]
pair_snd
  $ ./demostdlib.exe <<- EOF
  > pair_snd (["C#"; "OOP"], ["OCaml"; "FP"])
  > EOF
  - : string list = ["OCaml"; "FP"]
list_assoc_opt
  $ ./demostdlib.exe <<- EOF
  > list_assoc_opt "OCaml" [("F#", "B"); ("OCaml", "A"); ("Scala", "C")]
  > EOF
  - : [> `None | `Some of string ] = `Some ("A")
list_assoc_opt
  $ ./demostdlib.exe <<- EOF
  > list_assoc_opt "C#" [("F#", "B"); ("OCaml", "A"); ("Scala", "C")]
  > EOF
  - : [> `None | `Some of string ] = `None ()
list_split
  $ ./demostdlib.exe <<- EOF
  > list_split [("F#", "B"); ("OCaml", "A"); ("Scala", "C")]
  > EOF
  - : (string list * string list) = (["F#"; "OCaml"; "Scala"], ["B"; "A"; "C"])
