(* factorial : int -> int *)
let factorial n = 
  let rec factorial_inner n acc = 
    match n with
    0 -> acc
    | _ -> factorial_inner (n - 1) (acc * n)
  in
    factorial_inner n 1

(* insert : 'a  -> 'a list -> 'a list *)
let rec insert x l =
  match l with 
  [] -> [x]
  | h :: t -> 
      if x < h 
      then x :: h :: t 
      else h :: insert x t

      (* sort : 'a list -> 'a list *)
let rec insertion_sort l = 
  match l with
  [] -> []
  | h :: t -> insert h ( sort t)

  (* merge : 'a list -> 'a list -> 'a list *)
let rec merge a b = 
  match (a, b) with
  ([], _) -> b
  | (a, []) -> a
  | (ha :: ta, hb :: tb) ->
      if ha <= hb 
      then ha :: merge ta (hb :: tb)
      else hb :: merge (ha :: ta) tb

      (* merge_sort : 'a list -> 'a list *)
let rec merge_sort l = 
  match l with
  [] -> []
  | [x] -> [x]
  | _ -> 
      let len = length l / 2 in
      let (left, right) = (take len l, drop len l) in
      merge (merge_sort left) (merge_sort right)

(* Exercises *)

(* insert : 'a -> 'a list -> 'a list *)
let rec insert x l =
  match l with
  [] -> [x]
  | h :: t -> if x >= h then x :: h :: t else h :: insert x t
  

(* rev_sort : 'a list -> 'a list *)
let rec rev_sort l = 
  match l with
  [] -> []
  | h :: t -> insert h (rev_sort t)

  (* is_sorted : 'a list -> bool *)
let rec is_sorted l = 
  match l with
  [] | [_] -> true
  | [x; y] -> x <= y
  | x :: h :: t ->  x <= h && is_sorted (h :: t)

  (* single_sort : 'a list -> 'a list *)
let rec single_sort l = 
  let rec single_sort_insert x l = 
    match l with
    [] -> [x]
  | h :: t -> 
      if x <= h 
      then x :: h :: t
      else h :: single_sort_insert x t
  in
    match l with
    [] -> []
  | [x] -> [x]
  | h :: t -> single_sort_insert h (single_sort t)