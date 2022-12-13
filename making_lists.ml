let rec length_inner l acc =
  match l with
  [] -> acc
  | _ :: t -> length_inner t (acc + 1)

(* length : 'a list -> int *)
let length l = length_inner l 0

(* odds : 'a list -> 'a list *)
let rec odds l = 
  match l with
   h :: _ :: tt -> h :: odds tt
  | _ -> l

(* append: 'a list -> 'a list -> 'a list *)
let rec append a b = 
  match a with
  [] -> b
  | h :: t -> h :: append t b

(* rev : 'a list -> 'a list *)

let rec rev l =
  match l with
  [] -> []
  | h :: t -> rev t @ [h]

(* take : int -> 'a list -> 'a list *)

let rec take n l = 
  if n = 0 then []
  else 
    match l with
    [] -> []
  | h :: t -> h :: take (n - 1) t

(* drop : int -> 'a list -> 'a list *)

let rec drop n l = 
  if n = 0 then l 
  else match l with 
  [] -> []
  | _ :: t -> drop (n - 1) t
  

(* exercises *)

  (* evens : 'a list - > 'a list *)
let rec evens l = 
  match l with 
  [] -> []
  | [_] -> []
  | [_; x] -> [x]
  | _ :: h :: t -> h :: evens t

let rec count_true_inner l acc = 
  match l with
  [] -> acc
  | h :: t -> count_true_inner t (if h then (acc + 1) else acc)

  (* count_true : bool list -> int *)
let count_true l = 
  count_true_inner l 0

  (* build_palindrome : 'a list -> 'a list *)
let build_palindrome l = 
  l @ rev l

  (* check_palindrome : 'a list -> bool *)
let check_palindrome l = 
  l = rev l

let rec droplast_inner l acc = 
  match l with 
  [] | [_] -> acc
  | h :: t ->  droplast_inner t (acc @ [h])

  (* droplast : 'a list -> 'a list *)
let droplast l = 
  droplast_inner l []

  (* member : 'a -> 'a list -> bool *)
let rec member x l = 
  match l with
  [] -> false
  | h :: t -> if h = x then true else member x t

  (* make_set : 'a list -> 'a list *)
let rec make_set l = 
  match l with 
  [] -> []
  | h :: t -> if not (member h t) then h :: make_set t else make_set t

let rec rev_inner l acc = 
  match l with
  [] -> acc
  | h :: t -> rev_inner t (h :: acc)

  (* rev : 'a list -> 'a list *)
let rev l = 
  rev_inner l []

