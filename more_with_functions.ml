(* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l = 
  match l with
  [] -> []
  | h :: t -> f h :: map f t

(* mapl : ('a -> 'b) -> 'a list list -> 'b list list *)
let mapl f l = map (map f) l

(* Exercises *) 

(* member : 'a -> 'a list -> bool *)
let rec member x l = 
  match l with
  [] -> false
  | h :: t ->
      if h = x
      then true
      else member x t

(* member_all : 'a -> 'a list list -> bool *)
let member_all x ls = 
  let rec all p l = 
    match l with
    [] -> true
  | h :: t -> 
      if p h 
      then all p t
      else false
  in
    all (member x) ls

(* mapll : ('a -> 'b) -> 'a list list list -> 'b list list list *)
let mapll f lss = map (mapl f) lss

(* take : n -> 'a list -> 'a list *)
let rec take n l = 
  match l with
  [] -> []
  | h :: t -> 
      if n = 0
      then []
      else h :: take (n - 1) t

(* truncate : int -> 'a list list -> 'a list list *)
let truncate n ls = 
  map (take n) ls

(* take_firsts : int ->  int list list -> int list list *)
let take_first default ls = 
  map (fun l -> if List.length l = 0 then [default] else take 1 l) ls
