(* double : int list -> int list *)
let rec double l = 
  match l with
  [] -> []
  | h :: t -> h * 2 :: double t

  (* evens : int list -> bool list *)
let rec evens l = 
  match l with 
  [] -> []
  | h :: t -> (h mod 2 = 0) :: evens t

  (* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l = 
  match l with
  [] -> []
  | h :: t -> f h :: map f t

(* halve : int -> int *)
let halve n = n / 2

(* is_even : int -> bool *)
let is_even n = n mod 2 = 0

(* odds : int list -> bool list *)
let odds l = map (fun n -> n mod 2 <> 0) l

(* greater : 'a -> 'a -> bool *)
let greater a b = a > b

(* msort : ('a -> 'a -> bool') ->  'a list -> 'a list *)
let rec msort comp l = 
  let rec merge a b = 
    match a, b with
    [], b -> b
  | a, [] -> a
  | ha :: ta, hb :: tb ->
      if comp ha hb
      then ha :: merge ta (hb :: tb)
      else hb :: merge (ha :: ta) tb
  in
  match l with
  [] -> []
  | [x] -> [x]
  | _ -> 
    let len = length l / 2 in
      let (left, right) = (take len l, drop len l) in
        merge (msort comp left) (msort comp right)


(* Exercises *)

(* calm_rec : char list -> char list *)
let rec calm_rec l = 
  match l with 
  [] -> []
  | c :: cs ->
      if c = '!'
      then '.' :: calm_rec cs
      else c :: calm_rec cs

(* calm : char list -> char list *)
let calm l = 
  map (fun c -> if c = '!' then '.' else c) l

(* clip : int -> int *)
let clip n = 
  if n < 1 
  then 1
  else if n > 10
      then 10
      else n

(* cliplist : int list -> int list *)
let cliplist l = map clip l

(* cliplist_anon : int list -> int list *)
let cliplist_anon l = map (fun n -> if n < 1 then 1 else if n > 10 then 10 else n) l

(* apply : ('a -> 'a) -> int -> 'a -> 'a *)
let rec apply f n x = 
  if n = 0
  then x
  else apply f (n - 1) (f x)

(* insertion_sort : ('a -> 'a -> bool) -> 'a list -> 'a list *)
let rec insertion_sort comp l = 
  let rec insert x l = 
    match l with
    [] -> [x]
  | h :: t ->
      if comp x h 
      then x :: h :: t
      else h :: insert x t
  in
    match l with
    [] -> []
  | [x] -> [x]
  | h :: t  -> 
      insert h (insertion_sort comp t)

(* filter : ('a -> bool) -> 'a list - > 'a list *)
let rec filter p l = 
  match l with
  [] -> []
  | h :: t ->
      if p h
      then h :: filter p t
      else filter p t

(* for_all : ('a -> bool) -> 'a list -> bool *)
let for_all p l = filter p l = l

(* mapl : ('a -> 'b) -> 'a list list -> 'b list list *)
let rec mapl f l = 
  match l with
  [] -> []
  | h :: t -> map f h :: mapl f t
