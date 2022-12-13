(* safe_divide : int -> int -> int *)
let safe_divide n m = 
  try n / m with
  Division_by_zero -> 0

(* last -> 'a list -> 'a *)
let rec last l = 
  match l with
  [] -> raise Not_found
  | [x] -> x
  | _ :: t -> last t

(* take : int -> 'a list -> 'a list *)
let rec take n l = 
  match l with
  [] -> 
    if n = 0
    then []
    else raise (Invalid_argument "take")
  | h :: t ->
      if n < 0
      then raise (Invalid_argument "take")
      else if n = 0
          then []
          else h :: take (n - 1) t

(* drop : int -> 'a list -> 'a list *)
let rec drop n l =
  match l with
  [] -> 
    if n = 0
    then []
    else raise (Invalid_argument "drop")
  | h :: t ->
      if n < 0 
      then raise (Invalid_argument "drop")
      else if n = 0
          then l 
          else drop (n - 1) t

(* Exercises *)

(* smallest : int list -> int list *)
let smallest l = 
  let rec smallest_inner l res = 
    match l with
    [] -> if res = -1 then raise Not_found else res
  | h :: t -> 
      if h > 0 && (res = -1 || h < res)
      then smallest_inner t h
      else smallest_inner t res
  in
    smallest_inner l (-1)

(* smallest_or_zero : int list - > int *)
let smallest_or_zero l = 
  try smallest l with
  Not_found -> 0

exception Negative_arg_to_sqrt

(* floor_sqrt : int -> int *)
let floor_sqrt n = 
  let rec floor_sqrt_inner n s = 
    if (s + 1) * (s + 1) > n
    then s
    else floor_sqrt_inner n (s + 1)
  in
    if n < 0 
    then raise Negative_arg_to_sqrt
    else floor_sqrt_inner n 1

(* floor_sqrt_or_zero : int -> int *)
let floor_sqrt_or_zero n = 
  try floor_sqrt n with
  Negative_arg_to_sqrt -> 0
