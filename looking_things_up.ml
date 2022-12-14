(* fst : ('a, 'b) -> 'a *)
let fst (a, _) = a

(* snd : ('a, 'b) -> 'b *)
let snd (_, b) = b

(* census : (int x int) list *)
let census = [(1, 4); (2, 2); (3, 2); (4, 3); (5, 1); (6, 2)]

(* lookup : 'a -> ('a x 'b) list -> 'b *)
let rec lookup k t = 
  match t with
  [] -> raise Not_found
  | (h, v) :: t -> 
      if h = k
      then v
      else lookup k t

(* add : 'a -> ('a x 'b) list -> ('a x 'b) list *)
let rec add k v t =
  match t with
  [] -> [(k, v)]
  | (kk, vv) :: t ->
      if kk = k
      then (k, v) :: t
      else (kk, vv) :: add k v t

(* remove : 'a -> ('a x 'b) list -> ('a x 'b) list *)
let rec remove k t = 
  match t with
  [] -> []
  | (kk, vv) :: t ->
      if kk = k
      then t
      else (kk, vv) :: remove k t 

(* key_exists : 'a -> ('a x 'b) list -> bool *)
let key_exists k t =
  try 
    let _ = lookup k t in true
  with 
    Not_found -> false

(* Exercises *)

(* num_keys : ('a x 'b) list -> int *)
let num_keys t = List.length t

(* replace : 'a -> ('a x 'b) -> ('a x 'b) list *)
let rec replace k v t = 
  match t with
  [] -> raise Not_found
  | (kk, vv) :: t ->
      if kk = k
      then (kk, v) :: t
      else (kk, vv) :: replace k v t

(* make_dict : 'a list -> 'b list -> ('a x 'b) list *)
let rec make_dict ks vs = 
  if List.length ks != List.length vs
  then raise (Invalid_argument "ks and vs are not of the same length")
  else 
    match ks, vs with
    [], _ -> []
  | _, [] -> []
  | k :: ks, v :: vs ->
      (k, v) :: make_dict ks vs

(* destructure_dict : ('a x 'b) list -> ('a list, 'b list) *)
let destructure_dict t = 
  let rec destructure_dict_inner ks vs t = 
    match t with
    [] -> (ks, vs)
  | ( k, v) :: t ->
      destructure_dict_inner (k :: ks) (v :: vs) t
  in
    destructure_dict_inner [] [] t

(* pair_to_dict : ('a x 'b) list -> ('a x 'b) list *)
let pair_to_dict ps = 
  let rec pair_to_dict_inner dict ps = 
    match ps with
    [] -> dict
    | (k, v) :: t ->
        if key_exists k dict
        then pair_to_dict_inner dict t 
        else pair_to_dict_inner ((k, v) :: dict) t
  in
    pair_to_dict_inner [] ps

(* union : ('a x 'b) list -> ('a x 'b) list -> ('a x 'b) list *)
let union d t = 
  pair_to_dict (d @ t)
