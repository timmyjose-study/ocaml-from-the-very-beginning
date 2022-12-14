type colour = 
  Red 
  | Green 
  | Blue 
  | Yellow 
  | RGB of int * int * int

(* )cols : colour list *)
let cols = [Red; Red; Green; Blue; Green; Red; Blue; Green; Blue]

(* components : colour -> int x int x int *)
let components col =
  match col with
  Red -> (255, 0, 0)
  | Green -> (0, 255, 255)
  | Blue -> (0, 0, 255)
  | Yellow -> (255, 255, 0)
  | RGB (r, g, b) -> (r, g, b)

type 'a maybe = Nothing | Just of 'a

(* safe_divide : int -> int -> int maybe *)
let safe_divide n m = 
  if m = 0
  then Nothing
  else Just (n / m)

(* lookup_opt : 'a -> ('a x 'b) list -> 'b option *)
let rec lookup_opt k t = 
  match t with
  [] -> None
  | (kk, vv) :: t ->
      if kk = k
      then Some vv
      else lookup_opt k t

(* table : (int x string) list *)
let table = [(1, "hello"); (2, "world"); (3, "we"); (4, "meet"); (5, "again")]

type 'a sequence = Nil | Cons of 'a * 'a sequence

(* list_int : int sequence *)
let list_int = Cons (1, (Cons (2, Cons (3, (Cons (4, Cons (5, Nil)))))))

let rec length_seq l = 
  match l with
  Nil -> 0
  | Cons (_, t) -> 1 + length_seq t

let rec append_seq xs ys = 
  match xs, ys with
  Nil, ys -> ys
  | Cons (x, xs), ys -> Cons (x, append_seq xs ys)

type expr = 
  Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr

let rec evaluate e = 
  match e with
  Num n -> n
  | Add (e1, e2) -> evaluate e1 + evaluate e2
  | Sub (e1, e2) -> evaluate e1 - evaluate e2
  | Mul (e1, e2) -> evaluate e1 * evaluate e2
  | Div (e1, e2) ->
      match evaluate e2 with
      0 -> 0
      | e2res -> evaluate e1 / e2res

(* 1 + 2 * 3 *)
let e1 = Add (Num 1, Mul (Num 2, Num 3))

(* 1 * 2 + 3 *)
let e2 = Add (Mul (Num 1, Num 2), Num 3)

(* Exercises *)

type rect = 
  Rect of int * int

let area r = 
  match r with
  Rect (w, h) -> w * h

let rotate r = 
  match r with
  Rect (w, h) ->
    if h < w
    then Rect (h, w)
    else r

let rec take n l = 
  match l with
  [] -> []
  | h :: t ->
      if n = 0
      then []
      else h :: take (n - 1) t

let rec drop n l = 
  match l with
  [] -> []
  | _  :: t ->
      if n = 0
      then l
      else drop (n - 1) t

let rec msort l comp = 
  let rec merge ls rs = 
    match ls, rs with
      [], rs -> rs
      | ls, [] -> ls
      | l :: ls, r :: rs ->
          if comp l r
          then l :: merge ls (r :: rs) 
          else r :: merge (l :: ls) rs 
  in
  match l with
  [] -> []
  | [x] -> [x]
  | _ ->
      let len = List.length l / 2 in
        let (left, right) = (take len l, drop len l) in
          merge (msort left comp) (msort right comp) 

let sort_rects rs = 
  msort (List.map rotate rs) 
  (fun r1 r2 ->
    match r1, r2 with
    Rect (w1, h1), Rect (w2, h2) ->
      if w1 > w2
      then true
      else false)

let rects = [Rect (1, 10); Rect (20, 1); Rect (2, 3); Rect (3, 4); Rect (10, 10); Rect (20, 20); Rect (11, 10)]

