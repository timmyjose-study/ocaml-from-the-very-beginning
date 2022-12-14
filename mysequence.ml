type 'a sequence = Nil | Cons of 'a * 'a sequence

let rec length_seq xs = 
  match xs with
  Nil -> 0
  | Cons (_, xs) -> 1 + length_seq xs

let rec append_seq xs ys =
  match xs, ys with
  Nil, ys -> ys
  | Cons (x, xs), ys -> Cons (x, append_seq xs ys)

let rec take_seq n xs = 
  match xs with
  Nil -> Nil
  | Cons (x, xs) ->
      if n = 0
      then Nil
      else Cons (x, take_seq (n - 1) xs)

let rec drop_seq n xs = 
  match xs with
  Nil -> Nil
  | Cons (x, xs) ->
      if n = 0
      then Cons (x, xs)
      else drop_seq (n - 1) xs

let list_int = Cons (1, Cons (2, Cons (3, Cons (4, Cons (5, Nil)))))
