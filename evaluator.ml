type expr = 
  Num of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Pow of expr * expr

let rec pow b e = 
  match e with
  0 -> 1
  | _ -> b * pow b (e - 1)

let rec evaluate e = 
  try 
    match e with
    Num n -> n
    | Add (e1, e2) -> evaluate e1 + evaluate e2
    | Sub (e1, e2) -> evaluate e1 - evaluate e2
    | Mul (e1, e2) -> evaluate e1 * evaluate e2
    | Div (e1, e2) -> evaluate e1 / evaluate e2
    | Pow (e1, e2) -> pow (evaluate e1) (evaluate e2)
  with
    Division_by_zero -> 0

