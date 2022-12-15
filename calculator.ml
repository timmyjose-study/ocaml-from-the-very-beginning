let add (n: int) (m: int): int = 
  n + m

let sub (n: int) (m: int): int = 
  n - m

let mul (n: int) (m: int): int = 
  n * m

let div (n: int) (m: int): int option = 
  match m with 
  0 -> None
  | _ -> Some (n / m)