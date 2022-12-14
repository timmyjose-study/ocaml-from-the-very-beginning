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
  | _ :: t ->
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