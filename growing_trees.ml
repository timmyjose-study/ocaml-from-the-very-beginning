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

type 'a tree = 
  Leaf 
  | Node of 'a tree * 'a * 'a tree

let rec list_to_tree l =
  match l with
  [] -> Leaf
  | [h] -> Node (Leaf, h, Leaf)
  | h :: t  ->
      let len = List.length t / 2 in
        let (left, right) = (take len t,  drop len t) in
          Node (list_to_tree left, h, list_to_tree right)

let tree = list_to_tree [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]

let rec tree_to_list t = 
  match t with
  Leaf -> []
  | Node (l, v, r) -> tree_to_list l @ [v] @ tree_to_list r

let rec size t = 
  match t with
  Leaf -> 0
  | Node (l, _, r) -> 1 + size l + size r

let rec maxdepth t = 
  match t with
  Leaf -> 0
  | Node (l, _, r) -> 1 + Int.max (maxdepth l) (maxdepth r)

let rec tree_map f t = 
  match t with
  Leaf -> Leaf
  | Node (l, v, r) -> Node (tree_map f l, f v, tree_map f r)