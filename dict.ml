type ('a, 'b) tree = 
  Leaf
  | Node of ('a, 'b) tree * ('a * 'b) * ('a, 'b) tree

let tree1 = Node (Node (Leaf, (1, "one"), Node (Leaf, (2, "two"), Leaf)), (3, "three"), Node (Leaf, (4, "four"), Leaf))

let rec size t = 
  match t with
  Leaf -> 0
  | Node (l, _, r) -> 1 + size l + size r

let rec maxdepth t = 
  match t with
  Leaf -> 0
  | Node (l, _, r) -> 1 + Int.max (maxdepth l) (maxdepth r)

(* dictionary type *)

type 'a dict = ('a, string) tree

let rec insert (k: 'a) (v: 'b) (d: 'a dict): 'a dict =
  match d with
  Leaf -> Node (Leaf, (k, v), Leaf)
  | Node (l, (kk, vv), r) ->
      if kk > k
      then Node (insert k v l, (kk, vv), r)
      else Node (l, (kk, vv), insert k v r)

(*
let rec lookup (k: 'a) (d: 'a dict) = 
  match d with
  Leaf -> raise Not_found
  | Node (l, (kk, vv), r) ->
      if kk = k
      then vv
      else if kk > k
        then lookup k l
        else lookup k r
*)

let rec lookup (k: 'a) (d: 'a dict): 'b option =
  match d with
  Leaf -> None
  | Node (l, (kk, vv), r) ->
      if kk = k
      then Some vv
      else if kk > k
            then lookup k l
            else lookup k r

let rec zip (xs: 'a list) (ys: 'b list): ('a * 'b) list = 
  match xs, ys with
  [], _ -> []
  | _, [] -> []
  | x :: xs, y :: ys ->
      (x, y) :: zip xs ys

let list_to_dict (l: ('a * 'b) list): 'a dict = 
  let rec list_to_dict_inner l d = 
    match l with 
    [] -> d
  | (k, v) :: kvs ->
      list_to_dict_inner kvs (insert k v d)
  in
    list_to_dict_inner l Leaf

let (tree2: int dict) = 
  list_to_dict (zip [5; 1; 2; 3; 4; 11; 10] ["five"; "one"; "two"; "three"; "four"; "eleven"; "ten"])

let print_dict_entry ((k, v): ('a * string)) = 
  print_int k; (* hack! *)
  print_string " => ";
  print_string v;
  print_newline ()

let rec inorder (d: 'a dict) =
  match d with
  Leaf -> ()
  | Node (l, e, r) ->
      inorder l;
      print_dict_entry e;
      inorder r;
