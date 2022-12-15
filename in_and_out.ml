let print_dict_entry (k, v) =
  print_int k;
  print_newline ();
  print_string v;
  print_newline ()

let rec iter f l = 
  match l with
  [] -> ()
  | h :: t -> f h ; iter f t

let dict = [(1, "hello"); (2, "world"); (3, "we"); (4, "meet"); (5, "again")]

let print_dict = iter print_dict_entry 

let rec read_dict () = 
  try
    let k = read_int () in
      if k = 0 
      then []
      else let v = read_line () in
            (k, v) :: read_dict ()
  with
    Failure "int_of_string" ->
      print_string "This is not a valid integer. Try again..";
      print_newline ();
      read_dict ()