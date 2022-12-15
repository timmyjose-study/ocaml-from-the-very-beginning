let swap (a: 'a ref) (b: 'a ref) = 
  let t = !a in
    a := !b;
    b := t

let print_1_to_n (n: int) = 
  for ctr = 1 to n do
    print_int ctr;
    print_string " "
  done;
  print_newline ()

let print_n_to_1 (n: int) = 
  let ctr = ref n in
    while !ctr >= 0 do
      print_int !ctr;
      print_string " ";
      ctr := !ctr - 1
    done;
    print_newline ()

let smallest_pow2 (n: int): int = 
  let t = ref 1 in
    while !t < n do
      t := !t * 2
    done;
    !t
