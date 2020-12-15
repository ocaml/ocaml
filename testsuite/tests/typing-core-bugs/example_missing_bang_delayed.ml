let f x y =
  let z = ref 0 in
  z := !z + x;
  z := !z + y;
  z                 (* missing "!" here? *)
let _ =
  print_int (f 3 4) (* or maybe there? *)
