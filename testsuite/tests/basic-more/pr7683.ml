(* TEST *)

let f () n () =
  n

let g () =
  let r = ref 0 in
  f (incr r) !r (incr r)

let () = print_int (g ())
