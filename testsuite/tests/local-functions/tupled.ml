(* TEST
*)

(* PR#8705 *)
let () =
  let tupled (x, y) =
    print_string "";
    fun z -> x, y, z
  in
  let a, b, c = tupled (0, 1) 2 in
  assert (a = 0 && b = 1 && c = 2)
