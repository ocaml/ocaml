(* TEST
 *)

open Identity

let () =
  assert((map (fun _ -> 1) 0) = 1);
  assert(((fun _ -> 1) <*> 2) = 1);
  assert((bind 5 (fun _ -> -1)) = -1);
  assert((2 >>= (fun _ -> 3)) = 3);
  assert((return 2) = 2);
  print_endline "OK"
