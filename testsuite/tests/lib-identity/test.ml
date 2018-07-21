(* TEST
 *)

open Identity

let () =
  assert ((map (fun _ -> 1) 0) = 1);
  assert (((fun x -> x + 1) <@@> 1) = 2);
  assert (((fun _ -> 1) <*> 2) = 1);
  assert ((bind 5 (fun _ -> -1)) = -1);
  assert ((2 >>= (fun _ -> 3)) = 3);
  assert ((return 2) = 2);
  assert ((compose_after (fun x -> x * 3) (fun x -> x + 1) 0) = 3);
  assert ((compose_before (fun x -> x + 1) (fun x -> x * 3) 0) = 3);
  print_endline "OK"
