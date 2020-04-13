(* TEST
 *)

effect E : unit

let r = ref None
let () =
  match perform E; 42 with
  | n -> assert (n = 42)
  | effect E k ->
     continue k ();
     r := Some (k : (unit, unit) continuation);
     Gc.full_major ();
     print_string "ok\n"
