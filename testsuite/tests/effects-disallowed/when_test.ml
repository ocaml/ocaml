(* TEST
   * toplevel
*)

effect E : unit;;
let () =
  (match perform E with
   | v -> v
   | effect E k when (continue k (); false) -> assert false
   | effect E k' -> continue k' ())
;;
