(* TEST
  toplevel;
*)

open Effect;;
open Effect.Deep;;
type _ eff += E : unit eff;;

let () =
  (match perform E with
   | v -> v
   | effect E, k when (continue k (); false) -> assert false
   | effect E, k' -> continue k' ())
;;
