(* TEST
 ocamlrunparam += ",W=1";
*)

(* With runtime warnings enabled, the runtime reports each continuation
   that it reclaims without it having been resumed. *)

open Effect
open Effect.Deep

type _ t += Yield : unit t

let gen () : (unit, unit) continuation option =
  let r : (unit, unit) continuation option ref = ref None in
  (match perform Yield with
   | () -> ()
   | effect Yield, k -> r := Some k);
  !r

let[@inline never] promote_then_drop () =
  let k = gen () in
  Gc.full_major ();
  ignore (Sys.opaque_identity k)

let () =
  print_endline "dropped young";
  ignore (gen ());
  Gc.full_major ();
  print_endline "dropped after promotion";
  promote_then_drop ();
  Gc.full_major ();
  Gc.full_major ();
  print_endline "resumed";
  (match gen () with Some k -> continue k () | None -> ());
  Gc.full_major ();
  print_endline "warnings disabled";
  Sys.enable_runtime_warnings false;
  ignore (gen ());
  Gc.full_major ();
  print_endline "done"
