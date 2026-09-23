(* TEST *)

(** check that custom blocks are not copied by Weak.get
    (this test formerly checked Weak.get_copy, see #15064) *)

open Bigarray
open Bigarray.Array1

let () =
  let a = ref (create float64 c_layout 10) in
  Gc.compact ();
  set !a 0 42.;

  let w = Weak.create 1 in
  Weak.set w 0 (Some !a);

  let b =
    match Weak.get w 0 with
    | None -> assert false
    | Some b -> b
  in
  Printf.printf "a.(0) = %f\n" (get !a 0);
  Printf.printf "b.(0) = %f\n" (get b 0);
  a := create float64 c_layout 10;
  Gc.compact ();

  let c = create float64 c_layout 10 in
  set c 0 33.;
  Printf.printf "b.(0) = %f\n" (get b 0);
