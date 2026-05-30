(* TEST
 flambda;
 ocamlopt_flags += " -O3 ";
 native;
*)

(* Regression test for Issue #14828:
   Ensure that inlining recursive stubs terminates. *)

let rec f ?(x = f ()) () = x + 1

let rec g ?(x = h ()) () = x + 1
and h ?(y = g ()) () = y + 2

let rec i ?(x = j ()) () = x + 1
and j ?(y = k ()) () = y + 2
and k ?(z = i ()) () = z + 3

let caller () =
  let _ = (f[@unroll 1]) () in
  let _ = (g[@unroll 1]) () in
  let _ = (i[@unroll 1]) () in
  ()
