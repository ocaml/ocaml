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

type 'a self_rec = Self of ('a self_rec -> 'a) [@@unboxed]

let g
    (Self self : (?x:int -> unit -> int) self_rec)
    ?(x = self (Self self) ()) () =
  x + 1

let _ =
  if Sys.opaque_identity false then
    g (Self g) ()
  else
    0
