(* TEST_BELOW
(* Blank lines added here to preserve locations. *)
(* Note:
   This test was expected to fail because the definition in
   pr3918a.cmi is forgotten. However, with keep-expansion,
   there is no way to hide the expanded type, which gets
   inlined in pr3918b.cmi, making the test succed. *)






*)

(*
  ocamlc -c pr3918a.mli pr3918b.mli
  rm -f pr3918a.cmi
  ocamlc -c pr3918c.ml
*)

open Pr3918b

let f x = (x : 'a vlist :> 'b vlist)
let f (x : 'a vlist) = (x : 'b vlist)

(* TEST
 readonly_files = "pr3918a.mli pr3918b.mli";
 setup-ocamlc.byte-build-env;
 module = "pr3918a.mli";
 ocamlc.byte;
 module = "pr3918b.mli";
 ocamlc.byte;
 script = "rm -f pr3918a.cmi";
 script;
 {
   module = "pr3918c.ml";
   ocamlc_byte_exit_status = "0";
   ocamlc.byte;
 }{
   check-ocamlc.byte-output;
 }
*)
