(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

let f = (fun x -> x) [@inline] [@inline never]
let g = (fun x -> x) [@inline] [@something_else] [@ocaml.inline]

let h x = (g [@inlined] [@ocaml.inlined never]) x

let v = ((fun x -> x) [@inline] [@inlined]) 1 (* accepted *)

let i = ((fun x -> x) [@inline]) [@@inline]

(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
