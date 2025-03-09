(* TEST_BELOW
(* Blank lines added here to preserve locations. *)




*)

let t =
  (function `A | `B -> () : 'a) (`A : [`A]);
  (failwith "dummy" : 'a) (* to know how 'a is unified *)

(* TEST
 flags = "-i";
 ocamlc_byte_exit_status = "2";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
