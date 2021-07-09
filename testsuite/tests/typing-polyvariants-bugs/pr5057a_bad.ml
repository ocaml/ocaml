(* TEST
flags = " -w -a "
ocamlc_byte_exit_status = "2"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

(* This one should fail *)

let f flag =
  let module T = Set.Make(struct type t = int let compare = compare end) in
  let _ = match flag with `A -> 0 | `B r -> r in
  let _ = match flag with `A -> T.mem | `B r -> r in
  ()
