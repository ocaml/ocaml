(* TEST
flags = " -w -a "
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

module rec M
    : sig external f : int -> int = "%identity" end
    = struct external f : int -> int = "%identity" end
