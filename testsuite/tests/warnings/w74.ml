(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

type a = true
type b = false

type c =
  | true
  | false

type d =
  | A
  | true
  | C

type should_not_warn = bool =
  | false
  | true

(* TEST
 flags = "-w +A-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
