(* TEST_BELOW
(* Blank lines added here to preserve locations. *)













*)

open Deprecated_module

type s = M.t

open M
let _ = x

(* TEST
 modules = "deprecated_module.mli deprecated_module.ml";
 setup-ocamlc.byte-build-env;
 flags = "-w -a";
 module = "deprecated_module.mli";
 ocamlc.byte;
 module = "deprecated_module.ml";
 ocamlc.byte;
 flags = "-w +A-70";
 module = "deprecated_module_use.ml";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
