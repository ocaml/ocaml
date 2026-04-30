(* TEST
 flags += " -g ";
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)

let f (z:int) = z, 2 * z + 1
module type T = sig type t end
let g (module M:T) (x:M.t) y = x, 1 + y

let stop = f (1 + 1)

let a = f 1
let b = g (module String) "x" 100
