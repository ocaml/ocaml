(* TEST_BELOW *)

(* "_" is a simple expression (Pexp_hole) *)
let _ = _;;
f _ 0;;
_ 0;;
_.a;;
_ # m;;
(_ : int);;
(_, _);;
Some _;;
lazy _;;
1 + _;;

(* Labelled argument sugar *)
f ~_ ?_ ~_:_ ?_:_;;

(* "_" is a module expression (Pmod_hole) *)
module M = _;;
module N = _(M);;
module O = _();;
module P = F(_);;
module Q = (_ : sig end);;
module R = functor () -> _;;
include _;;
open _;;
let _ = (module _ : S);;
let _ = let module L = _ in ();;

(* TEST
 flags = "-dparsetree -dno-locations -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
