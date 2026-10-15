(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints on closure allocations *)

external ( + ) : int -> int -> int = "%addint"
external ( = ) : int -> int -> bool = "%equal"

(* Kinds of the parameters and of the result *)
let kinds (x : int) (y : float) (z : int array) (t : string) = y

(* Attributes *)
let[@inline] inline x = x + 1

let[@inline never] inline_never x = x + 1

module Functor (X : sig end) = struct end

(* Mutually recursive closures *)
let rec even n = if n = 0 then true else odd (n + -1)

and odd n = if n = 0 then false else even (n + -1)
