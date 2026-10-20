(* TEST
 flags = "-nopervasives -bytecode-hints";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints on block allocations and array lengths *)

(* Allocation of an immutable block *)
type record = { a : int; b : string }

let immutable_block a b = { a; b }

(* Allocation of a mutable block: no hint *)
type mutable_record = { mutable c : int; d : string }

let mutable_block c d = { c; d }

(* Allocation of an immutable float block *)
type float_record = { x : float; y : float }

let immutable_float_block x y = { x; y }

(* Allocation of a mutable float block: no hint *)
type mutable_float_record = { mutable z : float; t : float }

let mutable_float_block z t = { z; t }

(* Array lengths *)
external length : 'a array -> int = "%array_length"

let int_array_length (t : int array) = length t

let float_array_length (t : float array) = length t

let address_array_length (t : string array) = length t

let generic_array_length t = length t
