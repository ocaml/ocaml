module M = struct
  let x="mx"
  let x="x"
end;;

module N = struct
  let x="nx"
  let x="x"
end;;

(* value shadowing is authorized in sequential include *)
include M;;
include N;;
(* not in grouped include *)
include M and N;;

module F() = struct include M end;;
(* nested grouped include *)
include F() and F();;

module T = struct type t = int end 
module U = struct type t = float end
include T and U;;

module D = struct module Nested = struct end end 
module E = struct module Nested = struct end end
include D and E;;

module V = struct module type S = sig end end 
module W = struct module type S = sig end end
include V and W;;
