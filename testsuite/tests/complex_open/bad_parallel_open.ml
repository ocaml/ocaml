let pp = Format.printf

(* Ambiguous name *)
module M = struct let x = "M.x" end
module N = struct let x ="N.x" end;;

let which_x =
  let open M and N in
  x;;

open M and N
let which_x_global = x;;

module T = struct type t = C end
module U = struct type t = C end;;

let which_t =
    let open T and U in
    C ;;

module D = struct
  module Nested = struct let x = "D.Nested.x" end
end

module E = struct
  module Nested = struct let x = "E.Nested.x" end
end

open D and E
module Which_nested = Nested;;

module Z = struct type t = int end
module I = struct type t = int end

open Z and I
let which_int : t = 0;;

module O = struct class c = object end end
module A = struct class c = object end end

open O and A
let which_c = new c;;

module Os = struct class type cs = object end end
module As  = struct class type cs = object end end
open Os and As
let which_cs  : cs = object end;;

module V = struct module type S = sig end end
module R = struct module type S = sig end end

open V and R
module Which_s : S = struct end;;

(* Testing disambiguation *)
let which_x =
  let open M and N in
  let x = M.x in
  x;;


