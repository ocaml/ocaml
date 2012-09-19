(* undefined labels *)
type t = {x:int;y:int};;
{x=3;z=2};;
fun {x=3;z=2} -> ();;

(* mixed labels *)
{x=3; contents=2};;

(* private types *)
type u = private {mutable u:int};;
{u=3};;
fun x -> x.u <- 3;;

(* Punning and abbreviations *)
module M = struct
  type t = {x: int; y: int}
end;;

let f {M.x; y} = x+y;;
let r = {M.x=1; y=2};;
let z = f r;;

module M = struct
  type t = {x: int; y: int}
  type u = {y: bool}
end;;
(* path abbreviation is syntactic *)
let f {M.x; y} = x+y;; (* fails *)
let r = {M.x=1; y=2};; (* fails *)

(* Use type information *)
let f (x:Complex.t) = x.re;;
let f x = ignore (x:Complex.t); x.re;; (* non principal *)
module M = struct
  type t = {x:int}
  module N = struct type s = t = {x:int} end
  type u = { x:bool}
end;;
let f (r:M.u) = r.x;;
let f (r:M.t) = r.x;; (* fails *)
open M.N;;
let f r = r.x;;
let f (r:M.t) = r.x;; (* ok *)
