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

(* Use type information *)
type t = {x: int; y: int};;
type u = {x: bool; y: bool};;

let f (r:t) = r.x;; (* ok *)
let f r = ignore (r:t); r.x;; (* non principal *)

let f (r: t) =
  match r with
    {x; y} -> y + y;; (* ok *)
let f r =
    match r with
       {x; y} -> y + y;; (* fails *)
let f r =
    ignore (r: t);
    match r with
       {x; y} -> y + y;; (* fails for -principal *)

(* Use type information with modules*)
module M = struct
  type t = {x:int}
  type u = {x:bool}
end;;
let f (r:M.t) = r.M.x;; (* ok *)
let f (r:M.t) = r.x;; (* warning *)

module M = struct
  type t = {x: int; y: int}
end;;
module N = struct
  type u = {x: bool; y: bool}
end;;
open M;;
open N;;
let f (r:M.t) = r.x;; 

module M = struct
  type t = {x:int}
  module N = struct type s = t = {x:int} end
  type u = {x:bool}
end;;
open M.N;;
let f (r:M.t) = r.x;; 

(* Use field information *)
type u = {x:bool;y:int;z:char};;
type t = {x:int;y:bool};;
fun {x;z} -> x,z;;

type u = {x:int;y:bool};;
type t = {x:bool;y:int;z:char};;
{x=3; y=true};; 
