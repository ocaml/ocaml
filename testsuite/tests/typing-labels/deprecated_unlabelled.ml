(* TEST
 expect;
*)

(* [@@deprecated_unlabelled] on a value declaration records that this value
   used to have no labelled parameter: an application that omits all the
   labels is still accepted, but it triggers a deprecation alert instead of
   warning 6. *)

module M : sig
  val f : foo:int -> bar:int -> int
    [@@deprecated_unlabelled]
end = struct
  let f ~foo ~bar = foo + bar
end
[%%expect{|
module M : sig val f : foo:int -> bar:int -> int end
|}]

let a = M.f ~foo:1 ~bar:2
[%%expect{|
val a : int = 3
|}]

(* All the labels may be omitted, in which case the arguments are matched
   positionally. *)
let b = M.f 1 2
[%%expect{|
Line 1, characters 8-11:
1 | let b = M.f 1 2
            ^^^
Alert deprecated: omitting the labels in this application

val b : int = 3
|}]

(* The labels are still needed in a partial application. *)
let c = M.f 1
[%%expect{|
Line 1, characters 12-13:
1 | let c = M.f 1
                ^
Error: The function applied to this argument has type
         foo:int -> bar:int -> int
This argument cannot be applied without label
|}]

(* They cannot be omitted one at a time either. *)
let c = M.f 1 ~bar:2
[%%expect{|
Line 1, characters 12-13:
1 | let c = M.f 1 ~bar:2
                ^
Error: The function applied to this argument has type foo:int -> int
This argument cannot be applied without label
|}]

(* An extra message can be attached to the attribute. *)
module N : sig
  val g : foo:int -> bar:int -> int
    [@@deprecated_unlabelled "use M.f instead"]
end = struct
  let g ~foo ~bar = foo + bar
end
[%%expect{|
module N : sig val g : foo:int -> bar:int -> int end
|}]

let d = N.g 1 2
[%%expect{|
Line 1, characters 8-11:
1 | let d = N.g 1 2
            ^^^
Alert deprecated: omitting the labels in this application
use M.f instead

val d : int = 3
|}]

(* Parameters that are already unlabelled, and optional ones, are handled as
   usual. *)
module P : sig
  val h : x:int -> int -> ?opt:int -> unit -> int
    [@@deprecated_unlabelled]
end = struct
  let h ~x y ?(opt = 0) () = x + y + opt
end
[%%expect{|
module P : sig val h : x:int -> int -> ?opt:int -> unit -> int end
|}]

let e = P.h 1 2 ()
[%%expect{|
Line 1, characters 8-11:
1 | let e = P.h 1 2 ()
            ^^^
Alert deprecated: omitting the labels in this application

val e : int = 3
|}]

(* The attribute is only consulted when the value is applied directly: going
   through a binding loses it, and warning 6 applies again. *)
let f = let f = M.f in f 1 2
[%%expect{|
Line 1, characters 23-24:
1 | let f = let f = M.f in f 1 2
                           ^
Warning 6 [labels-omitted]: labels "foo", "bar" were omitted in the application
  of this function.

val f : int = 3
|}]
