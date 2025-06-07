(* TEST
   expect;
*)

module M : sig
  type t = int [@@deprecated_repr]
  val zero : t
  val print : t -> unit
  val prints : t list -> unit
end = struct
  type t = int
  let zero = 0
  let print = Printf.printf "%d\n%!"
  let prints l = List.iter print l
end

[%%expect {|
module M :
  sig
    type t = int
    val zero : t
    val print : t -> unit
    val prints : t list -> unit
  end
|}] (* ideally, deprecated_repr would be printed, but the issue exists with
       [@@deprecated] *)

(****
  Converting between int and t should trigger warnings, but simply using
  values of type t shouldn't.
 ****)

let () = M.print M.zero
[%%expect {|
|}]
let () = M.print 0
[%%expect {|
Line 1, characters 17-18:
1 | let () = M.print 0
                     ^
Alert deprecated: implicitly converting between a type and its deprecated representation
|}]
let () = (fun z -> z 0) M.print
[%%expect {|
Line 1, characters 24-31:
1 | let () = (fun z -> z 0) M.print
                            ^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation
|}]
let () = M.print (if true then M.zero else 0)
[%%expect {|
Line 1, characters 43-44:
1 | let () = M.print (if true then M.zero else 0)
                                               ^
Alert deprecated: implicitly converting between a type and its deprecated representation
|}]
let __ x = ((x : int) : M.t)
[%%expect {|
Line 1, characters 12-21:
1 | let __ x = ((x : int) : M.t)
                ^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : int -> M.t = <fun>
|}]
let __ x = ((x : M.t) : int)
[%%expect {|
Line 1, characters 12-21:
1 | let __ x = ((x : M.t) : int)
                ^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : M.t -> int = <fun>
|}]
let __ x = (x : int :> M.t)
[%%expect {|
Line 1, characters 11-27:
1 | let __ x = (x : int :> M.t)
               ^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 11-27:
1 | let __ x = (x : int :> M.t)
               ^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : int -> M.t = <fun>
|}] (* bug: why two warnings *)
let __ x = (x : M.t :> int)
[%%expect {|
Line 1, characters 11-27:
1 | let __ x = (x : M.t :> int)
               ^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 11-27:
1 | let __ x = (x : M.t :> int)
               ^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : M.t -> int = <fun>
|}] (* bug: why two warnings *)

let _ = M.zero (* This expands the type definition to warn if it's a function. It seems
                  fine to warn or not to warn if the repr is deprecated, I suppose, since
                  the new state would be not warning. *)
[%%expect {|
- : M.t = 0
|}]

(****
  Converting between M.t and u shouldn't trigger warnings, since this is correct
  regardless of t's representation. This implies that the typer shoudn't merely
  expands all head type constructors during unification and other other operations.
 ****)

type u = M.t
module F(U : sig
           val print : u -> unit
           val zero : u
         end) : sig end = struct
  open U

  let _ : M.t -> u = Fun.id
  let _ : u -> M.t = Fun.id

  let () = print M.zero
  let () = M.print zero
  let __ (x : M.t) : u = x
  let __ (x : u) : M.t = x
end
[%%expect {|
type u = M.t
module F : (U : sig val print : u -> unit val zero : u end) -> sig end
|}]

(**** in expressions ****)

module F : sig
  type t = int -> int [@@deprecated_repr]
  val f : t
end = struct
  type t = int -> int
  let f = Fun.id
end
let _ = F.f 1
[%%expect {|
module F : sig type t = int -> int val f : t end
- : int = 1
|}] (* bug: should warn *)

module C : sig
  type t = < a : int > [@@deprecated_repr]
end = struct
  type t = < a : int >
end
let __ (x : C.t) = x#a
[%%expect {|
module C : sig type t = < a : int > end
val __ : C.t -> int = <fun>
|}] (* bug: should warn *)

(**** in types ****)

type _t = [ `A of M.t | `A of int ]
type _u = [ `A of u | `A of M.t ]
[%%expect {|
Line 1, characters 10-35:
1 | type _t = [ `A of M.t | `A of int ]
              ^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

type _t = [ `A of M.t ]
type _u = [ `A of u ]
|}]

type 'a constrained = unit constraint 'a = int
let _ =
  let __ (_ : M.t constrained) = () in
  ()
[%%expect {|
type 'a constrained = unit constraint 'a = int
Line 3, characters 14-17:
3 |   let __ (_ : M.t constrained) = () in
                  ^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

- : unit = ()
|}]

(**** in modules ****)

module Z1 : sig
  val x : int
end = struct
  let x = M.zero
end
[%%expect {|
Line 2, characters 2-13:
2 |   val x : int
      ^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

module Z1 : sig val x : int end
|}]

module Z2 : sig
  type z = int
end = struct
  type z = M.t
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type z = M.t
5 | end
Alert deprecated: implicitly converting between a type and its deprecated representation

module Z2 : sig type z = int end
|}]

module Z3 : sig
  type z = int
end = struct
  type z = int [@@deprecated_repr]
end (* bug: missing warning *)
[%%expect {|
module Z3 : sig type z = int end
|}]

let f () =
  (* nondep expanding local type constructors away. It's hard to imagine why anyone
     would care about this, but perhaps there are examples involving functors that make
     sense. *)
  let module M : sig
        type t = int [@@deprecated_repr]
        val x : t
      end = struct
        type t = int
        let x = 0
      end
  in
  M.x
[%%expect {|
Line 13, characters 2-5:
13 |   M.x
       ^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val f : unit -> int = <fun>
|}]

(**** in patterns ****)

let _ = fun ((_ : M.t) : int) -> ()
let _ = fun ((_ : int) : M.t) -> ()
[%%expect {|
Line 1, characters 13-22:
1 | let _ = fun ((_ : M.t) : int) -> ()
                 ^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

- : int -> unit = <fun>
Line 2, characters 13-22:
2 | let _ = fun ((_ : int) : M.t) -> ()
                 ^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

- : M.t -> unit = <fun>
|}]

(**** type-based disambiguation ****)

type record = { a : int }
type record_alias = record [@@deprecated_repr]
let __ (x : record_alias) = x.a
let __ ({ a } : record_alias) = a
[%%expect {|
type record = { a : int; }
type record_alias = record
val __ : record_alias -> int = <fun>
val __ : record_alias -> int = <fun>
|}, Principal{|
type record = { a : int; }
type record_alias = record
val __ : record_alias -> int = <fun>
Line 4, characters 8-13:
4 | let __ ({ a } : record_alias) = a
            ^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : record_alias -> int = <fun>
|}]
(* bug: should warn regardless of -principal, and both functions should warn *)

type sum = A of int
type sum_alias = sum [@@deprecated_repr]
let __ (A a : sum_alias) = a
[%%expect {|
type sum = A of int
type sum_alias = sum
val __ : sum_alias -> int = <fun>
|}, Principal{|
type sum = A of int
type sum_alias = sum
Line 3, characters 8-11:
3 | let __ (A a : sum_alias) = a
            ^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : sum_alias -> int = <fun>
|}]
(* bug: should warn regardless of -principal *)

(**** when optional parameters are filled in ****)

open (struct
       type fu = unit -> unit
       let create = Fun.id
       let to_fun = Fun.id
     end : sig
       (* Making the type really opaque would create a type error below, so in
          principle a warning should be triggered. Which is probably doable, but not
          done since this problem seems rather niche. *)
       type fu = unit -> unit [@@deprecated_repr]
       val create : (unit -> unit) -> fu
       val to_fun : fu -> (unit -> unit)
     end)
let truc : fu -> unit = fun _f -> ()
[%%expect{|
type fu = unit -> unit
val create : (unit -> unit) -> fu = <fun>
val to_fun : fu -> unit -> unit = <fun>
val truc : fu -> unit = <fun>
|}]

let f ?machin:_ = create (fun () -> ())
let () = truc f
[%%expect{|
Line 1, characters 14-15:
1 | let f ?machin:_ = create (fun () -> ())
                  ^
Warning 16 [unerasable-optional-argument]: this optional argument cannot be erased.

val f : ?machin:'a -> fu = <fun>
Line 2, characters 14-15:
2 | let () = truc f
                  ^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 2, characters 14-15:
2 | let () = truc f
                  ^
Error: The value "f" has type "?machin:'a -> fu"
       but an expression was expected of type "fu" = "unit -> unit"
       The first argument is labeled "?machin",
       but an unlabeled argument was expected
|}] (* bug? Not clear if warning 16 should trigger here or not. Also not clear if the last
       function should fail to type or not. *)

(**** parametric type ****)

module P : sig
  type 'a t = 'a * int [@@deprecated_repr]
  val z : 'a list t
end = struct
  type 'a t = 'a * int
  let z = [], 1
end

let x = P.z
let __ (x : int P.t) = ()
let __ () = (assert false : float P.t)

[%%expect{|
module P : sig type 'a t = 'a * int val z : 'a list t end
val x : 'a list P.t = ([], 1)
val __ : int P.t -> unit = <fun>
val __ : unit -> float P.t = <fun>
|}]

(**** parametric but phantom type ****)

module Phantom : sig
  type 'a t = int [@@deprecated_repr]
  val z : 'a t
end = struct
  type 'a t = int
  let z = 1
end

let x = Phantom.z
[%%expect{|
module Phantom : sig type 'a t = int val z : 'a t end
val x : 'a Phantom.t = 1
|}]
let __ (x : int Phantom.t) = ()
[%%expect{|
Line 1, characters 7-26:
1 | let __ (x : int Phantom.t) = ()
           ^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : int -> unit = <fun>
|}] (* bug: this shouldn't warn *)
let __ () = (assert false : float Phantom.t)
[%%expect{|
Line 1, characters 12-44:
1 | let __ () = (assert false : float Phantom.t)
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : unit -> int = <fun>
|}] (* bug: this shouldn't warn *)
let __ (x : int Phantom.t) = (x : float Phantom.t)
[%%expect{|
Line 1, characters 7-26:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
           ^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 30-31:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
                                  ^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 30-31:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
                                  ^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 30-31:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
                                  ^
Error: The value "x" has type "int Phantom.t" = "int"
       but an expression was expected of type "float Phantom.t" = "int"
       Type "int" is not compatible with type "float"
|}, Principal{|
Line 1, characters 7-26:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
           ^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 30-31:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
                                  ^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 1, characters 29-50:
1 | let __ (x : int Phantom.t) = (x : float Phantom.t)
                                 ^^^^^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

val __ : int -> int = <fun>
|}] (* bug: apart from the noise from the previous bugs, the divergence between principal
       and non-principal is weird. *)

(* When user code has type errors, we probably don't want to emit random deprecation
   warnings. *)

let f (_ : M.t) (_ : _ Queue.t) = ()
let () = f (Queue.create ()) M.zero

[%%expect{|
val f : M.t -> 'a Queue.t -> unit = <fun>
Line 2, characters 11-28:
2 | let () = f (Queue.create ()) M.zero
               ^^^^^^^^^^^^^^^^^
Alert deprecated: implicitly converting between a type and its deprecated representation

Line 2, characters 11-28:
2 | let () = f (Queue.create ()) M.zero
               ^^^^^^^^^^^^^^^^^
Error: This expression has type "'a Queue.t"
       but an expression was expected of type "M.t" = "int"
|}]
(* bug: we should probably delay emitting the warnings until the surrounding structure
   item or compilation unit types, or something. *)
