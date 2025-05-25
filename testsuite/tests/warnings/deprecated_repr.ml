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
|}]

(* ############## *)
(* Converting between int and t should trigger warnings, but simply using
   values of type t shouldn't. *)

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

(* ############## *)
(* Converting between M.t and u shouldn't trigger warnings, since this is correct
   regardless of t's representation. This implies that the typer shoudn't merely
   expands all head type constructors during unification and other other operations. *)

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

(* ############## *)

(* More cases where a type constructor may get expanded, in types. *)

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

(* More cases where a type constructor may get expanded, in modules. *)

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

(* More cases where a type constructor may get expanded, in pattern. *)
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

(* More cases where a type constructor may get expanded, filling in of omitted
   optional parameter. *)

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
