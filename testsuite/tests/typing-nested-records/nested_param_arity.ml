(* TEST
 expect;
*)

(* A nested type selects its parameters by the source positions of the
   parent parameters that its fields reference.  The recursive
   placeholder and the final declaration must use the same positions, so
   a projected type has one arity during and after the translation of
   its recursive group. *)

(* ===== Reported alias case ===== *)

(* The alias unifies both parent parameters during translation.  The
   placeholder and the final declaration both keep one parameter: the
   position of ['b], the only parameter the source references.  This
   used to crash constraint checking with
   [Invalid_argument("List.exists2")]. *)
type ('a, 'b) t = {
  n : { value : ('b as 'a) };
  recursive_use : int t.n option;
}
[%%expect{|
type ('a, 'b) t = { n : { value : 'a; }; recursive_use : int t.n option; }
  constraint 'b = 'a
|}]

(* The projected type keeps the same arity after the recursive group. *)
let after_group (x : int t.n) : int = x.value
[%%expect{|
val after_group : int t.n -> int = <fun>
|}]

(* ===== Wrong arity is a normal diagnostic ===== *)

let wrong_arity (x : (int, string) t.n) = x.value
[%%expect{|
Line 1, characters 21-38:
1 | let wrong_arity (x : (int, string) t.n) = x.value
                         ^^^^^^^^^^^^^^^^^
Error: The type constructor "t.n" expects 1 argument(s),
       but is here applied to 2 argument(s)
|}]

(* ===== Alias that unifies two parent parameters, without recursion ===== *)

type ('a, 'b) unified = {
  u : { first : 'a; second : ('a as 'b) };
}
[%%expect{|
type ('a, 'b) unified = { u : { first : 'a; second : 'a; }; }
  constraint 'b = 'a
|}]

let use_unified (x : int unified.u) = (x.first, x.second)
[%%expect{|
val use_unified : int unified.u -> int * int = <fun>
|}]

(* ===== [Ptyp_poly] shadowing a parent parameter name ===== *)

(* The locally bound ['a] shadows the parent parameter, so the nested
   type has no parameters. *)
type 'a shadowed = {
  s : { apply : 'a. 'a -> int };
}
[%%expect{|
type 'a shadowed = { s : { apply : 'a0. 'a0 -> int; }; }
|}]

let use_shadowed (x : shadowed.s) = x.apply ()
[%%expect{|
val use_shadowed : shadowed.s -> int = <fun>
|}]

(* The bound name shadows only itself: ['b] is still selected. *)
type ('a, 'b) partly_shadowed = {
  s : { apply : 'a. 'a -> 'b };
  loop : bool partly_shadowed.s option;
}
[%%expect{|
type ('a, 'b) partly_shadowed = {
  s : { apply : 'a0. 'a0 -> 'b; };
  loop : bool partly_shadowed.s option;
}
|}]

let use_partly_shadowed (x : int partly_shadowed.s) : int = x.apply ()
[%%expect{|
val use_partly_shadowed : int partly_shadowed.s -> int = <fun>
|}]

(* ===== Unused and phantom parent parameters ===== *)

(* The phantom parameter is dropped from the projection, in the
   recursive use and afterwards. *)
type ('a, 'phantom) with_phantom = {
  p : { value : 'a };
  again : int with_phantom.p option;
}
[%%expect{|
type ('a, 'phantom) with_phantom = {
  p : { value : 'a; };
  again : int with_phantom.p option;
}
|}]

let use_phantom (x : (int, string) with_phantom) : int with_phantom.p = x.p
[%%expect{|
val use_phantom : (int, string) with_phantom -> int with_phantom.p = <fun>
|}]

(* A nested type that references no parent parameter has no
   parameters. *)
type ('a, 'b) all_unused = {
  ground : { flag : bool };
  tie : all_unused.ground option;
}
[%%expect{|
type ('a, 'b) all_unused = {
  ground : { flag : bool; };
  tie : all_unused.ground option;
}
|}]

(* ===== Two-level nesting: order stays parent order at each level ===== *)

(* The outer nested type references ['c] before ['b] in field order, but
   both levels keep parent declaration order: ('b, 'c). *)
type ('a, 'b, 'c) three = {
  outer : { left : 'c; inner : { pair : 'b * 'c } };
}
[%%expect{|
type ('a, 'b, 'c) three = {
  outer : { left : 'c; inner : { pair : 'b * 'c; }; };
}
|}]

(* left = 'c: with arguments (string, bool), left is bool.  This fails
   to type-check if the parameters were in field traversal order. *)
let outer_order (x : (string, bool) three.outer) : bool = x.left
[%%expect{|
val outer_order : (string, bool) three.outer -> bool = <fun>
|}]

let inner_order (x : (string, bool) three.outer.inner) : string * bool =
  x.pair
[%%expect{|
val inner_order : (string, bool) three.outer.inner -> string * bool = <fun>
|}]

(* ===== Mutually recursive declarations using projected types ===== *)

type ('a, 'b) left = {
  payload : { this : 'a };
  other : int right.wrapped option;
}
and 'c right = {
  wrapped : { that : 'c };
  back : string left.payload option;
}
[%%expect{|
type ('a, 'b) left = {
  payload : { this : 'a; };
  other : int right.wrapped option;
}
and 'c right = {
  wrapped : { that : 'c; };
  back : string left.payload option;
}
|}]

let cross (l : (string, unit) left) (r : int right) =
  (l.other, r.back)
[%%expect{|
val cross :
  (string, unit) left ->
  int right -> int right.wrapped option * string left.payload option = <fun>
|}]
