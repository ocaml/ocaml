(* TEST
 expect;
*)

(* Recursive placeholders and final declarations must select the same parent parameter positions. *)

(* Aliases must not change projected arity during recursive translation. *)

type ('a, 'b) t = {
  n : { value : ('b as 'a) };
  recursive_use : int t.n option;
}
[%%expect{|
type ('a, 'b) t = { n : { value : 'a; }; recursive_use : int t.n option; }
  constraint 'b = 'a
|}]

let after_group (x : int t.n) : int = x.value
[%%expect{|
val after_group : int t.n -> int = <fun>
|}]

(* Wrong arity reports an error. *)

let wrong_arity (x : (int, string) t.n) = x.value
[%%expect{|
Line 1, characters 21-38:
1 | let wrong_arity (x : (int, string) t.n) = x.value
                         ^^^^^^^^^^^^^^^^^
Error: The type constructor "t.n" expects 1 argument(s),
       but is here applied to 2 argument(s)
|}]

(* Aliases without recursion use the same rule. *)

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

(* Locally quantified variables do not select same-named parent parameters. *)

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

(* Only the locally bound name is shadowed. *)
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

(* Unused parent parameters are not selected. *)
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

(* A projection with no parent references has arity zero. *)
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

(* Parameter order follows the parent declaration. *)
type ('a, 'b, 'c) three = {
  outer : { left : 'c; inner : { pair : 'b * 'c } };
}
[%%expect{|
type ('a, 'b, 'c) three = {
  outer : { left : 'c; inner : { pair : 'b * 'c; }; };
}
|}]

let outer_order (x : (string, bool) three.outer) : bool = x.left
[%%expect{|
val outer_order : (string, bool) three.outer -> bool = <fun>
|}]

let inner_order (x : (string, bool) three.outer.inner) : string * bool =
  x.pair
[%%expect{|
val inner_order : (string, bool) three.outer.inner -> string * bool = <fun>
|}]

(* Mutual recursion uses the same selection rule. *)

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
