(* TEST
 expect;
*)

(* Regression tests for non-termination when printing declarations whose
   nested record fields contain cyclic types. The printer must prepare
   inner field types so that cycles print as finite [as 'a] aliases. *)

(* A cyclic polymorphic variant inside a nested record used to make the
   printer loop forever. *)
type t = { a : { x : [`A of 'b] as 'b } }
[%%expect{|
type t = { a : { x : [ `A of 'a ] as 'a; }; }
|}]

(* Same cyclic type, two nested records deep. *)
type u = { outer : { inner : { y : [`B of 'c] as 'c } } }
[%%expect{|
type u = { outer : { inner : { y : [ `B of 'a ] as 'a; }; }; }
|}]

(* Control: a non-cyclic polymorphic variant in a nested record prints
   with a stable constructor order. *)
type v = { tag : { k : [`First | `Second | `Third] } }
[%%expect{|
type v = { tag : { k : [ `First | `Second | `Third ]; }; }
|}]

(* Control: ordinary records and constructor records are unchanged. *)
type plain = { p : int; q : string }
type cstr = C of { r : int; s : bool }
[%%expect{|
type plain = { p : int; q : string; }
type cstr = C of { r : int; s : bool; }
|}]

(* An error message that prints a declaration containing the cyclic
   nested field must also terminate. *)
module M : sig
  type w = { a : { x : [`A of 'b] as 'b } }
end = struct
  type w = { a : { x : [`A of 'b] as 'b }; extra : int }
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type w = { a : { x : [`A of 'b] as 'b }; extra : int }
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig type w = { a : { x : [ `A of 'a ] as 'a; }; extra : int; } end
       is not included in
         sig type w = { a : { x : [ `A of 'a ] as 'a; }; } end
       Type declarations do not match:
         type w = { a : { x : [ `A of 'a ] as 'a; }; extra : int; }
       is not included in
         type w = { a : { x : [ `A of 'a ] as 'a; }; }
       An extra field, "extra", is provided in the first declaration.
|}]
