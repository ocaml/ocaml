(* TEST
 expect;
*)

(* Cyclic nested field types must print without looping. *)
type t = { a : { x : [`A of 'b] as 'b } }
[%%expect{|
type t = { a : { x : [ `A of 'a ] as 'a; }; }
|}]

(* This also works at greater depth. *)
type u = { outer : { inner : { y : [`B of 'c] as 'c } } }
[%%expect{|
type u = { outer : { inner : { y : [ `B of 'a ] as 'a; }; }; }
|}]

(* Preserve variant constructor order. *)
type v = { tag : { k : [`First | `Second | `Third] } }
[%%expect{|
type v = { tag : { k : [ `First | `Second | `Third ]; }; }
|}]

(* Preserve ordinary and constructor records. *)
type plain = { p : int; q : string }
type cstr = C of { r : int; s : bool }
[%%expect{|
type plain = { p : int; q : string; }
type cstr = C of { r : int; s : bool; }
|}]

(* Error printing must terminate too. *)
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
