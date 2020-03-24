(* TEST
   * expect
*)

[@@@ocaml.warning "+a"]

module M : sig end = struct
  module rec Foo : sig
    type t
    val create : Bar.t -> t
  end = struct
    type t = unit

    let create _ = ()
  end

  and Bar : sig
    type t
  end = struct
    type t = unit
  end

  let _ = Foo.create
end;;
[%%expect{|
Line 14, characters 4-10:
14 |     type t
         ^^^^^^
Warning 34: unused type t.
Lines 13-17, characters 2-5:
13 | ..and Bar : sig
14 |     type t
15 |   end = struct
16 |     type t = unit
17 |   end
Warning 60: unused module Bar.
module M : sig end
|}];;
