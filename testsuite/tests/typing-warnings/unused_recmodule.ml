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
Warning 34 [unused-type-declaration]: unused type t.
module M : sig end
|}];;
