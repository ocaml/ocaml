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
module M : sig end
|}];;
