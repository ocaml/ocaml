(* TEST
   * expect
*)

module M : sig
  type 'a t := [< `A ] as 'a
  val f : 'a -> 'a t
end = struct
  let f x = x
end;;
[%%expect{|
Line 2, characters 2-28:
2 |   type 'a t := [< `A ] as 'a
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Destructive substitutions are not supported for constrained
       types (other than when replacing a type constructor with
       a type constructor with the same arguments).
|}]

type foo = { foo : 'a. ([< `A] as 'a) -> 'a }

module Foo (X : sig type 'a t := [< `A ] as 'a type foo2 = foo = { foo : 'a. 'a t -> 'a t } end) = struct
    let f { X.foo } = foo
end;;
[%%expect{|
type foo = { foo : 'a. ([< `A ] as 'a) -> 'a; }
Line 3, characters 20-46:
3 | module Foo (X : sig type 'a t := [< `A ] as 'a type foo2 = foo = { foo : 'a. 'a t -> 'a t } end) = struct
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Destructive substitutions are not supported for constrained
       types (other than when replacing a type constructor with
       a type constructor with the same arguments).
|}]

type bar = { bar : 'a. ([< `A] as 'a) -> 'a }

module Bar (X : sig type 'a t := 'a type bar2 = bar = { bar : 'a. ([< `A] as 'a) t -> 'a t } end) = struct
  let f { X.bar } = bar
end;;
[%%expect{|
type bar = { bar : 'a. ([< `A ] as 'a) -> 'a; }
module Bar :
  functor
    (X : sig type bar2 = bar = { bar : 'a. ([< `A ] as 'a) -> 'a; } end) ->
    sig val f : X.bar2 -> ([< `A ] as 'a) -> 'a end
|}]
