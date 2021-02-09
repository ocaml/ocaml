(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

module A = struct type foo end;;
[%%expect {|
module A : sig type foo end
|}]

module rec B : sig
  open A
  type bar = Bar of foo
end = B;;
[%%expect {|
module rec B : sig type bar = Bar of A.foo end
|}]

module rec C : sig
  open A
end = C;;
[%%expect {|
Line 2, characters 2-8:
2 |   open A
      ^^^^^^
Warning 33 [unused-open]: unused open A.
module rec C : sig end
|}]

module rec D : sig
  module M : module type of struct
    module X : sig end = struct
      open A
      let None = None
    end
  end
end = D;;
[%%expect {|
Line 5, characters 10-14:
5 |       let None = None
              ^^^^
Warning 8 [partial-match]: this pattern-matching is not exhaustive.
Here is an example of a case that is not matched:
Some _
Line 4, characters 6-12:
4 |       open A
          ^^^^^^
Warning 33 [unused-open]: unused open A.
module rec D : sig module M : sig module X : sig end end end
|}]
