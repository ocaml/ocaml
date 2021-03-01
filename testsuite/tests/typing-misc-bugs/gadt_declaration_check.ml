(* TEST
   * expect
*)
type foo = Foo;;
[%%expect{|
type foo = Foo
|}];;

(* this should fail with an error message,
   not an uncaught exception (as it did temporarily
   during the development of typedecl_separability) *)
type bar = Bar : foo;;
[%%expect{|
Line 1, characters 17-20:
1 | type bar = Bar : foo;;
                     ^^^
Error: Constraints are not satisfied in this type.
       Type foo should be an instance of bar
|}];;
