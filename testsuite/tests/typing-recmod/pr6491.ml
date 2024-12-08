(* TEST
 expect;
*)

module rec Foo : sig class type c = object method x : int end end = Foo
and Bar : sig class type c = object inherit Foo.c end end = Bar
and Baz : sig class type c = object inherit Bar.c end end = Baz;;
[%%expect {|
Line 2, characters 44-49:
2 | and Bar : sig class type c = object inherit Foo.c end end = Bar
                                                ^^^^^
Error: This class type is recursive. This use of the class type "Foo.c"
       from the recursive module "Foo" within the definition of
       the class type "c" in the recursive module "Bar"
       makes the module type of "Bar" depend on the module type of "Foo".
       Such recursive definitions of class types within recursive modules
       are not allowed.
|}]

module rec Foo : sig class type c = object inherit Foo.c end end = Foo;;
[%%expect {|
Line 1, characters 51-56:
1 | module rec Foo : sig class type c = object inherit Foo.c end end = Foo;;
                                                       ^^^^^
Error: This class type is recursive. This use of the class type "Foo.c"
       from the recursive module "Foo" within the definition of
       the class type "c" in the recursive module "Foo"
       makes the module type of "Foo" depend on itself.
       Such recursive definitions of class types within recursive modules
       are not allowed.
|}]

module rec Foo : sig class type c = object method x : int end end = Foo
and Bar : sig class type c = Foo.c end = Bar
and Baz : sig class type c = Bar.c end = Baz

let foo (x : Foo.c) = x#x
let bar (x : Bar.c) = x#x
let baz (x : Baz.c) = x#x;;
[%%expect{|
Line 2, characters 29-34:
2 | and Bar : sig class type c = Foo.c end = Bar
                                 ^^^^^
Error: This class type is recursive. This use of the class type "Foo.c"
       from the recursive module "Foo" within the definition of
       the class type "c" in the recursive module "Bar"
       makes the module type of "Bar" depend on the module type of "Foo".
       Such recursive definitions of class types within recursive modules
       are not allowed.
|}]

(* #12480 *)
module rec TypedGui : sig
  class type untyped =
    object
    end

  class type t =
    object
      inherit untyped
    end
end = TypedGui
[%%expect{|
module rec TypedGui :
  sig class type untyped = object  end class type t = object  end end
|}]
