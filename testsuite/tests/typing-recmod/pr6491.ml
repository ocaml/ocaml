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
Error: Illegal recursive module reference
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
Error: Illegal recursive module reference
|}]

(* PR 6491 *)
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
