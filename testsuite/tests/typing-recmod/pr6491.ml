(* TEST
   * expect
*)

module rec Foo : sig class type c = object method x : int end end = Foo
and Bar : sig class type c = object inherit Foo.c end end = Bar
and Baz : sig class type c = object inherit Bar.c end end = Baz;;
[%%expect {|
module rec Foo : sig class type c = object method x : int end end
and Bar : sig class type c = object method x : int end end
and Baz : sig class type c = object  end end
|}]

module rec Foo : sig class type c = object method x : int end end = Foo
and Bar : sig class type c = Foo.c end = Bar
and Baz : sig class type c = Bar.c end = Baz

let foo (x : Foo.c) = x#x
let bar (x : Bar.c) = x#x
let baz (x : Baz.c) = x#x;;
[%%expect{|
module rec Foo : sig class type c = object method x : int end end
and Bar : sig class type c = Foo.c end
and Baz : sig class type c = Bar.c end
val foo : Foo.c -> int = <fun>
val bar : Bar.c -> int = <fun>
Line 7, characters 22-23:
7 | let baz (x : Baz.c) = x#x;;
                          ^
Error: This expression has type Baz.c
       It has no method x
|}]
