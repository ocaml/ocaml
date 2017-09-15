type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq;;
[%%expect{|
type ('a, 'b) eq = Refl : ('a, 'a) eq
type empty = (int, string) eq
|}]

let f = function `Foo (_ : empty) -> .;;
[%%expect{|
val f : [< `Foo of empty ] -> 'a = <fun>
|}]

let f : [< `Foo of empty] -> int = function `Foo (_ : empty) -> .;;
[%%expect{|
val f : [< `Foo of empty ] -> int = <fun>
|}]

let f : [`Foo of empty] -> int = function `Foo (_ : empty) -> .;;
[%%expect{|
val f : [ `Foo of empty ] -> int = <fun>
|}]

let f : [< `Foo of empty] -> int = function `Foo (_ : empty) -> . | _ -> .;;
[%%expect{|
val f : [ `Foo of empty ] -> int = <fun>
|}]
