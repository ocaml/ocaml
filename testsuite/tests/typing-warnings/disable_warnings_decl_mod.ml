(* TEST
 flags = " -w +A";
 expect;
*)

(* Alert deprecated *)

type t = A [@deprecated "blah"]

type[@warning "-3"] t2 = t = A;;

[%%expect {|
type t = A
type t2 = t = A
|}]

type t2 = t = A [@@warning "-3"];;

[%%expect {|
type t2 = t = A
|}]


(* Warning 30 *)

type[@warning "-30"] a = X and b = X;;


[%%expect {|
Line 1, characters 35-36:
1 | type[@warning "-30"] a = X and b = X;;
                                       ^
Warning 30 [duplicate-definitions]: the constructor "X" is defined in both types "a" and "b".

type a = X
and b = X
|}];;

type a = X and b = X [@@warning "-30"];;

[%%expect {|
Line 1, characters 19-20:
1 | type a = X and b = X [@@warning "-30"];;
                       ^
Warning 30 [duplicate-definitions]: the constructor "X" is defined in both types "a" and "b".

type a = X
and b = X
|}];;


type[@warning "-30"] a = { k: unit } and b = { k: unit };;

[%%expect {|
Line 1, characters 47-54:
1 | type[@warning "-30"] a = { k: unit } and b = { k: unit };;
                                                   ^^^^^^^
Warning 30 [duplicate-definitions]: the label "k" is defined in both types "a" and "b".

type a = { k : unit; }
and b = { k : unit; }
|}];;

type a = { k: unit } and b = { k: unit } [@@warning "-30"];;

[%%expect {|
Line 1, characters 31-38:
1 | type a = { k: unit } and b = { k: unit } [@@warning "-30"];;
                                   ^^^^^^^
Warning 30 [duplicate-definitions]: the label "k" is defined in both types "a" and "b".

type a = { k : unit; }
and b = { k : unit; }
|}];;

(* Warning 60 *)

module A = struct
  module type S = sig
    module Foo : sig end
  end
end;;
[%%expect{|
module A : sig module type S = sig module Foo : sig end end end
|}]

module type T = sig
  module G (X : A.S) : sig
    module[@warning "-60"] Bar := X.Foo
  end
end;;

[%%expect {|
Line 3, characters 4-39:
3 |     module[@warning "-60"] Bar := X.Foo
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 60 [unused-module]: unused module "Bar".

module type T = sig module G : (X : A.S) -> sig end end
|}];;


module type T = sig
  module G (X : A.S) : sig
    module Bar := X.Foo [@@warning "-60"]
  end
end;;

[%%expect {|
Line 3, characters 4-41:
3 |     module Bar := X.Foo [@@warning "-60"]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 60 [unused-module]: unused module "Bar".

module type T = sig module G : (X : A.S) -> sig end end
|}];;

(* Warning 62 *)

type[@warning "-62"] foo =
    Foo: 'b * 'b -> foo constraint 'b = [> `Bla ];;


[%%expect {|
type foo = Foo : 'b * 'b -> foo
|}];;

type foo =
    Foo: 'b * 'b -> foo constraint 'b = [> `Bla ] [@@warning "-62"];;

[%%expect {|
type foo = Foo : 'b * 'b -> foo
|}];;


(* Warning 65 *)

type[@warning "-65"] t = ();;

[%%expect{|
Line 1, characters 0-27:
1 | type[@warning "-65"] t = ();;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 65 [redefining-unit]: This type declaration is defining
  a new "()" constructor which shadows the existing one.
  Hint: Did you mean "type t = unit"?

type t = ()
|}]


type t = ()[@@warning "-65"];;

[%%expect{|
Line 1, characters 0-28:
1 | type t = ()[@@warning "-65"];;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 65 [redefining-unit]: This type declaration is defining
  a new "()" constructor which shadows the existing one.
  Hint: Did you mean "type t = unit"?

type t = ()
|}]

(* Warning 67 *)

module type[@warning "-67"] S = functor (Unused : sig end) -> sig end;;

[%%expect{|
module type S = (Unused : sig end) -> sig end
|}]


module type S = functor (Unused : sig end) -> sig end [@@warning "-67"];;

[%%expect{|
module type S = (Unused : sig end) -> sig end
|}]

(* Warning 69 *)

module Unused_record : sig end = struct
  type[@warning "-69"] t = { a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]


module Unused_record : sig end = struct
  type t = { a : int; b : int } [@@warning "-69"]
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]


module Unused_record : sig end = struct
  [@@@warning "-69"]
  type t = { a : int; b : int }
  let foo (x : t) = x
  let _ = foo
end;;

[%%expect{|
module Unused_record : sig end
|}]

(* Warning 73 *)

module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);;

module[@warning "-73"] M = F(struct end);;

[%%expect{|
module type S = sig val x : int end
val v : (module S) = <module>
module F : () -> S
module M : S
|}]

module M = F(struct end)[@@warning "-73"];;

[%%expect{|
module M : S
|}]
