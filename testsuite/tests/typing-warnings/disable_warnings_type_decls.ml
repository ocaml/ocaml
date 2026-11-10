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

type[@warning "-30"] a = X and[@warning "-30"] b = X;;


[%%expect {|
type a = X
and b = X
|}];;

type a = X and b = X [@@warning "-30"];;

[%%expect {|
type a = X
and b = X
|}];;


type[@warning "-30"] a = { k: unit } and[@warning "-30"] b = { k: unit };;

[%%expect {|
type a = { k : unit; }
and b = { k : unit; }
|}];;

type a = { k: unit } and b = { k: unit } [@@warning "-30"];;

[%%expect {|
type a = { k : unit; }
and b = { k : unit; }
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
type t = ()
|}]

type t = ()[@@warning "-65"];;

[%%expect{|
type t = ()
|}]
