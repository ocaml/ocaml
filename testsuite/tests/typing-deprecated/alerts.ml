(* TEST
   * expect
*)


(* Enable all alerts as errors, except foo (soft) and bar (disabled) *)
[@@@ocaml.alert "@all--foo-bar"];;

module X : sig
  val x: int [@@alert foo "Foo!"]
  val y: int [@@alert bar "Bar!"]
  val z: int [@@alert baz "Baz!"]
  val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
end = struct
  let x, y, z, t = 0, 0, 0, 0
end
[%%expect{|
module X : sig val x : int val y : int val z : int val t : int end
|}]

let _ = X.x;;
[%%expect{|
Line 1, characters 8-11:
1 | let _ = X.x;;
            ^^^
Alert foo: X.x
Foo!
- : int = 0
|}]

let _ = X.y;;
[%%expect{|
- : int = 0
|}]

let _ = X.z;;
[%%expect{|
Line 1, characters 8-11:
1 | let _ = X.z;;
            ^^^
Error (alert baz): X.z
Baz!
|}]

let _ = X.t;;
[%%expect{|
Line 1, characters 8-11:
1 | let _ = X.t;;
            ^^^
Error (alert baz): X.t
BAZ
Line 1, characters 8-11:
1 | let _ = X.t;;
            ^^^
Alert foo: X.t
FOO
|}]


module Z1 : sig
  val x: int [@@alert foo "Foo!"] [@@alert foo2 "Foo2"]
  val y: int [@@alert bar "Bar!"]
  val z: int [@@alert baz "Baz!"]
  val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
end = X;;
[%%expect{|
module Z1 : sig val x : int val y : int val z : int val t : int end
|}]

module Z2 : sig
  val x: int
  val y: int
  val z: int
  val t: int
end = X;;
[%%expect{|
Line 6, characters 6-7:
6 | end = X;;
          ^
Alert foo: x
Foo!
Line 4, characters 2-33:
4 |   val x: int [@@alert foo "Foo!"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 2, characters 2-12:
2 |   val x: int
      ^^^^^^^^^^
  Expected signature
Line 6, characters 6-7:
6 | end = X;;
          ^
Error (alert baz): z
Baz!
Line 6, characters 2-33:
6 |   val z: int [@@alert baz "Baz!"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 4, characters 2-12:
4 |   val z: int
      ^^^^^^^^^^
  Expected signature
Line 6, characters 6-7:
6 | end = X;;
          ^
Error (alert baz): t
BAZ
Line 7, characters 2-72:
7 |   val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 5, characters 2-12:
5 |   val t: int
      ^^^^^^^^^^
  Expected signature
Line 6, characters 6-7:
6 | end = X;;
          ^
Alert foo: t
FOO
Line 7, characters 2-72:
7 |   val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 5, characters 2-12:
5 |   val t: int
      ^^^^^^^^^^
  Expected signature
|}]

(* Turn all alerts into soft mode *)
[@@@ocaml.alert "--all"];;

module Z3 : sig
  val x: int
  val y: int
  val z: int
  val t: int
end = X;;
[%%expect{|
Line 8, characters 6-7:
8 | end = X;;
          ^
Alert foo: x
Foo!
Line 4, characters 2-33:
4 |   val x: int [@@alert foo "Foo!"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 4, characters 2-12:
4 |   val x: int
      ^^^^^^^^^^
  Expected signature
Line 8, characters 6-7:
8 | end = X;;
          ^
Alert baz: z
Baz!
Line 6, characters 2-33:
6 |   val z: int [@@alert baz "Baz!"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 6, characters 2-12:
6 |   val z: int
      ^^^^^^^^^^
  Expected signature
Line 8, characters 6-7:
8 | end = X;;
          ^
Alert baz: t
BAZ
Line 7, characters 2-72:
7 |   val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 7, characters 2-12:
7 |   val t: int
      ^^^^^^^^^^
  Expected signature
Line 8, characters 6-7:
8 | end = X;;
          ^
Alert foo: t
FOO
Line 7, characters 2-72:
7 |   val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
Line 7, characters 2-12:
7 |   val t: int
      ^^^^^^^^^^
  Expected signature
module Z3 : sig val x : int val y : int val z : int val t : int end
|}]


(* Disable all alerts *)
[@@@ocaml.alert "-all"];;

module Z4 : sig
  val x: int
  val y: int
  val z: int
  val t: int
end = X;;
[%%expect{|
module Z4 : sig val x : int val y : int val z : int val t : int end
|}]


(* Multiple alert messages of the same kind *)
[@@@ocaml.alert "+all--all"]
module X : sig
  val x: int [@@alert bla "X1"] [@@alert bla "X2"] [@@alert bla "X3"]
  val y: int [@@alert bla "X1"] [@@alert bla]  [@@alert bla "X3"]
  val z: int [@@alert bla] [@@alert bla] [@@alert bla]
end = struct
  let x, y, z = 0, 0, 0
end;;
let _ = X.x
let _ = X.y
let _ = X.z
[%%expect{|
module X : sig val x : int val y : int val z : int end
Line 9, characters 8-11:
9 | let _ = X.x
            ^^^
Alert bla: X.x
X1
X2
X3
- : int = 0
Line 10, characters 8-11:
10 | let _ = X.y
             ^^^
Alert bla: X.y
X1
X3
- : int = 0
Line 11, characters 8-11:
11 | let _ = X.z
             ^^^
Alert bla: X.z
- : int = 0
|}]


(* Invalid paylods *)
module X : sig
  val x: int [@@alert 42]
  val y: int [@@alert bla 42]
  val z: int [@@alert "bla"]
end = struct
  let x, y, z = 0, 0, 0
end
[%%expect{|
Line 2, characters 13-25:
2 |   val x: int [@@alert 42]
                 ^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'alert'.
Invalid payload
Line 3, characters 13-29:
3 |   val y: int [@@alert bla 42]
                 ^^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'alert'.
Invalid payload
Line 4, characters 13-28:
4 |   val z: int [@@alert "bla"]
                 ^^^^^^^^^^^^^^^
Warning 47 [attribute-payload]: illegal payload for attribute 'alert'.
Ill-formed list of alert settings
module X : sig val x : int val y : int val z : int end
|}]
