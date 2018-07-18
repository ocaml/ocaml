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
Line _, characters 8-11:
  let _ = X.x;;
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
Line _, characters 8-11:
  let _ = X.z;;
          ^^^
Error (alert baz): X.z
Baz!
|}]

let _ = X.t;;
[%%expect{|
Line _, characters 8-11:
  let _ = X.t;;
          ^^^
Error (alert baz): X.t
BAZ
Line _, characters 8-11:
  let _ = X.t;;
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
Line _, characters 6-7:
  end = X;;
        ^
Alert foo: x
Foo!
  Line _, characters 2-33:
    val x: int [@@alert foo "Foo!"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val x: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Error (alert baz): z
Baz!
  Line _, characters 2-33:
    val z: int [@@alert baz "Baz!"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val z: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Error (alert baz): t
BAZ
  Line _, characters 2-72:
    val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val t: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Alert foo: t
FOO
  Line _, characters 2-72:
    val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val t: int
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
Line _, characters 6-7:
  end = X;;
        ^
Alert foo: x
Foo!
  Line _, characters 2-33:
    val x: int [@@alert foo "Foo!"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val x: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Alert baz: z
Baz!
  Line _, characters 2-33:
    val z: int [@@alert baz "Baz!"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val z: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Alert baz: t
BAZ
  Line _, characters 2-72:
    val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val t: int
    ^^^^^^^^^^
  Expected signature
Line _, characters 6-7:
  end = X;;
        ^
Alert foo: t
FOO
  Line _, characters 2-72:
    val t: int [@@alert foo "FOO"] [@@alert bar "BAR"] [@@alert baz "BAZ"]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Definition
  Line _, characters 2-12:
    val t: int
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
