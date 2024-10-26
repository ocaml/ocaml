(* TEST
   expect;
*)

type t = Float of float [@unboxed] | Foo
[%%expect {|
Line 1, characters 0-40:
1 | type t = Float of float [@unboxed] | Foo
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type declaration is non-separated, it may contain both float and
       non-float values.
|}]

type abstract [@@shape [abstract]]
type t = Float of float [@unboxed] | Abstract of abstract [@unboxed]
[%%expect {|
type abstract [@@shape [abstract]]
Line 2, characters 0-68:
2 | type t = Float of float [@unboxed] | Abstract of abstract [@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type declaration is non-separated, it may contain both float and
       non-float values.
|}]

(* This is unexpected, we need such GADTs to be rejected
   as they also break separability. *)
type t = Any : 'a -> t [@unboxed]
[%%expect {|
type t = Any : 'a -> t [@unboxed]
|}]

type 'a id1 = 'a
type 'a id2 = Id of 'a [@@unboxed]
type 'a id3 = Id of 'a [@unboxed]
[%%expect {|
type 'a id1 = 'a
type 'a id2 = Id of 'a [@@unboxed]
type 'a id3 = Id of 'a [@unboxed]
|}]

(* The following definitions must also be rejected. *)
type any1 = Any : 'a id1 -> any1 [@unboxed]
(* This is unexpected and unsound. *)
[%%expect {|
type any1 = Any : 'a id1 -> any1 [@unboxed]
|}]

type any1' = Any : 'a id1 -> any1' [@@unboxed]
[%%expect {|
Line 1, characters 0-46:
1 | type any1' = Any : 'a id1 -> any1' [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type any2 = Any : 'a id2 -> any2 [@unboxed]
(* This is unexpected and unsound. *)
[%%expect {|
type any2 = Any : 'a id2 -> any2 [@unboxed]
|}]

type any2' = Any : 'a id2 -> any2' [@@unboxed]
[%%expect {|
Line 1, characters 0-46:
1 | type any2' = Any : 'a id2 -> any2' [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

type any3 = Any : 'a id3 -> any3 [@unboxed]
(* This is unexpected and unsound. *)
[%%expect {|
type any3 = Any : 'a id3 -> any3 [@unboxed]
|}]

type any3' = Any : 'a id3 -> any3' [@@unboxed]
(* This is unexpected and unsound. *)
[%%expect {|
type any3' = Any : 'a id3 -> any3' [@@unboxed]
|}]


(* In this case the type ['a unknown] is abstract,
   the example must be rejected as it could be defined 
   as the identities above. *)
type 'a unknown
type t = Unk : 'a unknown -> t [@unboxed]
(* This is unexpected and unsound. *)
[%%expect {|
type 'a unknown
type t = Unk : 'a unknown -> t [@unboxed]
|}]


(* Using a more precise abstract type, that rules out the identity,
   is correct. *)
type 'a unknown_function [@@shape [\#function]]
type t = Unf : 'a unknown_function -> t [@unboxed]
[%%expect {|
type 'a unknown_function [@@shape [function]]
type t = Unf : 'a unknown_function -> t [@unboxed]
|}]
