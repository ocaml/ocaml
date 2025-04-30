(* TEST
   expect;
*)

let unreferenced_getter = (.x)
[%%expect{|
Line 1, characters 28-29:
1 | let unreferenced_getter = (.x)
                                ^
Error: Unbound record field "x"
|}]

module M = struct
  type t = { x : int; f : 'a. 'a -> int -> 'a }
end
[%%expect{|
module M : sig type t = { x : int; f : 'a. 'a -> int -> 'a; } end
|}]

let m_f = (.M.f)
[%%expect{|
val m_f : M.t -> 'a -> int -> 'a = <fun>
|}]

let m_x = (.M.x)
[%%expect{|
val m_x : M.t -> int = <fun>
|}]

let m = { M.x = 5; f = (fun a _ -> a) }
[%%expect{|
val m : M.t = {M.x = 5; f = <fun>}
|}]

let f = m_f m
[%%expect{|
val f : '_weak1 -> int -> '_weak1 = <fun>
|}]

let f x y = m_f m x y
[%%expect{|
val f : 'a -> int -> 'a = <fun>
|}]

let _ = m_x m
[%%expect{|
- : int = 5
|}]

type unboxed = { u : string } [@@unboxed]
[%%expect{|
type unboxed = { u : string; } [@@unboxed]
|}]

let unboxed_u = (.u)
[%%expect{|
val unboxed_u : unboxed -> string = <fun>
|}]

let _ = unboxed_u { u = "hello" }
[%%expect{|
- : string = "hello"
|}]

type shared1 = { a : int; b : int }
type shared2 = { a : string; d : int }
[%%expect{|
type shared1 = { a : int; b : int; }
type shared2 = { a : string; d : int; }
|}]

let shared2_a = (.a)
[%%expect{|
val shared2_a : shared2 -> string = <fun>
|}]

let shared1_a = ((.a) : shared1 -> _)
[%%expect{|
val shared1_a : shared1 -> int = <fun>
|}]

let shared1_a = ((.a) : shared1 -> string)
[%%expect{|
Line 1, characters 17-21:
1 | let shared1_a = ((.a) : shared1 -> string)
                     ^^^^
Error: This expression has type "shared1 -> int"
       but an expression was expected of type "shared1 -> string"
       Type "int" is not compatible with type "string"
|}]

let _ = ((.a) : _ option -> _)
[%%expect{|
Line 1, characters 9-13:
1 | let _ = ((.a) : _ option -> _)
             ^^^^
Error: This expression has type "'a option" which is not a record type.
|}]

type foo

(* This is not an ideal error message, but the typechecker
   does not know that "foo" is not a record type for now. *)
let _ = ((.a) : foo -> _)
[%%expect{|
type foo
Line 5, characters 9-13:
5 | let _ = ((.a) : foo -> _)
             ^^^^
Error: This expression has type "shared2 -> string"
       but an expression was expected of type "foo -> 'a"
       Type "shared2" is not compatible with type "foo"
|}]

let _ = ((.undefined_field) : shared1 -> _)
[%%expect{|
Line 1, characters 11-26:
1 | let _ = ((.undefined_field) : shared1 -> _)
               ^^^^^^^^^^^^^^^
Error: This expression has type "shared1"
       There is no field "undefined_field" within type "shared1"
|}]

(* even if the field is known, type annotation takes precedence *)
let _ = ((.u) : shared1 -> _)
[%%expect{|
Line 1, characters 11-12:
1 | let _ = ((.u) : shared1 -> _)
               ^
Error: This expression has type "shared1"
       There is no field "u" within type "shared1"
|}]
