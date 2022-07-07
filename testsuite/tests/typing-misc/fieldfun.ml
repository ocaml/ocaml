(* TEST
   * expect
*)

let f = (.no_such_field)
[%%expect {|
Line 1, characters 10-23:
1 | let f = (.no_such_field)
              ^^^^^^^^^^^^^
Error: Unbound record field no_such_field
|}]

let f = (.M.field)
[%%expect {|
Line 1, characters 10-17:
1 | let f = (.M.field)
              ^^^^^^^
Error: Unbound module M
|}]

type a = { a : int }
let get_a = (.a)
[%%expect {|
type a = { a : int; }
val get_a : a -> int = <fun>
|}]

let a = { a = 123 }
let _ = get_a a
let _ = (.a) a
[%%expect {|
val a : a = {a = 123}
- : int = 123
- : int = 123
|}]

(* With module *)
module M = struct
  type t = { value : string }

  module N = struct
    type u = { u : int }
  end
end;;

let mt = M.{ value = "hello" }
let str = (.M.value) mt
(* Local open *)
let str = M.(.value) mt
[%%expect {|
module M :
  sig
    type t = { value : string; }
    module N : sig type u = { u : int; } end
  end
val mt : M.t = {M.value = "hello"}
val str : string = "hello"
val str : string = "hello"
|}]

let u = M.N.{ u = 456 }
let _ = (.M.N.u) u
[%%expect {|
val u : M.N.u = {M.N.u = 456}
- : int = 456
|}]

(* With same field name *)
type a2 = { a : float }
let a2 : a2 = { a = 3.14 }
let _ = (.a) a2
(* With annotation *)
let _ = ((.a) : a -> int) a
[%%expect {|
type a2 = { a : float; }
val a2 : a2 = {a = 3.14}
- : float = 3.14
- : int = 123
|}]
