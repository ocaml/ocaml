(* TEST
   * expect
*)

module rec A: sig val x: int end = struct let x = B.x end
and B:sig val x: int end = struct let x = E.y end
and C:sig val x: int end = struct let x = B.x end
and D:sig val x: int end = struct let x = C.x end
and E:sig val x: int val y:int end = struct let x = D.x let y = 0 end
[%%expect {|
Line 2, characters 27-49:
2 | and B:sig val x: int end = struct let x = E.y end
                               ^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: B -> E -> D -> C -> B.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 10-20:
2 | and B:sig val x: int end = struct let x = E.y end
              ^^^^^^^^^^
  Module B defines an unsafe value, x .
Line 5, characters 10-20:
5 | and E:sig val x: int val y:int end = struct let x = D.x let y = 0 end
              ^^^^^^^^^^
  Module E defines an unsafe value, x .
Line 4, characters 10-20:
4 | and D:sig val x: int end = struct let x = C.x end
              ^^^^^^^^^^
  Module D defines an unsafe value, x .
Line 3, characters 10-20:
3 | and C:sig val x: int end = struct let x = B.x end
              ^^^^^^^^^^
  Module C defines an unsafe value, x .
|}]

type t = ..
module rec A: sig type t += A end = struct type t += A = B.A end
and B:sig type t += A end = struct type t += A = A.A end
[%%expect {|
type t = ..
Line 2, characters 36-64:
2 | module rec A: sig type t += A end = struct type t += A = B.A end
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 28-29:
2 | module rec A: sig type t += A end = struct type t += A = B.A end
                                ^
  Module A defines an unsafe extension constructor, A .
Line 3, characters 20-21:
3 | and B:sig type t += A end = struct type t += A = A.A end
                        ^
  Module B defines an unsafe extension constructor, A .
|}]


module rec A: sig
  module F: functor(X:sig end) -> sig end
  val f: unit -> unit
end = struct
  module F(X:sig end) = struct end
  let f () = B.value
end
and B: sig val value: unit end = struct let value = A.f () end
[%%expect {|
Lines 4-7, characters 6-3:
4 | ......struct
5 |   module F(X:sig end) = struct end
6 |   let f () = B.value
7 | end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 2, characters 2-41:
2 |   module F: functor(X:sig end) -> sig end
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Module A defines an unsafe functor, F .
Line 8, characters 11-26:
8 | and B: sig val value: unit end = struct let value = A.f () end
               ^^^^^^^^^^^^^^^
  Module B defines an unsafe value, value .
|}]


module F(X: sig module type t module M: t end) = struct
  module rec A: sig
    module M: X.t
    val f: unit -> unit
  end = struct
    module M = X.M
    let f () = B.value
  end
  and B: sig val value: unit end = struct let value  = A.f () end
end
[%%expect {|
Lines 5-8, characters 8-5:
5 | ........struct
6 |     module M = X.M
7 |     let f () = B.value
8 |   end
Error: Cannot safely evaluate the definition of the following cycle
       of recursively-defined modules: A -> B -> A.
       There are no safe modules in this cycle (see manual section 12.2).
Line 3, characters 4-17:
3 |     module M: X.t
        ^^^^^^^^^^^^^
  Module A defines an unsafe module, M .
Line 9, characters 13-28:
9 |   and B: sig val value: unit end = struct let value  = A.f () end
                 ^^^^^^^^^^^^^^^
  Module B defines an unsafe value, value .
|}]


module rec M: sig val f: unit -> int end = struct let f () = N.x end
and N:sig val x: int end = struct let x = M.f () end;;
[%%expect {|
Exception: Undefined_recursive_module ("", 1, 43).
|}]
