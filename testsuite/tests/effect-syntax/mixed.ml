(* TEST
expect;
*)

type _ eff += A: int -> int eff | B: int -> int eff

exception Exn of int


let test f =
  match f () with
  | n | exception (Exn n)
  | effect A n, _ | effect B n, _ -> n
[%%expect {|
type _ eff += A : int -> int eff | B : int -> int eff
exception Exn of int
val test : (unit -> int) -> int = <fun>
|}]

let a = test ignore
[%%expect{|
Line 1, characters 13-19:
1 | let a = test ignore
                 ^^^^^^
Error: This expression has type "unit -> unit"
       but an expression was expected of type "unit -> int"
       Type "unit" is not compatible with type "int"
|}]

let b = test (fun () -> raise Exn 1)
[%%expect{|
Line 1, characters 34-35:
1 | let b = test (fun () -> raise Exn 1)
                                      ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.

Line 1, characters 30-33:
1 | let b = test (fun () -> raise Exn 1)
                                  ^^^
Error: The constructor "Exn" expects 1 argument(s),
       but is applied here to 0 argument(s)
|}]

let b = test (fun () -> Effect.perform (A 2))
[%%expect{|
val b : int = 2
|}]

let c = test (fun () -> Effect.perform (B 3))
[%%expect{|
val c : int = 3
|}]
