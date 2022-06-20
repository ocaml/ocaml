(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

(* Ignore OCAMLRUNPARAM=b to be reproducible *)
Printexc.record_backtrace false;;
[%%expect {|
- : unit = ()
|}]

let _ = Array.get;;
[%%expect {|
- : 'a array -> int -> 'a = <fun>
|}]

let _ = Array.get [||];;
[%%expect {|
Line 1, characters 8-22:
1 | let _ = Array.get [||];;
            ^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
- : int -> 'a = <fun>
|}]

let () = ignore Array.get;;
[%%expect {|
|}]

let () = ignore (Array.get [||]);;
[%%expect {|
Line 1, characters 16-32:
1 | let () = ignore (Array.get [||]);;
                    ^^^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
|}]


let _ = if true then Array.get else (fun _ _ -> 12);;
[%%expect {|
- : int array -> int -> int = <fun>
|}]

let _ = if true then Array.get [||] else (fun _ -> 12);;
[%%expect {|
Line 1, characters 21-35:
1 | let _ = if true then Array.get [||] else (fun _ -> 12);;
                         ^^^^^^^^^^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
- : int -> int = <fun>
|}]

let _ = (if true then Array.get [||] else (fun _ -> 12) : _ -> _);;
[%%expect {|
- : int -> int = <fun>
|}]

type t = {r: int -> int -> int}

let f x = let _ = x.r in ();;
[%%expect {|
type t = { r : int -> int -> int; }
val f : t -> unit = <fun>
|}]

let f x = let _ = x.r 1 in ();;
[%%expect {|
Line 1, characters 18-23:
1 | let f x = let _ = x.r 1 in ();;
                      ^^^^^
Warning 5 [ignored-partial-application]: this function application is partial,
maybe some arguments are missing.
val f : t -> unit = <fun>
|}]

let _ = raise Exit 3;;
[%%expect {|
Line 1, characters 19-20:
1 | let _ = raise Exit 3;;
                       ^
Warning 20 [ignored-extra-argument]: this argument will not be used by the function.
Exception: Stdlib.Exit.
|}]

let f a b = a + b;;
[%%expect {|
val f : int -> int -> int = <fun>
|}]
let g x = x + 1
let _ = g (f 1);;
[%%expect {|
val g : int -> int = <fun>
Line 2, characters 10-15:
2 | let _ = g (f 1);;
              ^^^^^
Error: This expression has type int -> int
       but an expression was expected of type int
  Hint: This function application is partial,
  maybe some arguments are missing.
|}]
