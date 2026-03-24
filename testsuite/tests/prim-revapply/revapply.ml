(* TEST
 flags = "-w +48";
 expect;
*)

external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let f x = x + x
let g x = x * x
let h x = x + 1
let add x y = x + y
[%%expect{|
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
val f : int -> int = <fun>
val g : int -> int = <fun>
val h : int -> int = <fun>
val add : int -> int -> int = <fun>
|}]

let _ =
  List.iter (Format.printf "%d@.")
    [
      3 |> f; (* 6 *)
      3 |> f |> g; (* 36 *)
      3 |> g |> f; (* 18 *)
      3 |> f |> g |> h; (* 37 *)
      3 |> add 2 |> add 3 |> f |> g |> add 4; (* 260 *)
    ]
[%%expect{|
6
36
18
37
260
- : unit = ()
|}]


(* PR#10081 *)
let bump ?(cap = 100) x = min cap (x + 1)
let _f x = x |> bump (* no warning 48 *)
[%%expect{|
val bump : ?cap:int -> int -> int = <fun>
val _f : int -> int = <fun>
|}]

(* PR#10081 *)
type t = A | B
type s = A | B
let _f (x : t) = x |> function A -> 0 | B -> 1
[%%expect{|
type t = A | B
type s = A | B
val _f : t -> int = <fun>
|}, Principal{|
type t = A | B
type s = A | B
Line 3, characters 31-32:
3 | let _f (x : t) = x |> function A -> 0 | B -> 1
                                   ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

Line 3, characters 40-41:
3 | let _f (x : t) = x |> function A -> 0 | B -> 1
                                            ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not
  principal.

val _f : t -> int = <fun>
|}]

(* Abstract functions *)
let _ =
  let module A:sig
    type f
    type x
    val succ: f
    val zero:x
    external (|>): x -> f -> int = "%revapply"
  end = struct
    type f = int -> int
    type x = int
    let succ = succ
    let zero = 0
    external (|>): x -> f -> int = "%revapply"
  end in
  A.(zero |> succ)
[%%expect{|
- : int = 1
|}]

(* is_inferred *)
let f ?x ~y () = ()
let () = () |> Stdlib.(f ~y:0)
let () = () |> (let type u = A in f ~y:0)
[%%expect{|
val f : ?x:'a -> y:'b -> unit -> unit = <fun>
Line 3, characters 34-40:
3 | let () = () |> (let type u = A in f ~y:0)
                                      ^^^^^^
Error: This expression has type "?x:'a -> unit -> unit"
       but an expression was expected of type "unit -> 'b"
       The first argument is labeled "?x",
       but an unlabeled argument was expected
Hint: This function application is partial, maybe some arguments are missing.
|}]
