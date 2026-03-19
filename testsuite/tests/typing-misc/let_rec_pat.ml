(* TEST
 expect;
*)

(* This series of tests checks the pattern validation for [let rec] bindings.
   Only "variable-like" patterns are allowed. See [is_var_pat] for details. *)

(* Valid patterns *)

let rec x = 1 in x
[%%expect{|
- : int = 1
|}];;

let rec (x : int) = 1 in x
[%%expect{|
- : int = 1
|}];;

let rec ((x : int) : int) = 1 in x
[%%expect{|
- : int = 1
|}];;

module M = struct type t = int end;;
let rec M.((x : t)) = 1 in x
[%%expect{|
module M : sig type t = int end
- : M.t = 1
|}];;

(* Invalid patterns *)

let rec _ = 1 in ()
[%%expect{|
Line 1, characters 8-9:
1 | let rec _ = 1 in ()
            ^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec (x as y) = 1 in x
[%%expect{|
Line 1, characters 8-16:
1 | let rec (x as y) = 1 in x
            ^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec 42 = 42 in ()
[%%expect{|
Line 1, characters 8-10:
1 | let rec 42 = 42 in ()
            ^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec (x, y) = (1, 2) in x
[%%expect{|
Line 1, characters 8-14:
1 | let rec (x, y) = (1, 2) in x
            ^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec Some x = Some 1 in x
[%%expect{|
Line 1, characters 8-14:
1 | let rec Some x = Some 1 in x
            ^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

type r = { a : int; b : int };;
let rec { a; b } = { a = 1; b = 2 } in a
[%%expect{|
type r = { a : int; b : int; }
Line 2, characters 8-16:
2 | let rec { a; b } = { a = 1; b = 2 } in a
            ^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec [| x |] = [| 1 |] in x
[%%expect{|
Line 1, characters 8-15:
1 | let rec [| x |] = [| 1 |] in x
            ^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec (Some x | None as x) = None in ()
[%%expect{|
Line 1, characters 8-28:
1 | let rec (Some x | None as x) = None in ()
            ^^^^^^^^^^^^^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec lazy x = lazy 1 in x
[%%expect{|
Line 1, characters 8-14:
1 | let rec lazy x = lazy 1 in x
            ^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

module type S = sig val f : int -> int end;;
let rec (module M : S) = (module struct let f n = if n <= 0 then 1 else n * M.f (n - 1) end : S) in M.f 5
[%%expect{|
module type S = sig val f : int -> int end
Line 2, characters 8-22:
2 | let rec (module M : S) = (module struct let f n = if n <= 0 then 1 else n * M.f (n - 1) end : S) in M.f 5
            ^^^^^^^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

type t = [ `A ];;
let rec #t = `A in ()
[%%expect{|
type t = [ `A ]
Line 2, characters 8-10:
2 | let rec #t = `A in ()
            ^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec ((x, y) : int * int) = (1, 2) in x
[%%expect{|
Line 1, characters 8-28:
1 | let rec ((x, y) : int * int) = (1, 2) in x
            ^^^^^^^^^^^^^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec M.(Some (x : t)) = Some 1 in x
[%%expect{|
Line 1, characters 8-24:
1 | let rec M.(Some (x : t)) = Some 1 in x
            ^^^^^^^^^^^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;

let rec x = a and (a, b) = (1, 2) in x
[%%expect{|
Line 1, characters 18-24:
1 | let rec x = a and (a, b) = (1, 2) in x
                      ^^^^^^
Error: Only variables are allowed as left-hand side of "let rec"
|}];;
