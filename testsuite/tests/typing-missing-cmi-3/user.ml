(* TEST

files = "original.ml middle.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "original.ml"
*** ocamlc.byte
module = "middle.ml"
**** script
script = "rm -f original.cmi"
***** expect
*)


#directory "ocamlc.byte";;
#load "middle.cmo"

let x:'a. 'a Middle.t =
  let _r = ref 0 in
  Middle.T
[%%expect {|
val x : 'a Middle.t = Middle.T
|}]


let () = Middle.(g x)
[%%expect {|
|}]

let () = Middle.(f x)
[%%expect {|
Line 1, characters 19-20:
1 | let () = Middle.(f x)
                       ^
Error: This expression has type (module Original.T)
       but an expression was expected of type
         (module Original.T with type t = int)
|}]

let () = Middle.f (module struct end)
[%%expect {|
Line 1, characters 26-36:
1 | let () = Middle.f (module struct end)
                              ^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match: sig end is not included in Original.T
|}]

let foo (x : Middle.pack1) =
  let module M = (val x) in
  ()
[%%expect {|
Line 2, characters 17-24:
2 |   let module M = (val x) in
                     ^^^^^^^
Error: This module type is not a signature
|}]

let foo (x : Middle.pack2) =
  let module M = (val x) in
  ()
[%%expect {|
Line 2, characters 17-24:
2 |   let module M = (val x) in
                     ^^^^^^^
Error: This module type is not a signature
|}]

module type T1 = sig type t = int end
let foo x = (x : Middle.pack1 :> (module T1))
[%%expect {|
module type T1 = sig type t = int end
File "_none_", line 1:
Error: This module type is not a signature
|}]

module type T2 = sig module M : sig type t = int end end
let foo x = (x : Middle.pack2 :> (module T2))
[%%expect {|
module type T2 = sig module M : sig type t = int end end
File "_none_", line 1:
Error: This module type is not a signature
|}]
