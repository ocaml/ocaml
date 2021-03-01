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
