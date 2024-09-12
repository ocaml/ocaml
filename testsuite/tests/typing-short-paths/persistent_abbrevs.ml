(* TEST
 readonly_files = "abbrev.ml";
 setup-ocamlc.byte-build-env;
 module = "abbrev.ml";
 ocamlc.byte;
 flags = " -short-paths ";
 expect;
*)

#directory "ocamlc.byte";;
open Abbrev
let x = [[1]; 2.0]
[%%expect {|
Line 2, characters 14-17:
2 | let x = [[1]; 2.0]
                  ^^^
Error: The constant "2.0" has type "float" but an expression was expected of type
         "int list"
|}]

type a = A
module M=struct
  type a = B
  let f A B = ()
   let g f = f B A
  let () = g f
end
[%%expect{|
type a = A
Line 6, characters 13-14:
6 |   let () = g f
                 ^
Error: The value "f" has type "a/2 -> a -> unit"
       but an expression was expected of type "a -> a/2 -> 'a"
       Type "a/2" is not compatible with type "a"
       Line 3, characters 2-12:
         Definition of type "a"
       Line 1, characters 0-10:
         Definition of type "a/2"
|}]
