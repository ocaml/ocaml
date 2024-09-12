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
Error: The constant "2.0" has type "float/2"
       but an expression was expected of type "int/2 list/2"
       File "_none_", line 1:
         Definition of type "float/2"
       File "_none_", line 1:
         Definition of type "int/2"
       File "_none_", line 1:
         Definition of type "list/2"
|}]
