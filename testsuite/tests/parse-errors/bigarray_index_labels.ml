(* TEST
 setup-ocamlc.byte-build-env;
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

open Bigarray

let my_big_array =
    Bigarray.Array2.create Bigarray.float32 Bigarray.c_layout 20 20;;

my_big_array.{(1, ~y:2)} <- 1.5;;
