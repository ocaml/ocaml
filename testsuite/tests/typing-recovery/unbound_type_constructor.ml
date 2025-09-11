(* TEST
   flags = " -typing-recovery";
   ocamlc_byte_exit_status = "2";
   setup-ocamlc.byte-build-env;
   compile_only = "true";
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

let foo1 : int = 3

let foo2 : int * toto = 3, 4

let foo3 : int * int = 3, 4

type tata = Tata of int

let foo4 : tata = Tata (snd foo2)
