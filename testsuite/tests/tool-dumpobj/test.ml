(* TEST
 flags = "-nopervasives -no-g";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 run;
 check-program-output;
*)
let x = 42L
