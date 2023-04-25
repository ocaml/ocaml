(* TEST
 modules = "nocrypto.mli fortuna.ml rng.ml";
 {
   setup-ocamlc.byte-build-env;
   {
     module = "nocrypto.mli";
     ocamlc.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "fortuna.ml";
     ocamlc.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "rng.ml";
     ocamlc.byte;
   }{
     program = "nocrypto.cmo";
     flags = "-pack";
     all_modules = "fortuna.cmo rng.cmo";
     ocamlc.byte;
   }
 }{
   setup-ocamlopt.byte-build-env;
   {
     module = "nocrypto.mli";
     ocamlopt.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "fortuna.ml";
     ocamlopt.byte;
   }{
     flags = "-for-pack Nocrypto";
     module = "rng.ml";
     ocamlopt.byte;
   }{
     program = "nocrypto.cmx";
     flags = "-pack";
     all_modules = "fortuna.cmx rng.cmx";
     ocamlopt.byte;
   }
 }
*)
