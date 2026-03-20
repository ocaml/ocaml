(* TEST
 modules = "a.ml coccinelle.ml";
 {
   setup-ocamlc.byte-build-env;
   flags = "-for-pack Pack";
   module = "a.ml";
   ocamlc.byte;
   flags = "-for-pack Pack";
   module = "coccinelle.ml";
   ocamlc.byte;
   module = "";
   flags = "";
   program="./coccinelle.byte";
   all_modules = "a.cmo coccinelle.cmo";
   module = "";
   ocamlc.byte;
   run;
   check-program-output;
 }{
   setup-ocamlopt.byte-build-env;
     flags = "-for-pack Pack";
     module = "a.ml";
     ocamlopt.byte;
     flags = "-for-pack Pack";
     module = "coccinelle.ml";
     ocamlopt.byte;
     all_modules = "a.cmx coccinelle.cmx";
     module = "";
     ocamlopt.byte;
     run;
     check-program-output;
 }
*)

(* Check that it is still possible to link modules compiled with -for-pack but
   not yet packed for the sake of backward compatibility. This is not officially
   supported, but it is not worth breaking packages using this property. *)

let () = Format.printf "A.x=%d@." A.x
