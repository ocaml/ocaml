(* TEST
 readonly_files = "hints_link_a.ml hints_link_b.ml hints_link_c.ml";
 setup-ocamlc.byte-build-env;
 flags = "-for-pack Hints_pack";
 module = "hints_link_a.ml";
 ocamlc.byte;
 module = "hints_link_b.ml";
 ocamlc.byte;
 module = "";
 flags = "-pack";
 program = "hints_pack.cmo";
 all_modules = "hints_link_a.cmo hints_link_b.cmo";
 ocamlc.byte;
 flags = "";
 module = "hints_link_c.ml";
 ocamlc.byte;
 module = "";
 flags = "-a";
 program = "hints_lib.cma";
 all_modules = "hints_link_c.cmo";
 ocamlc.byte;
 flags = "";
 module = "hints_link.ml";
 ocamlc.byte;
 module = "";
 program = "${test_build_directory}/hints_link_without_hints.byte";
 all_modules = "hints_lib.cma hints_pack.cmo hints_link.cmo";
 ocamlc.byte;
 flags = "-bytecode-hints";
 program = "${test_build_directory}/hints_link.byte";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* Hints are copied by the packager and the librarian, and relocated when
   linking. Each compilation unit performs a bigarray access with a
   distinct hint; the instruction the hint is attached to should be the
   access. *)

let get
    (t : (int, Bigarray.int8_signed_elt, Bigarray.fortran_layout)
         Bigarray.Array1.t) =
  Bigarray.Array1.get t 1

let _ = Hints_pack.Hints_link_a.get, Hints_pack.Hints_link_b.get,
        Hints_link_c.set
