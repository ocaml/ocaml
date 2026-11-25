(* TEST
 readonly_files = "pack_events_a.ml pack_events_pack.mli";
 setup-ocamlc.byte-build-env;
 flags = "-g";
 module = "pack_events_pack.mli";
 ocamlc.byte;
 flags = "-g -for-pack Pack_events_pack";
 module = "pack_events_a.ml";
 ocamlc.byte;
 module = "pack_events.ml";
 ocamlc.byte;
 module = "";
 flags = "-g -pack";
 program = "${test_build_directory}/pack_events_pack.cmo";
 all_modules = "pack_events_a.cmo pack_events.cmo";
 ocamlc.byte;
 run;
 check-program-output;
*)

(* The coercion to the interface of the pack wraps the primitive of
   [Pack_events_a] into a function, so the code of the pack itself has
   debugging events. They should be located in this code, not in the code
   of the packed units. *)

let g x = x
