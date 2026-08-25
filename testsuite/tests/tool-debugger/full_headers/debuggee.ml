(* TEST_BELOW *)

let arr = Array.make (1 lsl 22) 42

let main () =
  ignore (Sys.opaque_identity arr)

let () = main ()

(* This test checks that the debugger gets all the header bits
   when using the remote protocol (i.e. without fetching the full value).
   See Github issue #15012 for more details.
*)

(* TEST
 arch64;
 flags += " -g ";
 debugger_script = "${test_source_directory}/input_script";
 debugger;
 shared-libraries;
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
 ocamldebug;
 check-program-output;
*)
