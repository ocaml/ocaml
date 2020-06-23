(* TEST
   ocaml_script_as_argument = "true"
   ocaml_exit_status = "2"
 * toplevel
*)

(* Make the test reproducible regardless of whether OCAMLRUNPARAM=b or not *)
Printexc.record_backtrace true;;

let () =
  (* regression test for #9701 *)
  Format.pp_print_text Format.std_formatter
    "This test is currently not working at expected: \
     it does not include proper backtrace locations for \
     the exception raised in the ocaml script file. \
     See 'Called from unknown location' in \
     toplevel_script_backtrace.ocaml.reference";
  Format.printf "@."

let f () = failwith "test"
let proc () = f ()
let () = proc ()
