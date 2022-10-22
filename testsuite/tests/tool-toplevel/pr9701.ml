(* TEST
ocaml_script_as_argument = "true"
ocaml_exit_status = "2"
* setup-ocaml-build-env
** ocaml
*** check-ocaml-output
*)

#1 "pr9701.ml"
Printexc.record_backtrace true;;

let f () = failwith "test";;
let proc () = f ();;
let () = proc ();;
