(* TEST
 readonly_files = "puts.c";
 use_runtime = "false";
 unset FOO;
 include unix;
 hasunix;
 setup-ocamlc.byte-build-env;
 flags = "-w -a -output-complete-exe puts.c -ccopt -I${ocamlsrcdir}/runtime";
 program = "test2";
 ocamlc.byte;
 program = "./test2";
 run;
 check-program-output;
*)

external puts: string -> unit = "caml_puts"

let _ = at_exit (fun () -> print_endline "Program terminated")

let () =
  Unix.putenv "FOO" "Hello OCaml!";
  puts (Unix.getenv "FOO")
