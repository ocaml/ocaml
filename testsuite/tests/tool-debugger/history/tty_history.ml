(* TEST
   not-windows;
   include unix;
   hasunix;
   readonly_files = "tty_driver.ml tty_stubs.c";
   {
     setup-ocamlc.byte-build-env;
     program = "${test_build_directory}/tty_history.byte";
     flags = " -g ";
     ocamlc.byte;
   }{
     script = "sh ${test_source_directory}/build_tty_driver.sh ${ocamlc}";
   }{
     script = "./tty_driver.exe '${ocaml_srcdir}/runtime/ocamlrun ${ocaml_srcdir}/debugger/ocamldebug' ${test_build_directory}/tty_history.byte > tty_history.output";
   }{
     script = "diff -u tty_history.output ${test_source_directory}/tty_history.reference";
   }
 *)

(* Simple program used by the tty history regression test. *)

let rec factorial n =
  if n <= 1 then 1 else n * factorial (n - 1)

let () =
  print_int (factorial 5);
  print_newline ()
