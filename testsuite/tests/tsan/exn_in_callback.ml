(* TEST

 ocamlopt_flags = "-g -ccopt -fsanitize=thread -ccopt -O1 -ccopt -fno-omit-frame-pointer -ccopt -g";

 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "callbacks.c waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml exn_in_callback.ml";
 native;

*)
exception ExnA
exception ExnB

external print_and_call_ocaml_h : unit -> unit = "print_and_call_ocaml_h"

open Printf

(* Synchronization barriers for reproducibility of the tests (see comment in
   [exn_from_c.ml]). *)
let wg = Waitgroup.create 2
let wg' = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  ignore @@ !r;
  Waitgroup.join wg

let [@inline never] i () =
  eprintf "Entering i\n%!";
  eprintf "Throwing ExnB...\n%!";
  ignore (raise ExnB);
  eprintf "Leaving i\n%!"

let [@inline never] h () =
  eprintf "Entering h\n%!";
  i ();
  eprintf "Leaving h\n%!"

let _ = Callback.register "ocaml_h" h

let [@inline never] g () =
  eprintf "Entering g\n%!";
  eprintf "Calling C code\n%!";
  print_and_call_ocaml_h ();
  eprintf "Back from C code\n%!";
  eprintf "Leaving g\n%!"

let [@inline never] f () =
  eprintf "Entering f\n%!";
  (try g () with
  | ExnB ->
    eprintf "Caught an ExnB\n%!";
    Printexc.print_backtrace stderr;
    flush stderr;
    race ());
  Waitgroup.join wg';
  eprintf "Leaving f\n%!"

let [@inline never] writer () =
  Waitgroup.join wg;
  r := 1;
  Waitgroup.join wg'

let () =
  Printexc.record_backtrace true;
  let d = Domain.spawn writer in
  f ();
  Domain.join d
