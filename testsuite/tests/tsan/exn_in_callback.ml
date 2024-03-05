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

let wg = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  ignore @@ !r;
  Waitgroup.join wg

let [@inline never] i () =
  printf "Entering i\n%!";
  printf "Throwing ExnB...\n%!";
  ignore (raise ExnB);
  printf "Leaving i\n%!"

let [@inline never] h () =
  printf "Entering h\n%!";
  i ();
  printf "Leaving h\n%!"

let _ = Callback.register "ocaml_h" h

let [@inline never] g () =
  printf "Entering g\n%!";
  printf "Calling C code\n%!";
  print_and_call_ocaml_h ();
  printf "Back from C code\n%!";
  printf "Leaving g\n%!"

let [@inline never] f () =
  printf "Entering f\n%!";
  (try g () with
  | ExnB ->
    printf "Caught an ExnB\n%!";
    Printexc.print_backtrace stdout;
    race ());
  printf "Leaving f\n%!"

let [@inline never] writer () =
  Waitgroup.join wg;
  r := 1

let () =
  Printexc.record_backtrace true;
  let d = Domain.spawn writer in
  f ();
  Domain.join d
