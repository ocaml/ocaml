(* TEST

 ocamlopt_flags = "-g -ccopt -fsanitize=thread -ccopt -O1 -ccopt -fno-omit-frame-pointer -ccopt -g";

 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 readonly_files = "callbacks.c waitgroup_stubs.c";
 all_modules = "${readonly_files} waitgroup.ml exn_from_c.ml";
 native;

*)

external print_and_raise : unit -> unit = "print_and_raise"

open Printf

let wg = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  ignore @@ !r;
  Waitgroup.join wg

let [@inline never] i () =
  printf "Entering i\n%!";
  printf "Calling print_and_raise...\n%!";
  print_and_raise ();
  printf "Leaving i\n%!"

let [@inline never] h () =
  printf "Entering h\n%!";
  i ();
  printf "Leaving h\n%!"

let [@inline never] g () =
  printf "Entering g\n%!";
  h ();
  printf "Leaving g\n%!"

let [@inline never] f () =
  printf "Entering f\n%!";
  (try g ()
  with Failure msg ->
    printf "Caught Failure \"%s\"\n%!" msg;
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
