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

(* We use two waitgroups (synchronizing barriers, not detectable by TSan). The
   first barrier ensures that there is always a data race from TSan's point of
   view, by delaying the synchronizing [Domain.join] until after both domains
   have accessed the shared mutable field; and that these accesses always
   happen in the same order (write first or read first).

   The role of the second barrier is to always enforce the same order between
   the TSan report and logging lines such as "Leaving f". Not enforcing that
   order used to be a source of flakiness in the tests. *)
let wg = Waitgroup.create 2
let wg' = Waitgroup.create 2
let r = ref 0

let [@inline never] race () =
  ignore @@ !r;
  Waitgroup.join wg

let [@inline never] i () =
  eprintf "Entering i\n%!";
  eprintf "Calling print_and_raise...\n%!";
  print_and_raise ();
  eprintf "Leaving i\n%!"

let [@inline never] h () =
  eprintf "Entering h\n%!";
  i ();
  eprintf "Leaving h\n%!"

let [@inline never] g () =
  eprintf "Entering g\n%!";
  h ();
  eprintf "Leaving g\n%!"

let [@inline never] f () =
  eprintf "Entering f\n%!";
  (try g ()
  with Failure msg ->
    eprintf "Caught Failure \"%s\"\n%!" msg;
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
