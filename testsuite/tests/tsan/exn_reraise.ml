(* TEST

 ocamlopt_flags = "-g -ccopt -fsanitize=thread -ccopt -O1 -ccopt -fno-omit-frame-pointer -ccopt -g";
 include unix;
 set TSAN_OPTIONS="detect_deadlocks=0";

 tsan;
 native;

*)
exception ExnA
exception ExnB

open Printf

let r = ref 0

let [@inline never] race () = ignore @@ !r

let [@inline never] i () =
  printf "entering i\n%!";
  printf "throwing Exn...\n%!";
  ignore (raise ExnA);
  printf "leaving i\n%!"

let [@inline never] h () =
  printf "entering h\n%!";
  try i () with
  | ExnB -> printf "caught an ExnB\n%!";
  printf "leaving h\n%!"

let [@inline never] g () =
  printf "entering g\n%!";
  h ();
  printf "leaving g\n%!"

let [@inline never] f () =
  printf "entering f\n%!";
  (try g () with
  | ExnA ->
    printf "caught an ExnA\n%!";
    Printexc.print_backtrace stdout;
    race ());
  printf "leaving f\n%!"

let () =
  Printexc.record_backtrace true;
  let d = Domain.spawn (fun () -> Unix.sleep 1; r := 1) in
  f (); Unix.sleep 1;
  Domain.join d
