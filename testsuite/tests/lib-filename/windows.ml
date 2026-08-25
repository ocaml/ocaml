(* TEST
 target-windows;
 {
   bytecode;
 }{
   native;
 }
*)

(* Adversarial tests for Filename.quote_command: a hostile string supplied as
   a redirection target or program name must not be able to smuggle an extra
   command past cmd.exe.

   The cmd.exe metacharacters split in two, according to whether they are legal
   in a Windows file name:

   - Legal in a file name (& ( ) ^): a correctly quoted target becomes a real
     (oddly named) file, so we can actually run "true" with the redirection and
     confirm it executes (Result 0) instead of the injected "false" (which
     would give a non-zero code).  The payloads contain no spaces, so that the
     pre-fix code -- which double-quoted only when a space was present -- does
     not neutralise them by accident.

   - Illegal in a file name (| < >): such a string can never name a real file,
     so a correctly quoted target can only ever make cmd.exe fail, never
     execute.  Running it would just compare cmd.exe's locale-dependent error
     text, so instead we pin the constructed command line and check the target
     is wrapped in double quotes.

   true/false are the POSIX commands on the PATH (the testsuite runs under
   Cygwin). *)

let remove f = try Sys.remove f with Sys_error _ -> ()

(* Run the redirection and report the exit code.  A correctly quoted target is
   a real file, so "true" runs and the code is 0. *)
let run_probe label ?stdin ?stdout ?stderr () =
  let cmd = Filename.quote_command "true" ?stdin ?stdout ?stderr [] in
  let rc = Sys.command cmd in
  List.iter (function Some f -> remove f | None -> ()) [stdin; stdout; stderr];
  Printf.printf "%s: %S -> %d\n" label cmd rc

let () =
  print_string "==== Executable (legal file name) targets, must not inject\n";
  run_probe "stdout &"  ~stdout:"out&false" ();
  run_probe "stdout &&" ~stdout:"out&&false" ();
  run_probe "stdout ()" ~stdout:"out(false)" ();
  run_probe "stdout (" ~stdout:"out(false)" ();
  run_probe "stdout )" ~stdout:"out(false)" ();
  run_probe "stderr &"  ~stderr:"err&false" ();
