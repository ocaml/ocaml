(* TEST
   exit_status = "${TEST_ALARM}";
   not windows;
   hasunix;
   include unix;
   bytecode;
   native;
*)

(* If a testcase is expected to timeout when a bug is present,
   but pass fairly quickly when the bug is absent,
   then an alarm can be used to turn this into a useful test.
   There is a default timeout, but it is too long,
   and also we may want to set the timeout only around the
   piece of code that triggers the bug (which could be inside a loop).

   This is done only on Unix, because Unix.alarm is not supported on Windows.
*)

let () =
  let _ : int = Unix.alarm 1 in
  Unix.sleep 2
