(* TEST
   exit_status = "126";
 *)

(* To test that it fails in the expected way set a specific exitcode.
   This can be used to distinguish unexpectedly passing tests,
   or tests that fail in unexpected ways (e.g. different bug)

   Should be a value between 3 and 126, except 125.
   2 should be avoided, because that is the exitcode for exceptions
   1 should be avoided, because that is sometimes also used to mark a failed testcase
   125 should be avoided, because that is already used for SKIP
   127 should be avoided, because that is usually used for command-not-found by the shell.
 *)

let () =
  (* if (some_condition_that_a_bug_is_present) then *)
  exit 126
