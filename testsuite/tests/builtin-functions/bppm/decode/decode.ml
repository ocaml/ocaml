(* TEST

* dumpenv_expanded
set input1 = "ab%#c%.d%+ef"
set input2 = "$input1"
set result = "${bppm_decode $input2}"
** script
script = "sh ${test_source_directory}/decode_checker.sh"
*)

(* This file only contains the specification of how to run the test *)
