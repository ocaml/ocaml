(* TEST

* dumpenv_expanded
set input1 = "ab%c:d=ef"
set input2 = "$input1"
set result1 = "${bppm_encode $input2}"
set result2 = "${bppm_decode $result1}"
** script
script = "sh ${test_source_directory}/encode_checker.sh"
*)

(* This file only contains the specification of how to run the test *)
