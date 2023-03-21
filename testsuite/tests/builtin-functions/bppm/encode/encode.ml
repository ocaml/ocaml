(* TEST
 set input1 = "ab%c:d=ef";
 set input2 = "$input1";
 set result = "${bppm_encode $input2}";
 dumpenv_expanded;

 script = "sh ${test_source_directory}/encode_checker.sh";
 script;
*)

(* This file only contains the specification of how to run the test *)
