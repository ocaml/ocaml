open Printf

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number eq_fun (answer, correct_answer) =
 flush stdout;
 flush stderr;
 if not (eq_fun answer correct_answer) then begin
   fprintf stderr ">>> Bad result (%s, test %d)\n" !function_tested test_number;
   error_occurred := true;
   false
 end else begin
   printf " %d..." test_number;
   true
 end

let failure_test test_number fun_to_test arg =
 flush stdout;
 flush stderr;
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error_occurred := true;
   false
  with _ ->
   printf " %d..." test_number;
   true

let failwith_test test_number fun_to_test arg correct_failure =
 try
   fun_to_test arg;
   fprintf stderr ">>> Failure expected (%s, test %d)\n"
                  !function_tested test_number;
   error_occurred := true;
   false
  with x ->
   if x = correct_failure then begin
     printf " %d..." test_number;
     true
   end else begin
     fprintf stderr ">>> Bad failure (%s, test %d)\n"
                    !function_tested test_number;
     error_occurred := true;
     false
   end   

let eq = (==)
let eq_int = (==)
let eq_string = (=)

let sixtyfour = (1 lsl 32) <> 0
