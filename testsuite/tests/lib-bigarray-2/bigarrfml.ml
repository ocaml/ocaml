(* TEST

readonly_files = "bigarrf.f bigarrfstub.c"
last_flags = "-cclib -lgfortran"

* script
script = "sh ${test_source_directory}/has-gfortran.sh"

** setup-ocamlc.byte-build-env
*** script
script = "sh ${test_source_directory}/call-gfortran.sh ${cc} -c bigarrf.f"
**** ocamlc.byte
all_modules = "bigarrf.o bigarrfstub.c bigarrfml.ml"
***** run
output = "${test_build_directory}/program-output"
stdout = "${output}"
****** check-program-output

** setup-ocamlopt.byte-build-env
*** script
script = "sh ${test_source_directory}/call-gfortran.sh ${cc} -c bigarrf.f"
**** ocamlopt.byte
all_modules = "bigarrf.o bigarrfstub.c bigarrfml.ml"
***** run
output = "${test_build_directory}/program-output"
stdout = "${output}"
****** check-program-output

*)

open Bigarray
open Printf

(* Test harness *)

let error_occurred = ref false

let function_tested = ref ""

let testing_function s =
    function_tested := s;
    print_newline();
    print_string s;
    print_newline()

let test test_number answer correct_answer =
 flush stdout;
 flush stderr;
 if answer <> correct_answer then begin
   eprintf "*** Bad result (%s, test %d)\n" !function_tested test_number;
   flush stderr;
   error_occurred := true
 end else begin
   printf " %d..." test_number
 end

(* External Fortran functions *)

external fortran_filltab :
  unit -> (float, float32_elt, fortran_layout) Array2.t = "fortran_filltab"
external fortran_printtab :
  (float, float32_elt, fortran_layout) Array2.t -> unit = "fortran_printtab"

let _ =

  let make_array2 kind layout ind0 dim1 dim2 fromint =
    let a = Array2.create kind layout dim1 dim2 in
    for i = ind0 to dim1 - 1 + ind0 do
      for j = ind0 to dim2 - 1 + ind0 do
        a.{i,j} <- (fromint (i * 1000 + j))
      done
    done;
    a in

  print_newline();
  testing_function "------ Foreign function interface --------";
  testing_function "Passing an array to Fortran";
  fortran_printtab (make_array2 float32 fortran_layout 1 5 4 float);
  testing_function "Accessing a Fortran array";
  let a = fortran_filltab () in
  test 1 a.{1,1} 101.0;
  test 2 a.{2,1} 201.0;
  test 3 a.{1,2} 102.0;
  test 4 a.{5,4} 504.0;
