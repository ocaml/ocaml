(* TEST

flags = "-w a"

* setup-ocamlc.byte-build-env
** ocamlc.byte
*** run
**** check-program-output

* libwin32unix
** setup-ocamlopt.byte-build-env
*** ocamlopt.byte
**** run
***** check-program-output

* libunix
** script
script = "sh ${test_source_directory}/has-stackoverflow-detection.sh"
*** setup-ocamlopt.byte-build-env
**** ocamlopt.byte
***** run
****** check-program-output

*)

let rec f x =
  if not (x = 0 || x = 10000 || x = 20000)
  then 1 + f (x + 1)
  else
    try
      1 + f (x + 1)
    with Stack_overflow ->
      print_string "x = "; print_int x; print_newline();
      raise Stack_overflow

let _ =
 begin
  try
    ignore(f 0)
  with Stack_overflow ->
    print_string "Stack overflow caught"; print_newline()
 end ;
 (* GPR#1289 *)
 Printexc.record_backtrace true;
 begin
  try
    ignore(f 0)
  with Stack_overflow ->
    print_string "second Stack overflow caught"; print_newline()
 end
