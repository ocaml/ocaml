open Test

let _ =
  print_newline();
  if !error_occurred then begin
    prerr_endline "************* TEST FAILED ****************"; exit 2
  end else
    exit 0
