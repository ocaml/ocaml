(* TEST *)

(*
A test file for Format.print_string_if_newline,
print_or_newline and print_fits_or_breaks.
*)

let set_margin n =
  Format.set_margin n;
  Format.set_max_indent (n - 1)

open Format;;

let print_string_if_newline = pp_print_string_if_newline std_formatter
let print_or_newline = pp_print_or_newline std_formatter
let print_fits_or_breaks = pp_print_fits_or_breaks std_formatter

let test_print_string_if_newline m =
  let case ~first =
    print_space ();
    if first
    then print_string_if_newline "| "
    else print_string "| ";
    print_string "Foooo -> foooo"
  in
  print_string "margin = ";
  print_int m;
  print_newline ();
  set_margin m;
  open_hvbox 2;
  print_string "let foo = function";
  case ~first:true;
  case ~first:false;
  close_box ();
  print_newline ()
;;

test_print_string_if_newline 15;;
test_print_string_if_newline 55;;

print_newline ();;

let test_print_or_newline m =
  let sep ~first =
    print_space ();
    if first
    then print_or_newline 0 0 "" "  (* breaks *) "
    else print_or_newline 0 0 "; " "; (* breaks *) "
  in
  print_string "margin = ";
  print_int m;
  print_newline ();
  set_margin m;
  open_hvbox 0;
  print_string "let foo ="; print_space ();
  print_string "[";
  sep ~first:true;
  print_string "0"; sep ~first:false;
  print_string "0"; sep ~first:false;
  print_string "0"; sep ~first:false;
  print_string "0"; sep ~first:false;
  print_string "0"; sep ~first:false;
  print_string "0";
  print_space ();
  print_string "]";
  close_box ();
  print_newline ()
;;

test_print_or_newline 20;;
test_print_or_newline 60;;

print_newline ();;

let test_print_fits_or_breaks m =
  print_string "margin = ";
  print_int m;
  print_newline ();
  set_margin m;
  open_hvbox 0;
  print_fits_or_breaks "" 0 0 "( ";
  print_string "a";
  for i = 1 to 10 do
    print_space (); print_string "+ a"
  done;
  print_fits_or_breaks "" 0 1 " )";
  close_box ();
  print_newline ()
;;

test_print_fits_or_breaks 15;;
test_print_fits_or_breaks 50;;
