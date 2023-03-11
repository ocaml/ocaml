(* TEST
   include testing
*)

(*

A test file for the Format module.

*)

open Testing;;
open Format;;

let say s = Printf.printf (s ^^ "\n%!");;

let pp_print_intarray = pp_print_array ~pp_sep:(fun fmt () -> pp_print_char fmt ' ') pp_print_int;;

let () =

  say "empty";
  test (asprintf "%a" pp_print_intarray [||] = "");

  say "\nmisc";
  test (asprintf "%a" pp_print_intarray [| 0 |]       = "0");
  test (asprintf "%a" pp_print_intarray [| 0; 1; 2 |] = "0 1 2");
  test (asprintf "%a" pp_print_intarray [| 0; 0 |]    = "0 0");

  say "\nend of tests"
