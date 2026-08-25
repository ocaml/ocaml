(* TEST
 include testing;
*)

(*

A test file for the Format module.

*)

open Testing;;
open Format;;

let say s = Printf.printf (s ^^ "\n%!");;

let run name pp_print_array of_array =
  let pp_print_intarray =
    pp_print_array
      ?pp_sep:(Some (fun fmt () -> pp_print_char fmt ' '))
      pp_print_int
  in
  say "empty %s" name;
  test (asprintf "%a" pp_print_intarray (of_array [||]) = "");

  say "\nvarious %ss" name;
  test (asprintf "%a" pp_print_intarray (of_array [| 0 |])       = "0");
  test (asprintf "%a" pp_print_intarray (of_array [| 0; 1; 2 |]) = "0 1 2");
  test (asprintf "%a" pp_print_intarray (of_array [| 0; 0 |])    = "0 0");

  say "\nend of tests"

let () =
  run "array" pp_print_array Fun.id;
  run "iarray" pp_print_iarray Iarray.of_array;
  run "dynarray" pp_print_dynarray Dynarray.of_array;
