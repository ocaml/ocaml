(* TEST
   include testing
*)

(*

A test file for the Format module.

*)

open Testing;;
open Format;;

let say s = Printf.printf s;;

let pp_print_intseq = pp_print_seq ~pp_sep:(fun fmt () -> pp_print_char fmt ' ') pp_print_int;;

try

  say "empty\n%!";
  test (asprintf "%a%!" pp_print_intseq Seq.empty = "");

  say "\nmisc\n%!";
  test (asprintf "%a" pp_print_intseq (List.to_seq [0]) = "0");
  test (asprintf "%a" pp_print_intseq (List.to_seq [0;1;2]) = "0 1 2");
  test (asprintf "%a" pp_print_intseq (List.to_seq [0;0]) = "0 0");

  say "\nend of tests\n%!";

with e ->
  say "unexpected exception: %s\n%!" (Printexc.to_string e);
  test false;
;;
