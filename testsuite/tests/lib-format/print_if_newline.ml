(* TEST *)

(*

A test file for Format.print_if_newline.

*)

open Format;;

printf "\ntest print_if_newline\n%!";
printf "  newline here\n%!";
print_if_newline ();
printf "  this gets printed";
print_if_newline ();
printf "  this doesn't get printed";

printf "\nprint_if_newline doesn't crash when last statement\n%!";
printf "  newline here\n";
(* Important that the following is the last statement in the file.

   [print_if_newline] sets up the Format module to skip printing
   the next printing command. However, it should not crash if there
   is no next printing statement. *)
print_if_newline ();
;;
