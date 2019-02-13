(* TEST
(* Tests from manual, section intf-c *)
(*
  This test is currently skipped because there is no proper way to
  figure out whether Curses is available or not. If it becomes possible
  to figure that out, it would be nice to be able to check that the test
  compiles. Executing seems lessrelevant.
*)
* skip
reason = "curses can not be properly detected at the moment"
*)

(* File prog.ml -- main program using curses *)
open Curses;;
let main_window = initscr () in
let small_window = newwin 10 5 20 10 in
  mvwaddstr main_window 10 2 "Hello";
  mvwaddstr small_window 4 3 "world";
  refresh();
  Unix.sleep 5;
  endwin()
