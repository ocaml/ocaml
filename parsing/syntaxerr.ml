(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Auxiliary type for reporting syntax errors *)

open Format

type error =
    Unclosed of Location.t * string * Location.t * string
  | Other of Location.t

exception Error of error
exception Escape_error

let report_error = function
    Unclosed(opening_loc, opening, closing_loc, closing) ->
      if String.length !Location.input_name = 0
      && Location.highlight_locations opening_loc closing_loc
      then begin
        print_string "Syntax error: '";
        print_string closing;
        print_string "' expected, the highlighted '";
        print_string opening;
        print_string "' might be unmatched"
      end else begin
        Location.print closing_loc;
        print_string "Syntax error: '";
        print_string closing;
        print_string "' expected"; force_newline();
        Location.print opening_loc;
        print_string "This '";
        print_string opening;
        print_string "' might be unmatched"
      end
  | Other loc ->
      Location.print loc;
      print_string "Syntax error"


