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

let report_error = function
    Unclosed(opening_loc, opening, closing_loc, closing) ->
      if String.length !Location.input_name > 0 then begin
        Location.print closing_loc;
        print_string "Syntax error: missing '";
        print_string closing;
        print_string "'"; force_newline();
        Location.print opening_loc;
        print_string "This is the location of the unmatched '";
        print_string opening;
        print_string "'"
      end else begin
        Location.print opening_loc;
        print_string "Syntax error: this '";
        print_string opening;
        print_string "' has no matching '";
        print_string closing;
        print_string "'"
      end
  | Other loc ->
      Location.print loc;
      print_string "Syntax error"


