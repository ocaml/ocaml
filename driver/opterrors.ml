(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Error report *)

open Format
open Location

(* Report an error *)

let report_error exn =
  open_hovbox 0;
  begin match exn with
    Lexer.Error(err, start, stop) ->
      Location.print {loc_start = start; loc_end = stop};
      Lexer.report_error err
  | Parse.Error(start, stop) ->
      Location.print {loc_start = start; loc_end = stop};
      print_string "Syntax error"
  | Env.Error err ->
      Env.report_error err
  | Typecore.Error(loc, err) ->
      Location.print loc; Typecore.report_error err
  | Typetexp.Error(loc, err) ->
      Location.print loc; Typetexp.report_error err
  | Typedecl.Error(loc, err) ->
      Location.print loc; Typedecl.report_error err
  | Includemod.Error err ->
      Includemod.report_error err
  | Typemod.Error(loc, err) ->
      Location.print loc; Typemod.report_error err
  | Translcore.Error(loc, err) ->
      Location.print loc; Translcore.report_error err
  | Compilenv.Error code ->
      Compilenv.report_error code
  | Asmgen.Error code ->
      Asmgen.report_error code
  | Asmlink.Error code ->
      Asmlink.report_error code
  | Asmlibrarian.Error code ->
      Asmlibrarian.report_error code
  | Sys_error msg ->
      print_string "I/O error: "; print_string msg
  | x ->
      close_box(); raise x
  end;
  close_box(); print_newline()
