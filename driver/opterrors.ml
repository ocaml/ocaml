(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Error report *)

open Format
open Location

(* Report an error *)

let report_error ppf exn =
  let report ppf = function
  | Lexer.Error(err, start, stop) ->
      Location.print ppf
        {loc_start = start; loc_end = stop; loc_ghost = false};
      Lexer.report_error ppf err
  | Syntaxerr.Error err ->
      Syntaxerr.report_error ppf err
  | Env.Error err ->
      Env.report_error ppf err
  | Typecore.Error(loc, err) ->
      Location.print ppf loc; Typecore.report_error ppf err
  | Typetexp.Error(loc, err) ->
      Location.print ppf loc; Typetexp.report_error ppf err
  | Typedecl.Error(loc, err) ->
      Location.print ppf loc; Typedecl.report_error ppf err
  | Includemod.Error err ->
      Includemod.report_error ppf err
  | Typemod.Error(loc, err) ->
      Location.print ppf loc; Typemod.report_error ppf err
  | Translcore.Error(loc, err) ->
      Location.print ppf loc; Translcore.report_error ppf err
  | Compilenv.Error code ->
      Compilenv.report_error code
  | Asmgen.Error code ->
      Asmgen.report_error code
  | Asmlink.Error code ->
      Asmlink.report_error code
  | Asmlibrarian.Error code ->
      Asmlibrarian.report_error code
  | Sys_error msg ->
      fprintf ppf "I/O error: %s" msg
  | Typeclass.Error(loc, err) ->
      Location.print ppf loc; Typeclass.report_error ppf err
  | x -> fprintf ppf "@]"; raise x in

  fprintf ppf "@[%a@]@." report exn
