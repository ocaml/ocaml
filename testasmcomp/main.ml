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

open Clflags

let compile_file filename =
  Emit.begin_assembly();
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    while true do
      Asmgen.compile_phrase(Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic; Emit.end_assembly()
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        prerr_string "Syntax error near character ";
        prerr_int (Lexing.lexeme_start lb);
        prerr_newline()
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x

let main() =
  Arg.parse
    ["-dcmm", Arg.Set dump_cmm;
     "-dsel", Arg.Set dump_selection;
     "-dlive", Arg.Unit(fun () -> dump_live := true;
                                  Printmach.print_live := true);
     "-dspill", Arg.Set dump_spill;
     "-dsplit", Arg.Set dump_split;
     "-dinterf", Arg.Set dump_interf;
     "-dprefer", Arg.Set dump_prefer;
     "-dalloc", Arg.Set dump_regalloc;
     "-dreload", Arg.Set dump_reload;
     "-dscheduling", Arg.Set dump_scheduling;
     "-dlinear", Arg.Set dump_linear]
    compile_file

let _ = Printexc.catch main (); exit 0

