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

let compile_file filename =
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  try
    while true do
      Asmgen.compile_phrase(Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic
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
    ["-dcmm", Arg.Unit(fun () -> Clflags.dump_cmm := true);
     "-dsel", Arg.Unit(fun () -> Clflags.dump_selection := true);
     "-dlive", Arg.Unit(fun () -> Clflags.dump_live := true;
                                  Printmach.print_live := true);
     "-dspill", Arg.Unit(fun () -> Clflags.dump_spill := true);
     "-dsplit", Arg.Unit(fun () -> Clflags.dump_split := true);
     "-dinterf", Arg.Unit(fun () -> Clflags.dump_interf := true);
     "-dprefer", Arg.Unit(fun () -> Clflags.dump_prefer := true);
     "-dalloc", Arg.Unit(fun () -> Clflags.dump_regalloc := true);
     "-dreload", Arg.Unit(fun () -> Clflags.dump_reload := true);
     "-dlinear", Arg.Unit(fun () -> Clflags.dump_linear := true)]
    compile_file

let _ = Printexc.catch main (); exit 0

