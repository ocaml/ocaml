(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags
let write_asm_file = ref false

let compile_file filename =
  if !write_asm_file then begin
    let out_name = Filename.chop_extension filename ^ ".s" in
    Emitaux.output_channel := open_out out_name
  end; (* otherwise, stdout *)
  Compilenv.reset "test";
  Clflags.cmm_invariants := true;
  Emit.begin_assembly();
  let ic = open_in filename in
  let lb = Lexing.from_channel ic in
  lb.Lexing.lex_curr_p <- Lexing.{ lb.lex_curr_p with pos_fname = filename };
  try
    while true do
      Asmgen.compile_phrase ~ppf_dump:Format.std_formatter
        (Parsecmm.phrase Lexcmm.token lb)
    done
  with
      End_of_file ->
        close_in ic; Emit.end_assembly();
        if !write_asm_file then close_out !Emitaux.output_channel
    | Lexcmm.Error msg ->
        close_in ic; Lexcmm.report_error lb msg
    | Parsing.Parse_error ->
        close_in ic;
        let start_p = Lexing.lexeme_start_p lb in
        let end_p = Lexing.lexeme_end_p lb in
        Printf.eprintf "File \"%s\", line %i, characters %i-%i:\n\
                        Syntax error.\n%!"
          filename
          start_p.Lexing.pos_lnum
          (start_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
          (end_p.Lexing.pos_cnum - start_p.Lexing.pos_bol)
    | Parsecmmaux.Error msg ->
        close_in ic; Parsecmmaux.report_error msg
    | x ->
        close_in ic; raise x

let usage = "Usage: codegen <options> <files>\noptions are:"

let main() =
  Arg.parse [
     "-S", Arg.Set write_asm_file,
       " Output file to filename.s (default is stdout)";
     "-g", Arg.Set Clflags.debug, "";
     "-dcmm", Arg.Set dump_cmm, "";
     "-dcse", Arg.Set dump_cse, "";
     "-dsel", Arg.Set dump_selection, "";
     "-dlive", Arg.Unit(fun () -> dump_live := true ), "";
     "-dspill", Arg.Set dump_spill, "";
     "-dsplit", Arg.Set dump_split, "";
     "-dinterf", Arg.Set dump_interf, "";
     "-dprefer", Arg.Set dump_prefer, "";
     "-dalloc", Arg.Set dump_regalloc, "";
     "-dreload", Arg.Set dump_reload, "";
     "-dscheduling", Arg.Set dump_scheduling, "";
     "-dlinear", Arg.Set dump_linear, "";
     "-dtimings", Arg.Unit (fun () -> profile_columns := [ `Time ]), "";
    ] compile_file usage

let () =
  main ();
  Profile.print Format.std_formatter !Clflags.profile_columns;
  exit 0
