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

open Clflags

let usage = "Usage: ocaml <options> [script-file]\noptions are:"

let file_argument name =
  exit (if Toploop.run_script Format.err_formatter name Sys.argv then 0 else 2)

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-labels", Arg.Clear classic, " Use commuting label mode";
     "-modern", Arg.Clear classic, " (deprecated) same as -labels";
     "-noassert", Arg.Set noassert, " Do not compile assertion checks";
     "-rectypes", Arg.Set recursive_types, " Allow arbitrary recursive types";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-w", Arg.String (Warnings.parse_options false),
           "<flags>  Enable or disable warnings according to <flags>:\n\
       \032    A/a enable/disable all warnings\n\
       \032    C/c enable/disable suspicious comment\n\
       \032    F/f enable/disable partially applied function\n\
       \032    M/m enable/disable overriden method\n\
       \032    P/p enable/disable partial match\n\
       \032    S/s enable/disable non-unit statement\n\
       \032    U/u enable/disable unused match case\n\
       \032    V/v enable/disable hidden instance variable\n\
       \032    X/x enable/disable all other warnings\n\
       \032    default setting is A (all warnings enabled)";
     "-warn-error" , Arg.String (Warnings.parse_options true),
       "<flags>  Enable or disable fatal warnings according to <flags>\n\
         \032    (see option -w for the list of flags)\n\
         \032    default setting is a (all warnings are non-fatal)";

     "-dparsetree", Arg.Set dump_parsetree, " (undocumented)";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)";
    ] file_argument usage;
  Toploop.loop Format.std_formatter

let _ = Printexc.catch main ()
