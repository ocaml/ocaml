(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Clflags

let usage = "Usage: ocaml <options>\noptions are:"

let file_argument name =
  exit (if Toploop.run_script name then 0 else 2)

let main () =
  Arg.parse [
     "-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs),
           "<dir>  Add <dir> to the list of include directories";
     "-unsafe", Arg.Set fast, " No bound checking on array and string access";
     "-drawlambda", Arg.Set dump_rawlambda, " (undocumented)";
     "-dlambda", Arg.Set dump_lambda, " (undocumented)";
     "-dinstr", Arg.Set dump_instr, " (undocumented)"
    ] file_argument usage;
  Toploop.loop()

let _ = Printexc.catch main ()
