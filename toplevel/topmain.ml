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

let main () =
  Arg.parse
    ["-I", Arg.String(fun dir -> include_dirs := dir :: !include_dirs);
     "-unsafe", Arg.Set fast;
     "-drawlambda", Arg.Set dump_rawlambda;
     "-dlambda", Arg.Set dump_lambda;
     "-dinstr", Arg.Set dump_instr]
    (fun name -> raise(Arg.Bad("don't know what to do with " ^ name)));
  Toploop.loop()

let _ = Printexc.catch main ()
