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

let _ =
  let args =
    String.concat " " (List.tl (Array.to_list Sys.argv)) in
  exit(Sys.command("cslc -linkall toplevellib.cma " ^ args ^ " topmain.cmo"))
