(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Jerome Vouillon, projet Cristal, INRIA Rocquencourt        *)
(*          Objective Caml port by John Malecki and Xavier Leroy       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Handling of keyboard interrupts *)

let interrupted = ref false

let protect = ref false

let break signum =
  if !protect
  then interrupted := true
  else raise Sys.Break

let _ =
  Sys.signal Sys.sigint (Sys.Signal_handle break);
  Sys.signal Sys.sigpipe (Sys.Signal_handle (fun _ -> raise End_of_file))

let protected f =
  if !protect then
    f ()
  else begin
    protect := true;
    if not !interrupted then
       f ();
    protect := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end
  end

let unprotected f =
  if not !protect then
    f ()
  else begin
    protect := false;
    if !interrupted then begin interrupted := false; raise Sys.Break end;
    f ();
    protect := true
  end
