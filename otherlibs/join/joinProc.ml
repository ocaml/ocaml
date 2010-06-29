(***********************************************************************)
(*                                                                     *)
(*                           JoCaml                                    *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2008 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Unix

let try_set_close_on_exec fd =
  try set_close_on_exec fd; true with Invalid_argument _ -> false

let open_proc cmd args input output err toclose =
  let cloexec = List.for_all try_set_close_on_exec toclose in
  match fork () with
  | 0 ->
      if not cloexec then List.iter close toclose;
      if input <> stdin then begin
	dup2 input stdin; close input
      end;
      if output <> stdout then begin
	dup2 output stdout; close output
      end;
      if err <> stderr then begin
	dup2 err stderr; close err
      end;
      begin try Unix.execvp cmd args
      with _ -> exit 127 end
  | id -> id


let command cmd args =
  open_proc cmd args stdin stdout stderr []

let open_in cmd args =
  let in_read, in_write = pipe() in
  let id = open_proc cmd args stdin in_write stderr [in_read] in
  close in_write ;
  let inchan = in_channel_of_descr in_read in
  id, inchan

let open_out cmd args =
  let out_read, out_write = pipe() in
  let id = open_proc cmd args out_read stdout stderr [out_write] in
  close out_read;
  let outchan = out_channel_of_descr out_write in
  id, outchan

let open_in_out cmd args =
  let in_read, in_write = pipe() in
  let out_read, out_write = pipe() in
  let id =
    open_proc cmd args out_read in_write stderr [in_read; out_write] in
  close out_read;
  close in_write;
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  id, (inchan, outchan)


let open_full cmd args =
  let in_read, in_write = pipe() in
  let out_read, out_write = pipe() in
  let err_read, err_write = pipe() in
  let id =
    open_proc cmd args out_read in_write err_write
      [in_read; out_write; err_read] in
(* Critical section in open_fork, from fork system call *)
  close out_read;
  close in_write;
  close err_write;
(* End critical section *)
  let inchan = in_channel_of_descr in_read in
  let outchan = out_channel_of_descr out_write in
  let errchan = in_channel_of_descr err_read in
  id, (inchan, outchan, errchan)

