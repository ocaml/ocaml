(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2005 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(*DEBUG*)open Join_debug

(****************************)
(* Readers/writer controler *)
(****************************)

type controler =
  {
    cond_w : Condition.t ;
    cond_r : Condition.t ;
    mutex : Mutex.t ;
    mutable n_read : int ;
    mutable n_wait : int ;
  } 


let controler_create () =
  {
    cond_w = Condition.create () ;
    cond_r = Condition.create () ;
    mutex = Mutex.create () ;
    n_read = 0 ;
    n_wait = 0 ;
  } 

let do_lock c f =
  Mutex.lock c.mutex ;
  f c ;
  Mutex.unlock c.mutex

let read_access c =
  do_lock c
    (fun c ->
      c.n_wait <- c.n_wait + 1 ;
      while c.n_read < 0 do
        Condition.wait c.cond_r c.mutex
      done ;
      c.n_wait <- c.n_wait - 1 ;
      c.n_read <- c.n_read + 1)

let read_return c =
  do_lock c
    (fun c ->
      c.n_read <- c.n_read - 1;
      if c.n_read = 0 then
        Condition.signal c.cond_w)

let protect_read c reader key =
  read_access c ;
  try
    let r = reader key in
    read_return c ;
    r
  with
  | e ->
      read_return c ;
      raise e

let write_access c =
  do_lock c
    (fun c ->
      while c.n_read <> 0 do
        Condition.wait c.cond_w c.mutex
      done ;
      c.n_read <- -1)

let write_return c =
  do_lock c
    (fun c ->
      c.n_read <- 0 ;
      begin if c.n_wait > 0 then
        Condition.broadcast c.cond_r
      else
        Condition.signal c.cond_w end)
      
      

let protect_write c writer arg =
  write_access c ;
  try
    let r = writer arg in
    write_return c ;
    r
  with
  | e ->
      write_return c ;
      raise e
(*******************************************)
(* Locked counters, with detection of zero *)
(*******************************************)

type counter =
  { mutable cval : int ;
    cmutex : Mutex.t ;
    ccond  : Condition.t ; }

let counter_create () =
  { cval = 0 ; cmutex = Mutex.create () ; ccond = Condition.create () ; }

let incr c =
  Mutex.lock c.cmutex ;
  c.cval <- c.cval + 1 ;
  Mutex.unlock c.cmutex

let decr c =
  Mutex.lock c.cmutex ;
  begin match c.cval with
  | 0 -> assert false
  | 1 ->
      Condition.broadcast c.ccond ;
      c.cval <- 0
  | v ->
      c.cval <- v-1
  end ;
  Mutex.unlock c.cmutex


let rec hard_wait_zero c = match c.cval with
| 0 -> ()
| _ -> Condition.wait c.ccond c.cmutex

let wait_zero c =
  Mutex.lock c.cmutex ;
  hard_wait_zero c ;
  Mutex.unlock c.cmutex
    
(* Nicer Printexc functions *)

open Unix

let prerr_exn = function
  | Unix_error(err, fun_name, arg) ->
    begin try prerr_string Sys.argv.(0)
    with Invalid_argument _ -> prerr_string "????" end;
    prerr_string ": \"";
    prerr_string fun_name;
    prerr_string "\" failed";
    if String.length arg > 0 then begin
      prerr_string " on \"";
      prerr_string arg;
      prerr_string "\""
    end;
    prerr_string ": ";
    prerr_endline (error_message err)
  | e -> prerr_endline (Printexc.to_string e)

let exn_to_string = function
  | Unix_error(err, fun_name, arg) ->
    let buff = Buffer.create 80 in
    begin try Buffer.add_string buff Sys.argv.(0)
    with Invalid_argument _ -> Buffer.add_string buff "????" end;
    Buffer.add_string buff ": \"";
    Buffer.add_string buff fun_name;
    Buffer.add_string buff "\" failed";
    if String.length arg > 0 then begin
      Buffer.add_string buff " on \"";
      Buffer.add_string buff arg;
      Buffer.add_string buff "\""
    end;
    Buffer.add_string buff ": ";
    Buffer.add_string buff (error_message err) ;
    Buffer.contents buff
  | e -> Printexc.to_string e

(* A few Unix stuff *)
let local_name = gethostname ()

let get_local_addr () =
  try
    let first_addr entry = entry.h_addr_list.(0) in
    first_addr (gethostbyname local_name)
  with
  | Not_found | Invalid_argument _ -> inet_addr_loopback

let string_of_sockaddr = function
  | ADDR_UNIX s -> s
  | ADDR_INET (a,p) ->
      string_of_inet_addr a^":"^string_of_int p

exception JoinExit

(* All options that can be set from outside are here *)

(* STILL TO TEST
let globalize_flags = [Marshal.Closures]
*)
let globalize_flags = []

