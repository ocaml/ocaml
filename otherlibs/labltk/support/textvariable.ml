(***********************************************************************)
(*                                                                     *)
(*                 MLTk, Tcl/Tk interface of Objective Caml            *)
(*                                                                     *)
(*    Francois Rouaix, Francois Pessaux, Jun Furuse and Pierre Weis    *)
(*               projet Cristal, INRIA Rocquencourt                    *)
(*            Jacques Garrigue, Kyoto University RIMS                  *)
(*                                                                     *)
(*  Copyright 2002 Institut National de Recherche en Informatique et   *)
(*  en Automatique and Kyoto University.  All rights reserved.         *)
(*  This file is distributed under the terms of the GNU Library        *)
(*  General Public License, with the special exception on linking      *)
(*  described in file LICENSE found in the Objective Caml source tree. *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Support
open Protocol

external internal_tracevar : string -> cbid -> unit
        = "camltk_trace_var"
external internal_untracevar : string -> cbid -> unit
        = "camltk_untrace_var"
external set : string -> string -> unit = "camltk_setvar"
external get : string -> string = "camltk_getvar"


type textVariable = string

(* List of handles *)
let handles = Hashtbl.create 401

let add_handle var cbid = 
  try
    let r = Hashtbl.find handles var in
    r := cbid :: !r
  with
    Not_found -> 
      Hashtbl.add handles var (ref [cbid])

let exceptq x =
  let rec ex acc = function
     [] -> acc
   | y::l when y == x -> ex acc l
   | y::l -> ex (y::acc) l
  in
  ex []

let rem_handle var cbid =
  try
    let r = Hashtbl.find handles var in
    match exceptq cbid !r with
      [] -> Hashtbl.remove handles var
    | remaining -> r := remaining
  with
    Not_found -> ()

(* Used when we "free" the variable (otherwise, old handlers would apply to
 * new usage of the variable)
 *)
let rem_all_handles var =
  try
    let r = Hashtbl.find handles var in
    List.iter (internal_untracevar var) !r;
    Hashtbl.remove handles var
  with
    Not_found -> ()


(* Variable trace *)
let handle vname ~callback:f =
  let id = new_function_id() in
  let wrapped _ =
    clear_callback id;
    rem_handle vname id;
    f() in
  Hashtbl.add callback_naming_table id wrapped;
  add_handle vname id;
  if !Protocol.debug then begin
    prerr_cbid id; prerr_string " for variable "; prerr_endline vname
  end;
  internal_tracevar vname id

(* Avoid space leak (all variables are global in Tcl) *)
module StringSet =
  Set.Make(struct type t = string let compare = compare end)
let freelist = ref (StringSet.empty)
let memo = Hashtbl.create 101

(* Added a variable v referenced by widget w *)
let add w v =
  let w = Widget.forget_type w in
  let r = 
    try Hashtbl.find memo w 
    with
      Not_found -> 
        let r = ref StringSet.empty in
          Hashtbl.add memo w r;
          r in
   r := StringSet.add v !r

(* to be used with care ! *)
let free v =
  rem_all_handles v;
  freelist := StringSet.add v !freelist

(* Free variables associated with a widget *)
let freew w =
  try
    let r = Hashtbl.find memo w in
    StringSet.iter free !r;
    Hashtbl.remove memo w 
  with
    Not_found -> ()

let _ = add_destroy_hook freew

(* Allocate a new variable *)
let counter = ref 0
let getv () = 
  let v = 
    if StringSet.is_empty !freelist then begin
      incr counter; 
      "camlv("^ string_of_int !counter ^")"
      end
    else
      let v = StringSet.choose !freelist in
        freelist := StringSet.remove v !freelist;
        v in
    set v "";
    v

let create ?on: w () =
  let v = getv() in
  begin
  match w with
     Some w -> add w v
   | None -> ()
  end;
  v

(* to be used with care ! *)
let free v =
  freelist := StringSet.add v !freelist

let cCAMLtoTKtextVariable s = TkToken s

let name s = s
let coerce s = s

