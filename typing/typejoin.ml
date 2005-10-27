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

open Types
open Typedtree

let get_replies e = match e.exp_type.desc with
| Tproc konts -> konts
| _ -> assert false

let id_lt (x,_) (y,_) = Pervasives.compare x y < 0



exception MissingLeft of Ident.t
exception MissingRight of Ident.t
exception Double of Ident.t * Location.t * Location.t

(* Symetrical difference, catches double answers  *)

let rec delta xs ys = match xs, ys with
| [],_  -> ys
| _, [] -> xs
| x::rx, y::ry ->
   if id_lt x y then
     x::delta rx ys
   else if id_lt y x then
     y::delta xs ry
   else (* x=y *)
     let id,loc1 = x
     and _,loc2 = y in
     raise (Double (id, loc1, loc2))


let rec inter loc xs ys = match xs, ys with
| [],(id,_)::_  -> raise (MissingLeft id)
| (id,_)::_, [] ->  raise (MissingRight id)
| [],[] -> []
| x::rx, y::ry ->
   if id_lt y x then
     raise (MissingLeft (fst y))
   else if id_lt x y then
     raise (MissingRight (fst x))
   else (* x=y *)
     (fst x,loc)::inter loc rx ry
