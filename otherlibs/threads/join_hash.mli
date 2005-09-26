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

type ('a,'b) t

val create : unit -> ('a,'b) t

val add :  ('a,'b) t -> 'a -> 'b -> unit

val find : ('a,'b) t -> 'a -> 'b

val find_remove  : ('a,'b) t -> 'a -> 'b

val get :  ('a,'b) t -> ('a -> 'b) -> 'a -> 'b

val iter : ('a, 'b) t -> ('a -> 'b -> unit) -> unit

val remove : ('a,'b) t -> 'a -> unit
