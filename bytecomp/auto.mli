(***********************************************************************)
(*                                                                     *)
(*                                JoCaml                               *)
(*                                                                     *)
(*            Luc Maranget, projet Moscova, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*>JOCAML *)

(* Compiled join automaton *)

open Typedtree

type joinchannelopt = Chan of Ident.t * int | Alone of Ident.t

type joinchannelfull= joinchannel * joinchannelopt

type dispatcher =
   Ident.t * joinchannelfull  * (pattern * joinchannelfull) list * partial

type reaction = 
    Ident.t * (* Guarded process identifier *)
      joinpattern list * (* Original join patterns *)
      joinpattern list list *  (* Compiled joinpatterns *)
      (Ident.t * pattern) list * (* Added matching *)
      expression (* Guarded process *)

type channel_env =  (Ident.t * joinchannelfull) list 

type t =
  {
   cauto_name : Ident.t * Ident.t; (* auto name, wrapped auto name *)
   (* All names defined, with sort of  *)
   cauto_channels : channel_env ;
   cauto_nchans : int ; (* number of actual channels *)
   (* Original names *)
   cauto_original : channel_env ;
   cauto_loc : Location.t ;
   cauto_dispatchers : dispatcher list ;
   cauto_forwarders : reaction list ;
   cauto_reactions : reaction list ;
 }  
(*<JOCAML *)
