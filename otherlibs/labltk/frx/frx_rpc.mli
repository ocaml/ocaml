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
(* Some notion of RPC *)

val register : string -> (string list -> unit) -> unit
  (* [register external_name f] *)
val invoke : string -> string -> string list -> string
  (* [invoke interp name args] *)
val async_invoke : string -> string -> string list -> unit
  (* [async_invoke interp name args] *)
val rpc_info : string -> string
  (* [rpc_info interp] *)
