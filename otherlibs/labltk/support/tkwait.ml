(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*         Francois Rouaix, Francois Pessaux and Jun Furuse              *)
(*               projet Cristal, INRIA Rocquencourt                      *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)


external internal_tracevis : string -> string -> unit
        = "camltk_wait_vis"
external internal_tracedestroy : string -> string -> unit
        = "camltk_wait_des"
