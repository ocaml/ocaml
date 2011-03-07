(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(** Registering Caml values with the C runtime.

   This module allows Caml values to be registered with the C runtime
   under a symbolic name, so that C code can later call back registered
   Caml functions, or raise registered Caml exceptions.
*)

val register : string -> 'a -> unit
(** [Callback.register n v] registers the value [v] under
   the name [n]. C code can later retrieve a handle to [v]
   by calling [caml_named_value(n)]. *)

val register_exception : string -> exn -> unit
(** [Callback.register_exception n exn] registers the
   exception contained in the exception value [exn]
   under the name [n]. C code can later retrieve a handle to
   the exception by calling [caml_named_value(n)]. The exception
   value thus obtained is suitable for passign as first argument
   to [raise_constant] or [raise_with_arg]. *)
