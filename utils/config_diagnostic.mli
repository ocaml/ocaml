(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** System configuration

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

module Versions: Diagnostic_history.S

include Diagnostic.Record with type vl := Versions.id

val print : id Log.t -> unit
val var : string -> string option
  (** the configuration value of a variable, if it exists *)

  (** {1 Displaying configuration variables} *)

val show_variable_and_exit : string -> unit
(** Display the value of the given configuration variable,
    then exit the program with code 0. *)
