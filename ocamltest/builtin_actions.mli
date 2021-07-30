(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of a few built-in actions *)

val pass : Actions.t
val skip : Actions.t
val fail : Actions.t

val dumpenv : Actions.t

val hasunix : Actions.t
val libunix : Actions.t
val libwin32unix : Actions.t

val windows : Actions.t
val not_windows : Actions.t

val bsd : Actions.t
val not_bsd : Actions.t

val arch32 : Actions.t
val arch64 : Actions.t

(** Whether the compiler target is POWER architecture. *)
val arch_power : Actions.t

val has_symlink : Actions.t

val setup_build_env : Actions.t

val setup_simple_build_env : Actions.t

val run : Actions.t
val script : Actions.t

val check_program_output : Actions.t

val file_exists : Actions.t

val copy : Actions.t
