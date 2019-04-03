(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Like [Compilation_unit], but also corresponds to units derived entirely
    from Cmm code (and not having any OCaml source file), such as the startup
    and shared startup files.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type of backend compilation units. *)
type t = private
  | Compilation_unit of Compilation_unit.t
  | Startup
  | Shared_startup
  | Runtime_and_external_libs

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Create a backend compilation unit from a middle-end compilation unit. *)
val compilation_unit : Compilation_unit.t -> t

(** The backend compilation unit for an executable's startup file. *)
val startup : t

(** The backend compilation unit for a shared library's startup file. *)
val shared_startup : t

(** The backend compilation unit for code external to that produced by the
    OCaml compiler, such as the runtime, and the system C library. *)
val runtime_and_external_libs : t
