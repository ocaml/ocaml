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

(** A description as to where, amongst the various (shared) object files
    involved in building a particular program, some entity (typically a
    symbol) is defined.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type of object file descriptions. *)
type t

(** Printing, comparison, sets, maps, etc.

    When generating code for an object file [(o1 : t)] and emitting a symbol
    reference to another object file [(o2 : t)], that symbol reference will
    be to an undefined symbol (until such time as relocated by the linker in
    the presence of both object files) iff [not (equal o1 o2)].
*)
include Identifiable.S with type t := t

(** The object file that holds code and data for the current OCaml compilation
    unit.  (When packing, there may be more than one object file for a given
    compilation unit.  However during any one run of the compiler there is
    only one; that is the one referred to here.) *)
val current_compilation_unit : t

(** The group of object files that hold code and data for OCaml compilation
    units apart from the current one. *)
val another_compilation_unit : t

(** The object file holding startup code for an executable. *)
val startup : t

(** The object file holding startup code for a shared library. *)
val shared_startup : t

(** The group of object files holding non-OCaml external definitions, for
    example code and data in the runtime, or in other libraries. *)
val runtime_and_external_libs : t
