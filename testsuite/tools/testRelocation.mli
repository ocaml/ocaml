(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Relocation Test - which files contain the installation prefix and/or build
    path. *)

val run : reproducible:bool -> Harness.Import.config -> Environment.t -> unit
(** If [~reproducible:true] then an additional check is added to the base rule
    set. Only {v Makefile.config v} is permitted to contain the prefix and
    no file is allowed to contain the build path. *)
