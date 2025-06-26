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

(** Test 3 - {v CAML_LD_LIBRARY_PATH v}, {v OCAMLLIB v}, {v CAMLLIB v} and
    {v ld.conf v} processed correctly. *)

val run : Harness.Import.config -> Environment.t -> unit
