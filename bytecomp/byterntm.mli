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

(** Parser for RNTM in bytecode executables. Parses both the RNTM section and
    the shebang launcher produced by {!Bytelink}. *)

(** Search methods used by a tendered bytecode image to find a runtime. *)
type search_method =
| Disable of string
    (** Check fixed location only *)
| Fallback of string
    (** Check given location first then fallback to searching for the
        interpreter *)
| Enable
    (** Always search for the interpreter *)

val read_runtime :
  Bytesections.section_table -> in_channel
  -> (string * Misc.RuntimeID.t option * search_method) option
(** Returns the runtime used by this tendered/standalone image. If the runtime
    used cannot be parsed, or the image was linked using -without-runtime, then
    [None] is returned. *)
