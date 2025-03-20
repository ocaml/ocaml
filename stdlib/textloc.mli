(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*               Olivier Nicole, projet Cambium, Inria Paris              *)
(*                                                                        *)
(*   Copyright 2026 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** [Textloc.t] is the type of source code locations given to implicit location
    arguments. *)

(** Represents a range of positions in a text file. *)
type t = textloc

(** [dummy] is an invalid location usually used as a placeholder, or for
    testing or error signaling purposes. *)
val dummy : t

val filename : t -> string

(** One-based line number. *)
val line_num : t -> int

(** Zero-based byte offset of the start position, from the first byte of the
    line identified by [line_num]. *)
val line_pos : t -> int

(** Zero-based byte offset of the end position, from the first byte of the
    line identified by [line_num]. *)
val end_pos : t -> int
