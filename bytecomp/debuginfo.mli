(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type kind = Dinfo_call | Dinfo_raise

type t = private {
  dinfo_kind: kind;
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int
}

val none: t

val is_none: t -> bool

val to_string: t -> string

val from_location: kind -> Location.t -> t
val from_filename: kind -> string -> t

val from_call: Lambda.lambda_event -> t
val from_raise: Lambda.lambda_event -> t

val to_location: t -> Location.t
