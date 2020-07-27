(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type error =
  | Truncated_file
  | Unrecognized of string
  | Unsupported of string * int64
  | Out_of_range of string

val error_to_string: error -> string

type t

val read: string -> (t, error) Result.t

val defines_symbol: t -> string -> bool

val symbol_offset: t -> string -> int64 option
