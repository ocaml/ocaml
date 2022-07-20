(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Cambium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2022 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The lexer for special comments. *)

val line_number : int ref

val comments_level : int ref

val main : Lexing.lexbuf -> Odoc_parser.token

val elements : Lexing.lexbuf -> Odoc_parser.token

val simple : Lexing.lexbuf -> Odoc_parser.token
