(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The shallow abstract syntax *)

type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
}

(** Format a location into a string the standard error format of OCaml:
    "File %S, line %d, characters %d-%d". *)
val show_location : location -> string

(** Print a warning to stderr message after the location and
    the "Warning: " prefix.
    The message may span multiple lines but should not be terminated by
    a newline. *)
val print_warning : location -> string -> unit

type regular_expression =
    Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression * (string * location)

type ('arg,'action) entry =
  {name:string ;
   shortest : bool ;
   args : 'arg ;
   clauses : (regular_expression * 'action) list}

type lexer_definition = {
  header: location;
  entrypoints: ((string list, location) entry) list;
  trailer: location;
  refill_handler : location option;
}
