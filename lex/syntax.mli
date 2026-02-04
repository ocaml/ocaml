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

(** Location in the source mll file.

    Its uses include identifying source OCaml code (header, trailer, actions)
    and injecting it into the generated ml file.
*)
type location = {
  loc_file : string;
  start_pos : int;
  end_pos : int;
  start_line : int;
  start_col : int;
  end_line : int;
  end_col : int;
}

val location_of_positions : Lexing.position -> Lexing.position -> location

(** Format a location into a string in the standard error format of OCaml. *)
val show_location : location -> string

(** Print a warning to stderr.
    The message may span multiple lines but should not be terminated by
    a newline.

    @param fatal indicate that the warning is fatal in the error message.
    This does not exit the program.

    @param name show the warning name as known to the [-w] option, if any.
*)
val print_warning : ?fatal:bool -> ?name:string -> location -> string -> unit

type regular_expression =
    Epsilon
  | Characters of Cset.t
  | Eof
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression
  | Bind of regular_expression * (string * location)

type ('arg, 'action) entry = {
  name: string;
  shortest: bool;
  args: 'arg;
  body_location: location;
  clauses: (regular_expression * 'action) list
}

type lexer_definition = {
  header: location;
  entrypoints: ((string list, location) entry) list;
  trailer: location;
  refill_handler : location option;
}
