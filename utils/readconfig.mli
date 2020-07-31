(* *********************************************************************)
(*                                                                     *)
(*              The Compcert verified compiler                         *)
(*                                                                     *)
(*          Xavier Leroy, INRIA Paris-Rocquencourt                     *)
(*                                                                     *)
(*  Copyright Institut National de Recherche en Informatique et en     *)
(*  Automatique.  All rights reserved.  This file is distributed       *)
(*  under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 2 of the License, or  *)
(*  (at your option) any later version.  This file is also distributed *)
(*  under the terms of the INRIA Non-Commercial License Agreement.     *)
(*                                                                     *)
(* *********************************************************************)

(* Reading configuration files *)

(* The format of a configuration file is a list of lines
      variable=value
   The "value" on the right hand side is a list of whitespace-separated
   words.  Quoting is honored with the same rules as POSIX shell:
      \<newline>            for multi-line values
      single quotes         no escapes within
      double quotes         \$ \` \<doublequote> \\ \<newline> as escapes
   Finally, lines starting with '#' are comments.
*)

val read_config_file: string -> unit
  (** Read (key, value) pairs from the given file name.  Raise [Error]
      if file is ill-formed. *)

val key_val: string -> string list option
  (** [key_val k] returns the value associated with key [k], if any.
      Otherwise, [None] is returned. *)

exception Error of string * int * string
  (** Raised in case of error.
      First argument is file name, second argument is line number,
      third argument is an explanation of the error. *)
