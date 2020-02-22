(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


(** Input channels.

    @since 4.10 *)

type t = in_channel

val stdin : t
(** Standard output. *)

(** {2 General input functions} *)

type open_flag =
  | Create of int
  | Exclusive
  | Binary
  | Non_blocking

val open_ : ?flags:open_flag list -> string -> t
(** Open the named file for reading, and return a new input channel on that
    file, positioned at the beginning of the file. *)

val with_file : ?flags:open_flag list -> string -> (t -> 'a) -> 'a
(** [with_file ?flags filename f] opens the file named [filename] for reading
    according to [flags], invokes [f] to process the contents of that file then,
    once [f] has returned or triggered an exception, closes the file before
    proceeding. *)

val close : t -> unit
(** [close ic] closes [ic]. Input functions raise a [Sys_error] exception when
    they are applied to a closed input channel, except [close], which does
    nothing when applied to an already closed channel. *)

val close_noerr : t -> unit
(** [close_noerr ic] is like [close ic] but ignores all errors. *)

val input : t -> bytes -> int -> int -> int
(** [input ic buf pos len] reads up to [len] characters from
    the given channel [ic], storing them in byte sequence [buf], starting at
    character number [pos].
    It returns the actual number of characters read, between 0 and
    [len] (inclusive).
    A return value of 0 means that the end of file was reached.
    A return value between 0 and [len] exclusive means that
    not all requested [len] characters were read, either because
    no more characters were available at that time, or because
    the implementation found it convenient to do a partial read;
    [input] must be called again to read the remaining characters,
    if desired.  (See also {!Stdlib.really_input} for reading
    exactly [len] characters.)
    Exception [Invalid_argument "input"] is raised if [pos] and [len]
    do not designate a valid range of [buf]. *)

val really_input : t -> bytes -> int -> int -> unit option
(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in byte sequence [buf], starting at character number [pos].
    Return [None] if the end of file is reached before [len]
    characters have been read.
    Raise [Invalid_argument "really_input"] if
    [pos] and [len] do not designate a valid range of [buf]. *)

val really_input_string : t -> int -> string option
(** [really_input_string ic len] reads [len] characters from channel [ic]
    and returns them in a new string.
    Return [None] if the end of file is reached before [len]
    characters have been read. *)

val input_char : t -> char option
(** Read one character from the given input channel.
    Return [None] if there are no more characters to read. *)

val input_byte : t -> int option
(** Same as {!input_char}, but return the 8-bit integer representing
    the character.
    Return [None] if an end of file was reached. *)

val input_binary_int : t -> int option
(** Read an integer encoded in binary format (4 bytes, big-endian)
    from the given input channel. See {!Stdlib.output_binary_int}.
    Return [None] if an end of file was reached while reading the
    integer. *)

val input_line : t -> string option
(** Read characters from the given input channel, until a
    newline character is encountered. Return the string of
    all characters read, without the newline character at the end.
    Return [None] if the end of the file is reached
    at the beginning of line. *)

val input_lines : t -> string list
(** Read the rest of the file and split it into lines. *)

val iter_lines : (string -> unit) -> t -> unit
(** [iter_lines f ic] is equivalent to [List.iter f (input_lines ic)] but more
    efficient. *)

val fold_lines : ('a -> string -> 'a) -> 'a -> t -> 'a
(** [iter_lines f x ic] is equivalent to
    [List.fold_left f x (input_lines ic)]. *)

val input_to_string : t -> string
(** Read all remaining data. *)

val seek : t -> int64 -> unit
(** [seek chan pos] sets the current reading position to [pos] for channel
    [chan]. This works only for regular files. On files of other kinds, the
    behavior is unspecified. *)

val pos : t -> int64
(** Return the current reading position for the given channel. *)

val length : t -> int64
(** Return the size (number of characters) of the regular file
    on which the given channel is opened.  If the channel is opened
    on a file that is not a regular file, the result is meaningless.
    The returned size does not take into account the end-of-line
    translations that can be performed when reading from a channel
    opened in text mode. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode ic true] sets the channel [ic] to binary
    mode: no translations take place during input.
    [set_binary_mode ic false] sets the channel [ic] to text
    mode: depending on the operating system, some translations
    may take place during input.  For instance, under Windows,
    end-of-lines will be translated from [\r\n] to [\n].
    This function has no effect under operating systems that
    do not distinguish between text mode and binary mode. *)
