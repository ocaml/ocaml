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

    @since 4.11 *)

type t = in_channel

val stdin : t
(** Standard output. *)

(** {1 General input functions} *)

type open_flag =
  | Create of int
  | Exclusive
  | Binary
  | Non_blocking

val open_ : ?flags:open_flag list -> string -> t
(** [open_ ?flags fn] opens the file named [fn] for reading according to [flags]
    (default to [[]]), and returns a new input channel on that file, positioned
    at the beginning of the file. *)

val with_file : ?flags:open_flag list -> string -> (t -> 'a) -> 'a
(** [with_file ?flags filename f] opens the file named [filename] for reading
    according to [flags] (defaults to [[]]), invokes [f] to process the contents
    of that file then, once [f] has returned or triggered an exception, closes
    the file before proceeding.

    All errors arising during closing are ignored. *)

val close : t -> unit
(** [close ic] closes [ic]. Input functions raise a [Sys_error] exception when
    they are applied to a closed input channel, except [close], which does
    nothing when applied to an already closed channel. *)

val close_noerr : t -> unit
(** [close_noerr ic] is like [close ic] but ignores all errors. In particular,
    it never raises [Sys_error]. *)

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

    Exception [Invalid_argument] is raised if [pos] and [len] do not designate a
    valid range of [buf]. *)

val really_input : t -> bytes -> int -> int -> unit option
(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in byte sequence [buf], starting at character number [pos].
    Return [None] if the end of file is reached before [len] characters have
    been read. In this case, all available characters will have been written to
    [buf].

    Raises [Invalid_argument] if [pos] and [len] do not designate a valid range
    of [buf]. *)

val really_input_string : t -> int -> string option
(** [really_input_string ic len] reads [len] characters from channel [ic] and
    returns them in a new string.  Returns [None] if the end of file is reached
    before [len] characters have been read. *)

val input_char : t -> char option
(** [input_char ic] reads one character from [ic]. Returns [None] if there are
    no more characters to read. *)

val input_byte : t -> int option
(** [input_byte ic] reads one byte from [ic]. Returns [None] if if there are no
    more bytes to read. *)

val input_binary_int : t -> int option
(** [input_binary_int ic] reads an integer encoded in binary format (4 bytes,
    big-endian) from [ic]. Returns [None] if the end of file is reached while
    reading the integer.

    See also {!Out_channel.output_binary_int}. *)

val input_line : t -> string option
(** [input_line ic] reads characters from [ic], until a newline character is
    encountered. Returns the string of all characters read, without the newline
    character at the end. Returns [None] if the end of file is reached at te
    beginning of line.

    Note that if [ic] is in text mode (see {!set_binary_mode}), then the
    sequence [\r\n] is also accepted in place of the newline character. *)

val fold_lines : ('a -> string -> 'a) -> 'a -> t -> 'a
(** [fold_lines f x ic] is [x] if [input_line ic = None] and
    [fold_lines f (f x l) ic] if [input_line ic = Some l]. *)

val input_lines : t -> string list
(** [input_lines ic] is [List.rev (fold_lines (fun l s -> s :: l) [] ic)]. *)

val iter_lines : (string -> unit) -> t -> unit
(** [iter_lines f ic] is [fold_lines (fun () l -> f l) () ic]. *)

val input_to_string : t -> string option
(** [input_to_string ic] is [None] if more than [Sys.max_string_length] bytes
    are left to be read from [ic].  Otherwise, it is [Some s], where [s]
    contains all remaining data in [ic]. *)

val seek : t -> int64 -> unit
(** [seek ic pos] sets the current reading position to byte [pos] for channel
    [ici]. This works only for regular files. On files of other kinds, the
    behavior is unspecified. *)

val pos : t -> int64
(** [pos ic] returns the current reading position for [ic]. *)

val length : t -> int64
(** [length ic] is the size (number of bytes) of the regular file on which the
    given channel is opened.  If the channel is opened on a file that is not a
    regular file, the result is meaningless.  The returned size does not take
    into account the end-of-line translations that can be performed when reading
    from a channel opened in text mode. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode ic b] sets the channel [ic] to binary mode (if [b] is
    [true]) or text mode (if [b] is [false]). In binary mode, no translations
    take place during input. In text mode, depending on the operating system,
    some translations may take place during input. For instance, under Windows,
    end-of-lines will be translated from [\r\n] to [\n].  This function has no
    effect under operating systems that do not distinguish between text mode and
    binary mode. *)

val get_binary_mode : t -> bool
(** [get_binary_mode oc] returns [true] if [oc] is in binary mode and [false] if
    it is in text mode. This function always returns [true] in operatring systems
    that do not distinguish between text mode and binary mode. *)
