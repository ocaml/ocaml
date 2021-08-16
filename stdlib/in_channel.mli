(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Input channels

    @since 4.14.0 *)

type t = in_channel
(** The type of input channel. *)

type open_flag = Stdlib.open_flag =
  | Open_rdonly      (** open for reading. *)
  | Open_wronly      (** open for writing. *)
  | Open_append      (** open for appending: always write at end of file. *)
  | Open_creat       (** create the file if it does not exist. *)
  | Open_trunc       (** empty the file if it already exists. *)
  | Open_excl        (** fail if Open_creat and the file already exists. *)
  | Open_binary      (** open in binary mode (no conversion). *)
  | Open_text        (** open in text mode (may perform conversions). *)
  | Open_nonblock    (** open in non-blocking mode. *)
(** Opening modes for {!open_gen}. *)

val stdin : t
(** The standard input for the process. *)

val open' : string -> t
(** Open the named file for reading, and return a new input channel on that
    file, positioned at the beginning of the file. *)

val open_bin : string -> t
(** Same as {!open'}, but the file is opened in binary mode, so that no
    translation takes place during reads. On operating systems that do not
    distinguish between text mode and binary mode, this function behaves like
    {!open'}. *)

val open_gen : open_flag list -> int -> string -> t
(** [open_gen mode perm filename] opens the named file for reading, as described
    above. The extra arguments [mode] and [perm] specify the opening mode and
    file permissions.  {!open'} and {!open_bin} are special cases of this
    function. *)

val seek : t -> int64 -> unit
(** [seek chan pos] sets the current reading position to [pos] for channel
    [chan]. This works only for regular files. On files of other kinds, the
    behavior is unspecified. *)

val pos : t -> int64
(** Return the current reading position for the given channel.  For files opened
    in text mode under Windows, the returned position is approximate (owing to
    end-of-line conversion); in particular, saving the current position with
    {!pos}, then going back to this position using {!seek} will not work.  For
    this programming idiom to work reliably and portably, the file must be
    opened in binary mode. *)

val length : t -> int64
(** Return the size (number of characters) of the regular file on which the
    given channel is opened.  If the channel is opened on a file that is not a
    regular file, the result is meaningless.  The returned size does not take
    into account the end-of-line translations that can be performed when reading
    from a channel opened in text mode. *)

val close : t -> unit
(** Close the given channel.  Input functions raise a [Sys_error] exception when
    they are applied to a closed input channel, except {!close}, which does
    nothing when applied to an already closed channel. *)

val close_noerr : t -> unit
(** Same as {!close}, but ignore all errors. *)

val input_char : t -> char option
(** Read one character from the given input channel.  Returns [None] if there
    are no more characters to read. *)

val input_byte : t -> int option
(** Same as {!input_char}, but return the 8-bit integer representing the
    character. Returns [None] if an end of file was reached. *)

val input_line : t -> string option
(** Read characters from the given input channel, until a newline character is
    encountered. Return the string of all characters read, without the newline
    character at the end.  Returns [None] if the end of the file is reached at
    the beginning of line. *)

val input_binary_int : t -> int option
(** Read an integer encoded in binary format (4 bytes, big-endian) from the
    given input channel. See {!Out_channel.output_binary_int}.  Returns [None] if an
    end of file was reached while reading the integer. *)

val input : t -> bytes -> int -> int -> int
(** [input ic buf pos len] reads up to [len] characters from the given channel
    [ic], storing them in byte sequence [buf], starting at character number
    [pos].  It returns the actual number of characters read, between 0 and [len]
    (inclusive).

    A return value of 0 means that the end of file was reached.

    A return value between 0 and [len] exclusive means that not all requested
    [len] characters were read, either because no more characters were available
    at that time, or because the implementation found it convenient to do a
    partial read; {!input} must be called again to read the remaining characters,
    if desired.  (See also {!really_input} for reading exactly [len]
    characters.)

    @raise Invalid_argument if [pos] and [len] do not designate a valid range of
    [buf]. *)

val really_input : t -> bytes -> int -> int -> unit option
(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in byte sequence [buf], starting at character number [pos].

    Returns [None] if the end of file is reached before [len] characters have
    been read.

    @raise Invalid_argument if [pos] and [len] do not designate a valid range of
    [buf]. *)

val really_input_string : t -> int -> string option
(** [really_input_string ic len] reads [len] characters from channel [ic] and
    returns them in a new string.  Returns [None] if the end of file is reached
    before [len] characters have been read. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode ic true] sets the channel [ic] to binary mode: no
    translations take place during input.

    [set_binary_mode ic false] sets the channel [ic] to text mode: depending
    on the operating system, some translations may take place during input.  For
    instance, under Windows, end-of-lines will be translated from [\r\n] to
    [\n].

    This function has no effect under operating systems that do not distinguish
    between text mode and binary mode. *)
