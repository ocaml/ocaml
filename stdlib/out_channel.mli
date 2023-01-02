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

(** Output channels.

    @since 4.14 *)

type t = out_channel
(** The type of output channel. *)

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

val stdout : t
(** The standard output for the process. *)

val stderr : t
(** The standard error output for the process. *)

val open_bin : string -> t
(** Open the named file for writing, and return a new output channel on that
    file, positioned at the beginning of the file. The file is truncated to zero
    length if it already exists. It is created if it does not already exists. *)

val open_text : string -> t
(** Same as {!open_bin}, but the file is opened in text mode, so that newline
    translation takes place during writes. On operating systems that do not
    distinguish between text mode and binary mode, this function behaves like
    {!open_bin}. *)

val open_gen : open_flag list -> int -> string -> t
(** [open_gen mode perm filename] opens the named file for writing, as described
    above. The extra argument [mode] specifies the opening mode. The extra
    argument [perm] specifies the file permissions, in case the file must be
    created.  {!open_text} and {!open_bin} are special cases of this
    function. *)

val with_open_bin : string -> (t -> 'a) -> 'a
(** [with_open_bin fn f] opens a channel [oc] on file [fn] and returns [f
    oc]. After [f] returns, either with a value or by raising an exception, [oc]
    is guaranteed to be closed. *)

val with_open_text : string -> (t -> 'a) -> 'a
(** Like {!with_open_bin}, but the channel is opened in text mode (see
    {!open_text}). *)

val with_open_gen : open_flag list -> int -> string -> (t -> 'a) -> 'a
(** Like {!with_open_bin}, but can specify the opening mode and file permission,
    in case the file must be created (see {!open_gen}). *)

val seek : t -> int64 -> unit
(** [seek chan pos] sets the current writing position to [pos] for channel
    [chan]. This works only for regular files. On files of other kinds (such as
    terminals, pipes and sockets), the behavior is unspecified. *)

val pos : t -> int64
(** Return the current writing position for the given channel.  Does not work on
    channels opened with the [Open_append] flag (returns unspecified results).

    For files opened in text mode under Windows, the returned position is
    approximate (owing to end-of-line conversion); in particular, saving the
    current position with {!pos}, then going back to this position using {!seek}
    will not work.  For this programming idiom to work reliably and portably,
    the file must be opened in binary mode. *)

val length : t -> int64
(** Return the size (number of characters) of the regular file on which the
    given channel is opened.  If the channel is opened on a file that is not a
    regular file, the result is meaningless. *)

val close : t -> unit
(** Close the given channel, flushing all buffered write operations.  Output
    functions raise a [Sys_error] exception when they are applied to a closed
    output channel, except {!close} and {!flush}, which do nothing when applied
    to an already closed channel.  Note that {!close} may raise [Sys_error] if
    the operating system signals an error when flushing or closing. *)

val close_noerr : t -> unit
(** Same as {!close}, but ignore all errors. *)

val flush : t -> unit
(** Flush the buffer associated with the given output channel, performing all
    pending writes on that channel.  Interactive programs must be careful about
    flushing standard output and standard error at the right time. *)

val flush_all : unit -> unit
(** Flush all open output channels; ignore errors. *)

val output_char : t -> char -> unit
(** Write the character on the given output channel. *)

val output_byte : t -> int -> unit
(** Write one 8-bit integer (as the single character with that code) on the
    given output channel. The given integer is taken modulo 256. *)

val output_string : t -> string -> unit
(** Write the string on the given output channel. *)

val output_bytes : t -> bytes -> unit
(** Write the byte sequence on the given output channel. *)

val output : t -> bytes -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters from byte sequence [buf],
    starting at offset [pos], to the given output channel [oc].

    @raise Invalid_argument if [pos] and [len] do not designate a valid range of
    [buf]. *)

val output_substring : t -> string -> int -> int -> unit
(** Same as {!output} but take a string as argument instead of a byte
    sequence. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode oc true] sets the channel [oc] to binary mode: no
    translations take place during output.

    [set_binary_mode oc false] sets the channel [oc] to text mode: depending on
    the operating system, some translations may take place during output.  For
    instance, under Windows, end-of-lines will be translated from [\n] to
    [\r\n].

    This function has no effect under operating systems that do not distinguish
    between text mode and binary mode. *)

val set_buffered : t -> bool -> unit
(** [set_buffered oc true] sets the channel [oc] to {e buffered} mode. In this
    mode, data output on [oc] will be buffered until either the internal buffer
    is full or the function {!flush} or {!flush_all} is called, at which point
    it will be sent to the output device.

    [set_buffered oc false] sets the channel [oc] to {e unbuffered} mode. In
    this mode, data output on [oc] will be sent to the output device
    immediately.

    All channels are open in {e buffered} mode by default. *)

val is_buffered : t -> bool
(** [is_buffered oc] returns whether the channel [oc] is buffered (see
    {!set_buffered}). *)

val isatty : t -> bool
(** [isatty oc] is [true] if [oc] refers to a terminal or console window,
    [false] otherwise.

    @since 5.1 *)
