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


(** Output channels.

    @since 4.10 *)

type t = out_channel

val stdout : t
(** Standard output. *)

val stderr : t
(** Standard error. *)

(** {1 General output functions} *)

type open_flag =
  | Append
  | Create of int
  | Exclusive
  | Truncate
  | Binary
  | Non_blocking

val open_ : ?flags:open_flag list -> string -> t
(** [open_ ?flags fn] opens the file [fn] for writing, and returns a new output
    channel on that file, positioned at the beginning of the file. The file is
    truncated to zero length if it already exists. It is created if it does not
    already exists. *)

val with_file : ?flags:open_flag list -> string -> (t -> 'a) -> 'a
(** [with_file ?flags filename f] opens the file named [filename] for writing
    according to [flags], invokes [f] to process the contents of that file then,
    once [f] has returned or triggered an exception, closes the file before
    proceeding. *)

val close : t -> unit
(** [close oc] closes the channel [oc], flushing all buffered write operations.
    Output functions raise a [Sys_error] exception when they are applied to a
    closed output channel, except [close] and [flush], which do nothing when
    applied to an already closed channel.  Note that [close] may raise
    [Sys_error] if the operating system signals an error when flushing or
    closing. *)

val close_noerr : t -> unit
(** [close_noerr oc] is like [close oc] but ignores all errors. *)

val flush : t -> unit
(** [flush oc] flushes the buffer associated with the given output channel,
    performing all pending writes on that channel. Interactive programs must be
    careful about flushing standard output and standard error at the right
    time. *)

val flush_all : unit -> unit
(** [flush_all ()] flushes all open output channels; ignore errors. *)

val output_char : t -> char -> unit
(** [output_char oc c] writes the character [c] on the output channel [oc]. *)

val output_string : t -> string -> unit
(** [output_string oc s] writes the string [s] on the output channel [oc]. *)

val output_bytes : t -> bytes -> unit
(** [output_bytes oc b] writes the byte sequence [b] on the output channel
    [oc]. *)

val output : t -> bytes -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters from byte sequence [buf],
    starting at offset [pos], to the output channel [oc].  Raises
    [Invalid_argument] if [pos] and [len] do not designate a valid range of
    [buf]. *)

val output_substring : t -> string -> int -> int -> unit
(** [output_substring oc s pos len] is like [output] but takes an argument of
    type [string] instead of [bytes]. *)

val output_byte : t -> int -> unit
(** [output_byte oc n] write one 8-bit integer [n] (as the single character with
    that code) on the given output channel. Integer [n] is taken modulo 256. *)

val output_binary_int : t -> int -> unit
(** [output_binary_int oc n] writes the integer [n] in binary format (4 bytes,
    big-endian) on the channel [oc]. Integer [n] is taken modulo 2{^32}. The
    only reliable way to read it back is through the
    {!In_channel.input_binary_int} function. The format is compatible across all
    machines for a given version of OCaml. *)

val seek : t -> int64 -> unit
(** [seek chan pos] sets the current writing position to [pos] for channel
    [chan]. This works only for regular files. On files of other kinds (such as
    terminals, pipes and sockets), the behavior is unspecified. *)

val pos : t -> int64
(** [pos oc] returns the current writing position for the given channel. Does
    not work on channels opened with the [Open_append] flag (returns unspecified
    results). *)

val length : t -> int64
(** Return the size (number of characters) of the regular file on which the
    given channel is opened.  If the channel is opened on a file that is not a
    regular file, the result is meaningless. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode oc b] sets the channel [oc] to binary mode (if [b] is
    [true]) or text mode (if [b] is [false]).  In binary mode, no translations
    take place during output. In text mode, depending on the operating system,
    some translations may take place during output.  For instance, under
    Windows, end-of-lines will be translated from [\n] to [\r\n].  This function
    has no effect under operating systems that do not distinguish between text
    mode and binary mode. *)

val output_line : t -> string -> unit
(** [output_line oc s] writes [s] followed by a newline character to [oc].

    Note that if [oc] is in text mode (see {!set_binary_mode}), then the
    sequence [\r\n] will be used instead of the newline character. *)

val output_lines : t -> string list -> unit
(** [output_lines oc l] is [List.iter (output_line oc) l]. *)
