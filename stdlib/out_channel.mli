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

(** {2 General output functions} *)

type open_flag =
  | Append
  | Create of int
  | Exclusive
  | Truncate
  | Binary
  | Non_blocking

val open_ : ?flags:open_flag list -> string -> t
(** Open the named file for writing, and return a new output channel on that
    file, positioned at the beginning of the file. The file is truncated to zero
    length if it already exists. It is created if it does not already exists. *)

val with_file : ?flags:open_flag list -> string -> (t -> 'a) -> 'a
(** [with_file ?flags filename f] opens the file named [filename] for writing
    according to [flags], invokes [f] to process the contents of that file then,
    once [f] has returned or triggered an exception, closes the file before
    proceeding. *)

val close : t -> unit
(** Close the given channel, flushing all buffered write operations.
    Output functions raise a [Sys_error] exception when they are
    applied to a closed output channel, except [close_out] and [flush],
    which do nothing when applied to an already closed channel.
    Note that [close_out] may raise [Sys_error] if the operating
    system signals an error when flushing or closing. *)

val flush : t -> unit
(** Flush the buffer associated with the given output channel,
    performing all pending writes on that channel.
    Interactive programs must be careful about flushing standard
    output and standard error at the right time. *)

val flush_all : unit -> unit
(** Flush all open output channels; ignore errors. *)

val output_char : t -> char -> unit
(** Write the character on the given output channel. *)

val output_string : t -> string -> unit
(** Write the string on the given output channel. *)

val output_bytes : t -> bytes -> unit
(** Write the byte sequence on the given output channel. *)

val output : t -> bytes -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters from byte sequence [buf],
    starting at offset [pos], to the given output channel [oc].
    Raise [Invalid_argument "output"] if [pos] and [len] do not
    designate a valid range of [buf]. *)

val output_substring : t -> string -> int -> int -> unit
(** Same as [output] but take a string as argument instead of
    a byte sequence. *)

val output_byte : t -> int -> unit
(** Write one 8-bit integer (as the single character with that code)
    on the given output channel. The given integer is taken modulo
    256. *)

val output_binary_int : t -> int -> unit
(** Write one integer in binary format (4 bytes, big-endian)
    on the given output channel.
    The given integer is taken modulo 2{^32}.
    The only reliable way to read it back is through the
    {!input_binary_int} function. The format is compatible across
    all machines for a given version of OCaml. *)

val seek : t -> int64 -> unit
(** [seek chan pos] sets the current writing position to [pos]
    for channel [chan]. This works only for regular files. On
    files of other kinds (such as terminals, pipes and sockets),
    the behavior is unspecified. *)

val pos : t -> int64
(** Return the current writing position for the given channel.  Does
    not work on channels opened with the [Open_append] flag (returns
    unspecified results). *)

val length : t -> int64
(** Return the size (number of characters) of the regular file
    on which the given channel is opened.  If the channel is opened
    on a file that is not a regular file, the result is meaningless. *)

val set_binary_mode : t -> bool -> unit
(** [set_binary_mode oc true] sets the channel [oc] to binary
    mode: no translations take place during output.
    [set_binary_mode oc false] sets the channel [oc] to text
    mode: depending on the operating system, some translations
    may take place during output.  For instance, under Windows,
    end-of-lines will be translated from [\n] to [\r\n].
    This function has no effect under operating systems that
    do not distinguish between text mode and binary mode. *)

val output_line : t -> string -> unit
(** Output a line terminated by a newline character. *)

val output_lines : t -> string list -> unit
(** Outputs a list of lines, each terminated by a newline character *)
