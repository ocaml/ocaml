(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Pierre Weis, projet Cristal, INRIA Rocquencourt          *)
(*                                                                     *)
(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* Module [Buffer]: string buffers, automatically expanded as necessary.
   Provide accumulative concatenation of strings in linear time,
   instead of quadratic time.
   Also a convenient abstraction of strings and channels
   for input output. *)

type t;;
     (* The abstract type of buffers. *)

(* Basic operations on buffers. *)
val create : int -> t;;
     (* [create n] returns a fresh buffer of length [n].
        Further calls to [reset] will shrink the buffer to length [n].
        The initial contents of the buffer is not specified. *)
val of_string : string -> t;;
     (* Creates a new buffer. Its initial contents is the string given. *)
val to_string : t -> string;;
     (* Returns a copy of the actual contents of the buffer.
        The writing position of the buffer is reset to zero. *)
val contents : t -> string;;
     (* Returns a copy of the actual contents of the buffer.
        The writing position of the buffer is left unchanged. *)
val length : t -> int;;
     (* [length buff n] returns the actual length of buffer [buff]. *)
val position : t -> int;;
     (* Returns the actual writing position of the buffer. *)
val set_position : t -> int -> unit;;
     (* [set_position buff pos] sets the writing position of the buffer
        to [pos]. Raises [Invalid_argument "set_position"]
        if [0 <= pos < length buff] does not hold. *)
val give_room : t -> int -> unit;;
     (* [give_room b n] gives room to output at least [n] characters in
        the buffer. *)
val clear : t -> unit;;
     (* Reset to zero the writing position of the buffer. *)
val reset : t -> unit;;
     (* [reset buff] resets the buffer to its initial length and resets
        to zero the writing position of the buffer. The underlying
        storage character string of the buffer is restored to its
        initial value. *)

(* The [printf] facility for buffers. *)
val bprintf : t -> ('a, t, unit) format -> 'a;;
     (* [bprintf] has the same functionality as [fprintf] but material
        is output on buffers. See the module [printf] for details. *)

(* Output function for buffers. *)
val output_string : t -> string -> unit;;
val output_char : t -> char -> unit;;
val output : t -> string -> int -> int -> unit;;
     (* Similar to the usual functions from module [Pervasives],
        but output is done on the buffer argument. *)
val output_buffer : t -> t -> unit;;
     (* [output_buffer b1 b2] copies the contents of buffer [b2] into
        the buffer [b1].
        The writing position of buffer [b2] is left unchanged. *)
val output_buffer_out : out_channel -> t -> unit;;
     (* [output_buffer_out oc b] outputs the contents of buffer [b] to
        the out channel [oc].
        The writing position of the buffer is left unchanged. *)

(* Connection between buffers and out channels. *)
val connect_out : t -> out_channel -> unit;;
     (* Connects the buffer to the given out channel.
        Overflows or explicit flushes now cause the buffer to be
        output on the given out channel. *)
val disconnect_out : t -> unit;;
     (* Flushes the buffer to its out channel, reset the buffer, then
        suppress the connection between the buffer and the channel.
        The out channel is not closed.
        Nothing happens if the buffer is not connected to any out channel. *)
val flush : t -> unit;;
     (* Outputs the contents of the buffer to its out channel, and reset
        to zero the writing position of the buffer.
        The out channel is not flushed.
        Nothing happens if the buffer is not connected to any out
        channel. *)

(* Reading characters from input channels. *)
val input : in_channel -> t -> int -> int;;
     (* [input ic buff len] attempts to read [len] characters from input
        channel [ic] and stores them in buffer [buff].
        It returns the actual number of characters read.  *) 
val really_input : in_channel -> t -> int -> unit;;
     (* Same as the [input] function above, but using the input function
        [Pervasives.really_input] instead of [Pervasives.input].
        Raise [End_of_file] if the end of file is reached before [len]
        characters have been read. *)
val read_in_channel : in_channel -> t -> unit;;
     (* [Buffer.read_in_channel ic buff] copies the entire contents of
        input channel ic in buffer [buff]. *)

(* Text file operations. *)
val of_file : string -> t;;
     (* [Buffer.of_file file_name] returns a buffer filled with the
        contents of text file [file_name]. *)
val to_file : t -> string -> unit;;
     (* [Buffer.to_file buff file_name] writes the contents of buffer
        [buff] to the text file [file_name]. *)
