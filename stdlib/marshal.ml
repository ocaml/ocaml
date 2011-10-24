(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type extern_flags =
    No_sharing
  | Closures
(* note: this type definition is used in 'byterun/debugger.c' *)

external to_channel: out_channel -> 'a -> extern_flags list -> unit
    = "caml_output_value"
external to_string: 'a -> extern_flags list -> string
    = "caml_output_value_to_string"
external to_buffer_unsafe:
      string -> int -> int -> 'a -> extern_flags list -> int
    = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > String.length buff - len
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

external from_channel: in_channel -> 'a = "caml_input_value"
external from_string_unsafe: string -> int -> 'a
                           = "caml_input_value_from_string"
external data_size_unsafe: string -> int -> int = "caml_marshal_data_size"

let header_size = 20
let data_size buff ofs =
  if ofs < 0 || ofs > String.length buff - header_size
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_string buff ofs =
  if ofs < 0 || ofs > String.length buff - header_size
  then invalid_arg "Marshal.from_size"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs > String.length buff - (header_size + len)
    then invalid_arg "Marshal.from_string"
    else from_string_unsafe buff ofs
  end
