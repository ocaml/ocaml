(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type extern_flags =
    No_sharing
  | Closures

external to_channel: out_channel -> 'a -> extern_flags list -> unit
    = "output_value"
external to_string: 'a -> extern_flags list -> string
    = "output_value_to_string"
external to_buffer_unsafe:
      string -> int -> int -> 'a -> extern_flags list -> unit
    = "output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 or len < 0 or ofs + len > String.length buff
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

external from_channel: in_channel -> 'a = "input_value"
external from_string_unsafe: string -> int -> 'a = "input_value_from_string"
external data_size_unsafe: string -> int -> int = "marshal_data_size"

let header_size = 20
let data_size buff ofs =
  if ofs < 0 || ofs + header_size > String.length buff
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_string buff ofs =
  if ofs < 0 || ofs + header_size > String.length buff
  then invalid_arg "Marshal.from_size"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs + header_size + len > String.length buff
    then invalid_arg "Marshal.from_string"
    else from_string_unsafe buff ofs
  end  
