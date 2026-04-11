(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type extern_flags =
    No_sharing
  | Closures
  | Compat_32

(* note: this type definition is used in 'runtime/debugger.c' *)

external to_bytes: 'a -> extern_flags list -> bytes
    = "caml_output_value_to_bytes"
external to_string: 'a -> extern_flags list -> string
    = "caml_output_value_to_string"
external to_buffer_unsafe:
      bytes -> int -> int -> 'a -> extern_flags list -> int
    = "caml_output_value_to_buffer"

let to_buffer buff ofs len v flags =
  if ofs < 0 || len < 0 || ofs > Bytes.length buff - len
  then invalid_arg "Marshal.to_buffer: substring out of bounds"
  else to_buffer_unsafe buff ofs len v flags

let to_channel (oc : out_channel) v flags =
  let b = to_bytes v flags in
  output oc b 0 (Bytes.length b)

(* The functions below use byte sequences as input, never using any
   mutation. It makes sense to use non-mutated [bytes] rather than
   [string], because we really work with sequences of bytes, not
   a text representation.
*)

external from_bytes_unsafe: bytes -> int -> 'a = "caml_input_value_from_bytes"
external data_size_unsafe: bytes -> int -> int = "caml_marshal_data_size"

let header_size = 16
let data_size buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.data_size"
  else data_size_unsafe buff ofs
let total_size buff ofs = header_size + data_size buff ofs

let from_bytes buff ofs =
  if ofs < 0 || ofs > Bytes.length buff - header_size
  then invalid_arg "Marshal.from_bytes"
  else begin
    let len = data_size_unsafe buff ofs in
    if ofs > Bytes.length buff - (header_size + len)
    then invalid_arg "Marshal.from_bytes"
    else from_bytes_unsafe buff ofs
  end

let from_string buff ofs =
  (* Bytes.unsafe_of_string is safe here, as the produced byte
     sequence is never mutated *)
  from_bytes (Bytes.unsafe_of_string buff) ofs

let from_channel (ic : in_channel) =
  let header = Bytes.create header_size in
  really_input ic header 0 header_size;
  let data_len = data_size_unsafe header 0 in
  let data = Bytes.create (header_size + data_len) in
  Bytes.blit header 0 data 0 header_size;
  really_input ic data header_size data_len;
  from_bytes_unsafe data 0
