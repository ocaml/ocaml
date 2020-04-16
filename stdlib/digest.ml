(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Message digest (MD5) *)

type t = string

let compare = String.compare
let equal = String.equal

external unsafe_string: string -> int -> int -> t = "caml_md5_string"
external channel: in_channel -> int -> t = "caml_md5_chan"

let string str =
  unsafe_string str 0 (String.length str)

let bytes b = string (Bytes.unsafe_to_string b)

let substring str ofs len =
  if ofs < 0 || len < 0 || ofs > String.length str - len
  then invalid_arg "Digest.substring"
  else unsafe_string str ofs len

let subbytes b ofs len = substring (Bytes.unsafe_to_string b) ofs len

let file filename =
  let ic = open_in_bin filename in
  match channel ic (-1) with
    | d -> close_in ic; d
    | exception e -> close_in ic; raise e

let output chan digest =
  output_string chan digest

let input chan = really_input_string chan 16

let to_hex d =
  if String.length d <> 16 then invalid_arg "Digest.to_hex";
  String.to_hex d

let from_hex s =
  if String.length s <> 32 then invalid_arg "Digest.from_hex";
  try String.of_hex s with Failure _ -> invalid_arg "Digest.from_hex"
