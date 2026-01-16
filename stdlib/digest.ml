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

(* Utility functions *)

let hex_of_string d =
  let char_hex n =
    Char.chr (if n < 10 then Char.code '0' + n
                        else Char.code 'a' + n - 10) in
  let len = String.length d in
  let result = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    let x = Char.code d.[i] in
    Bytes.unsafe_set result (i*2) (char_hex (x lsr 4));
    Bytes.unsafe_set result (i*2+1) (char_hex (x land 0x0f));
  done;
  Bytes.unsafe_to_string result

let string_of_hex s =
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> invalid_arg "Digest.of_hex" in
  let byte i = digit s.[i] lsl 4 + digit s.[i+1] in
  String.init (String.length s / 2) (fun i -> Char.chr (byte (2 * i)))

(* Generic interface *)

module type S = sig
  type t = string
  val hash_length : int
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val string : string -> t
  val bytes : bytes -> t
  val substring : string -> int -> int -> t
  val subbytes : bytes -> int -> int -> t
  val channel : in_channel -> int -> t
  val file : string -> t
  val output : out_channel -> t -> unit
  val input : in_channel -> t
  val to_hex : t -> string
  val of_hex : string -> t
end

(* BLAKE2 hashing, parameterized by hash size *)

module BLAKE2 (X: sig val hash_length : int end) : S = struct

  type t = string

  let hash_length =
    if X.hash_length < 1 || X.hash_length > 64
    then invalid_arg "Digest.BLAKE2: wrong hash size";
    X.hash_length

  let compare = String.compare
  let equal = String.equal

  type state

  external create_gen: int -> string -> state = "caml_blake2_create"
  external update: state -> bytes -> int -> int -> unit = "caml_blake2_update"
  external final: state -> int -> t = "caml_blake2_final"
  external unsafe_string: int -> string -> string -> int -> int -> t
                        = "caml_blake2_string"
  external unsafe_bytes: int -> string -> bytes -> int -> int -> t
                        = "caml_blake2_bytes"

  let create () = create_gen hash_length ""

  let string str =
    unsafe_string hash_length "" str 0 (String.length str)

  let bytes b =
    unsafe_bytes hash_length "" b 0 (Bytes.length b)

  let substring str ofs len =
    if ofs < 0 || len < 0 || ofs > String.length str - len
    then invalid_arg "Digest.substring";
    unsafe_string hash_length "" str ofs len

  let subbytes b ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length b - len
    then invalid_arg "Digest.subbytes";
    unsafe_bytes hash_length "" b ofs len

  let channel ic toread =
    let buf_size = 4096 in
    let buf = Bytes.create buf_size in
    let ctx = create () in
    if toread < 0 then begin
      let rec do_read () =
        let n = In_channel.input ic buf 0 buf_size in
        if n = 0
        then final ctx hash_length
        else (update ctx buf 0 n; do_read ())
      in do_read ()
    end else begin
      let rec do_read toread =
        if toread = 0 then final ctx hash_length else begin
          let n = In_channel.input ic buf 0 (Int.min buf_size toread) in
          if n = 0
          then raise End_of_file
          else begin
            update ctx buf 0 n;
            do_read (toread - n)
          end
        end
      in do_read toread
    end

  let file filename =
    In_channel.with_open_bin filename (fun ic -> channel ic (-1))

  let output chan digest = output_string chan digest

  let input chan = really_input_string chan hash_length

  let to_hex d =
    if String.length d <> hash_length then invalid_arg "Digest.to_hex";
    hex_of_string d

  let of_hex s =
    if String.length s <> hash_length * 2 then invalid_arg "Digest.of_hex";
    string_of_hex s

end

module BLAKE128 = BLAKE2(struct let hash_length = 16 end)
module BLAKE256 = BLAKE2(struct let hash_length = 32 end)
module BLAKE512 = BLAKE2(struct let hash_length = 64 end)

(* MD5 hashing *)

module MD5 = struct

  type t = string

  let hash_length = 16

  let compare = String.compare
  let equal = String.equal

  external unsafe_string: string -> int -> int -> t = "caml_md5_string"
  external unsafe_bytes: bytes -> int -> int -> t = "caml_md5_bytes"
  external channel: in_channel -> int -> t = "caml_md5_chan"

  let string str =
    unsafe_string str 0 (String.length str)

  let bytes b =
    unsafe_bytes b 0 (Bytes.length b)

  let substring str ofs len =
    if ofs < 0 || len < 0 || ofs > String.length str - len
    then invalid_arg "Digest.substring"
    else unsafe_string str ofs len

  let subbytes b ofs len =
    if ofs < 0 || len < 0 || ofs > Bytes.length b - len
    then invalid_arg "Digest.subbytes"
    else unsafe_bytes b ofs len

  let file filename =
    In_channel.with_open_bin filename (fun ic -> channel ic (-1))

  let output chan digest = output_string chan digest

  let input chan = really_input_string chan 16

  let to_hex d =
    if String.length d <> 16 then invalid_arg "Digest.to_hex";
    hex_of_string d

  let of_hex s =
    if String.length s <> 32 then invalid_arg "Digest.from_hex";
    string_of_hex s

end

(* Default exported implementation is MD5 *)

include MD5

let from_hex = of_hex
