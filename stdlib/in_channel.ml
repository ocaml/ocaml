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

type t = in_channel

type open_flag = Stdlib.open_flag =
  | Open_rdonly
  | Open_wronly
  | Open_append
  | Open_creat
  | Open_trunc
  | Open_excl
  | Open_binary
  | Open_text
  | Open_nonblock

let stdin = Stdlib.stdin
let open_bin = Stdlib.open_in_bin
let open_text = Stdlib.open_in
let open_gen = Stdlib.open_in_gen

let with_open openfun s f =
  let ic = openfun s in
  Fun.protect ~finally:(fun () -> Stdlib.close_in_noerr ic)
    (fun () -> f ic)

let with_open_bin s f =
  with_open Stdlib.open_in_bin s f

let with_open_text s f =
  with_open Stdlib.open_in s f

let with_open_gen flags perm s f =
  with_open (Stdlib.open_in_gen flags perm) s f

let seek = Stdlib.LargeFile.seek_in
let pos = Stdlib.LargeFile.pos_in
let length = Stdlib.LargeFile.in_channel_length
let close = Stdlib.close_in
let close_noerr = Stdlib.close_in_noerr

let input_char ic =
  match Stdlib.input_char ic with
  | c -> Some c
  | exception End_of_file -> None

let input_byte ic =
  match Stdlib.input_byte ic with
  | n -> Some n
  | exception End_of_file -> None

let input_line ic =
  match Stdlib.input_line ic with
  | s -> Some s
  | exception End_of_file -> None

let input = Stdlib.input

let really_input ic buf pos len =
  match Stdlib.really_input ic buf pos len with
  | () -> Some ()
  | exception End_of_file -> None

let really_input_string ic len =
  match Stdlib.really_input_string ic len with
  | s -> Some s
  | exception End_of_file -> None

let input_all ic =
  let rec loop buf ofs =
    let len = Bytes.length buf in
    if len > ofs then
      let rem = len - ofs in
      let r = Stdlib.input ic buf ofs rem in
      if r = rem then
        Bytes.unsafe_to_string buf
      else if r = 0 then
        Bytes.sub_string buf 0 ofs
      else
        loop buf (ofs + r)
    else
      let new_buf =
        let new_len = 2 * Bytes.length buf in
        let new_len =
          if new_len <= Sys.max_string_length then
            new_len
          else if len < Sys.max_string_length then
            Sys.max_string_length
          else
            failwith "In_channel.input_all: cannot grow buffer"
        in
        let new_buf = Bytes.create new_len in
        Bytes.blit buf 0 new_buf 0 ofs;
        new_buf
      in
      loop new_buf ofs
  in
  let initial_size =
    try
      Stdlib.in_channel_length ic - Stdlib.pos_in ic
    with Sys_error _ ->
      0
  in
  let initial_size = if initial_size <= 0 then 65536 else initial_size in
  loop (Bytes.create initial_size) 0

let set_binary_mode = Stdlib.set_binary_mode_in
