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
  match f ic with
  | r -> Stdlib.close_in ic; r
  | exception e -> Stdlib.close_in_noerr ic; raise e

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
  let bufsz = 65536 in (* IO_BUFFER_SIZE *)
  let rec loop buf ofs =
    let buf =
      if Bytes.length buf - ofs >= bufsz then
        buf
      else begin
        let newbuf = Bytes.create (2 * Bytes.length buf + 1) in
        Bytes.blit buf 0 newbuf 0 ofs;
        newbuf
      end
    in
    let r = Stdlib.input ic buf ofs bufsz in
    if r = 0 then Bytes.sub_string buf 0 ofs else loop buf (ofs + r)
  in
  loop (Bytes.create bufsz) 0

let input_lines ic =
  let rec loop accu =
    match Stdlib.input_line ic with
    | s -> loop (s :: accu)
    | exception End_of_file -> List.rev accu
  in
  loop []

let set_binary_mode = Stdlib.set_binary_mode_in
