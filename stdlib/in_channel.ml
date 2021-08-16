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
let open' = Stdlib.open_in
let open_bin = Stdlib.open_in_bin
let open_gen = Stdlib.open_in_gen
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

let input_binary_int ic =
  match Stdlib.input_binary_int ic with
  | n -> Some n
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

let set_binary_mode = Stdlib.set_binary_mode_in
