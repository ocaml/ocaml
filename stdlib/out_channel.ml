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

type t = out_channel

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

let stdout = Stdlib.stdout
let stderr = Stdlib.stderr
let open_text = Stdlib.open_out
let open_bin = Stdlib.open_out_bin
let open_gen = Stdlib.open_out_gen
let seek = Stdlib.LargeFile.seek_out
let pos = Stdlib.LargeFile.pos_out
let length = Stdlib.LargeFile.out_channel_length
let close = Stdlib.close_out
let close_noerr = Stdlib.close_out_noerr
let flush = Stdlib.flush
let flush_all = Stdlib.flush_all
let output_char = Stdlib.output_char
let output_byte = Stdlib.output_byte
let output_string = Stdlib.output_string
let output_bytes = Stdlib.output_bytes
let output = Stdlib.output
let output_substring = Stdlib.output_substring
let output_binary_int = Stdlib.output_binary_int
let set_binary_mode = Stdlib.set_binary_mode_out
