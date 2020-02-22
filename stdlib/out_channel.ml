(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)


type t = out_channel

let stdout = stdout
let stderr = stderr

type open_flag =
  | Append
  | Create of int
  | Exclusive
  | Truncate
  | Binary
  | Non_blocking

let open_ ?flags:(flags0 = []) path =
  let aux (flags, perm) = function
    | Append -> Open_append :: flags, perm
    | Create perm -> Open_creat :: flags, perm
    | Exclusive -> Open_excl :: flags, perm
    | Truncate -> Open_trunc :: flags, perm
    | Binary -> Open_binary :: flags, perm
    | Non_blocking -> Open_nonblock :: flags, perm
  in
  let flags, perm = List.fold_left aux ([Open_wronly], 0o666) flags0 in
  let flags = if not (List.mem Open_binary flags) then Open_text :: flags else flags in
  open_out_gen flags perm path

let with_file ?flags path f =
  let oc = open_ ?flags path in
  Fun.protect ~finally:(fun () -> close_out_noerr oc) (fun () -> f oc)

let close = close_out
let close_noerr = close_out_noerr
let flush = flush
let flush_all = flush_all
let output_char = output_char
let output_string = output_string
let output_bytes = output_bytes
let output = output
let output_substring = output_substring
let output_byte = output_byte
let output_binary_int = output_binary_int
let seek = LargeFile.seek_out
let pos = LargeFile.pos_out
let length = LargeFile.out_channel_length
let set_binary_mode = set_binary_mode_out
let output_line oc l = output_string oc l; output_char oc '\n'
let output_lines oc l = List.iter (output_line oc) l
