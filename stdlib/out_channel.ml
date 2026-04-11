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
let open_bin = Stdlib.open_out_bin
let open_text = Stdlib.open_out
let open_gen = Stdlib.open_out_gen

let with_open openfun s f =
  let oc = openfun s in
  Fun.protect ~finally:(fun () -> Stdlib.close_out_noerr oc)
    (fun () -> f oc)

let with_open_bin s f =
  with_open Stdlib.open_out_bin s f

let with_open_text s f =
  with_open Stdlib.open_out s f

let with_open_gen flags perm s f =
  with_open (Stdlib.open_out_gen flags perm) s f

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

let output_bigarray oc buf ofs len =
  if ofs < 0 || len < 0 || ofs > Bigarray.Array1.dim buf - len
  then invalid_arg "output_bigarray"
  else
    match Stdlib.CamlinternalChannel.native_out_channel_of oc with
    | Some nc ->
      Stdlib.CamlinternalChannel.unsafe_output_bigarray_native nc buf ofs len
    | None ->
      (* User-defined channel: copy via an intermediate bytes buffer. *)
      let tmp = Bytes.create len in
      for i = 0 to len - 1 do
        Bytes.unsafe_set tmp i (Bigarray.Array1.get buf (ofs + i))
      done;
      Stdlib.output oc tmp 0 len

let set_binary_mode = Stdlib.set_binary_mode_out

let is_binary_mode = Stdlib.out_channel_is_binary_mode

let set_buffered = Stdlib.set_buffered_out

let is_buffered = Stdlib.is_buffered_out

let isatty = Stdlib.out_channel_isatty

let of_buffer b =
  let ops : Buffer.t Stdlib.out_ops = {
    out_write = (fun b bytes ofs len ->
      Buffer.add_subbytes b bytes ofs len; len);
    out_flush = (fun _ -> ());
    out_close = (fun _ -> ());
    out_seek = None;
    out_pos = Some (fun b -> Int64.of_int (Buffer.length b));
    out_length = None;
    out_set_binary = None;
    out_isatty = None;
    out_is_binary = None;
    out_get_fd = None;
  } in
  Stdlib.make_out_channel b ops

let map_char f oc =
  let ops : out_channel Stdlib.out_ops = {
    out_write = (fun oc bytes ofs len ->
      let mapped = Bytes.init len (fun i ->
        f (Bytes.unsafe_get bytes (ofs + i))) in
      Stdlib.output oc mapped 0 len;
      len);
    out_flush = Stdlib.flush;
    out_close = Stdlib.close_out_noerr;
    out_seek = Some Stdlib.LargeFile.seek_out;
    out_pos = Some Stdlib.LargeFile.pos_out;
    out_length = Some Stdlib.LargeFile.out_channel_length;
    out_set_binary = Some Stdlib.set_binary_mode_out;
    out_isatty = Some Stdlib.out_channel_isatty;
    out_is_binary = Some Stdlib.out_channel_is_binary_mode;
    out_get_fd = Some Stdlib.CamlinternalChannel.out_channel_fd;
  } in
  Stdlib.make_out_channel oc ops

let make
    ~write
    ~flush
    ~close
    ?seek
    ?pos
    ?length
    ?set_binary
    ?isatty
    ?is_binary
    ?get_fd
    st =
  let ops : 'st Stdlib.out_ops = {
    out_write = write;
    out_flush = flush;
    out_close = close;
    out_seek = seek;
    out_pos = pos;
    out_length = length;
    out_set_binary = set_binary;
    out_isatty = isatty;
    out_is_binary = is_binary;
    out_get_fd = get_fd;
  } in
  Stdlib.make_out_channel st ops
