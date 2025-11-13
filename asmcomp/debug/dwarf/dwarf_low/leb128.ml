(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let write_uleb128 buf value =
  let rec loop v =
    let byte = v land 0x7f in
    let v = v lsr 7 in
    if v <> 0 then begin
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop v
    end else begin
      Buffer.add_char buf (Char.chr byte)
    end
  in
  if value < 0 then
    invalid_arg "write_uleb128: value must be non-negative";
  loop value

let write_sleb128 buf value =
  let rec loop v =
    let byte = v land 0x7f in
    let v = v asr 7 in
    (* Check if we need more bytes *)
    let more = not ((v = 0 && (byte land 0x40) = 0) ||
                    (v = -1 && (byte land 0x40) <> 0)) in
    if more then begin
      Buffer.add_char buf (Char.chr (byte lor 0x80));
      loop v
    end else begin
      Buffer.add_char buf (Char.chr byte)
    end
  in
  loop value

let encode_uleb128 value =
  let buf = Buffer.create 8 in
  write_uleb128 buf value;
  Bytes.of_string (Buffer.contents buf)

let encode_sleb128 value =
  let buf = Buffer.create 8 in
  write_sleb128 buf value;
  Bytes.of_string (Buffer.contents buf)

let uleb128_size value =
  if value < 0 then
    invalid_arg "uleb128_size: value must be non-negative";
  let rec loop v acc =
    if v = 0 then acc
    else loop (v lsr 7) (acc + 1)
  in
  if value = 0 then 1 else loop value 0

let sleb128_size value =
  let rec loop v acc =
    let byte = v land 0x7f in
    let v = v asr 7 in
    let done_ = (v = 0 && (byte land 0x40) = 0) ||
                (v = -1 && (byte land 0x40) <> 0) in
    if done_ then acc + 1
    else loop v (acc + 1)
  in
  loop value 0
