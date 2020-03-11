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


type t = in_channel

let stdin = stdin

type open_flag =
  | Create of int
  | Exclusive
  | Binary
  | Non_blocking

let open_ ?flags:(flags0 = []) path =
  let aux (flags, perm) = function
    | Binary -> Open_binary :: flags, perm
    | Create perm -> Open_creat :: flags, perm
    | Exclusive -> Open_excl :: flags, perm
    | Non_blocking -> Open_nonblock :: flags, perm
  in
  let flags, perm = List.fold_left aux ([Open_rdonly], 0) flags0 in
  let flags = if not (List.mem Open_binary flags) then Open_text :: flags else flags in
  open_in_gen flags perm path

let with_file ?flags path f =
  let ic = open_ ?flags path in
  Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> f ic)

let close = close_in
let close_noerr = close_in_noerr
let input = input
let really_input ic b pos len =
  try Some (really_input ic b pos len) with End_of_file -> None
let really_input_string ic len =
  try Some (really_input_string ic len) with End_of_file -> None
let input_char ic =
  try Some (input_char ic) with End_of_file -> None
let input_byte ic =
  try Some (input_byte ic) with End_of_file -> None
let input_binary_int ic =
  try Some (input_binary_int ic) with End_of_file -> None
let input_line ic =
  try Some (input_line ic) with End_of_file -> None

let fold_lines f x ic =
  let rec loop acc =
    match input_line ic with
    | None -> acc
    | Some l -> loop (f acc l)
  in
  loop x

let input_lines ic =
  List.rev (fold_lines (fun acc l -> l :: acc) [] ic)

let iter_lines f ic =
  fold_lines (fun () l -> f l) () ic

let input_to_string ic =
  let n = Int64.sub (LargeFile.in_channel_length ic) (LargeFile.pos_in ic) in
  if Int64.compare n (Int64.of_int Sys.max_string_length) > 0 then None
  else really_input_string ic (Int64.to_int n)

let seek = LargeFile.seek_in
let pos = LargeFile.pos_in
let length = LargeFile.in_channel_length
let set_binary_mode = set_binary_mode_in

external get_binary_mode : in_channel -> bool = "caml_ml_channel_binary_mode"
