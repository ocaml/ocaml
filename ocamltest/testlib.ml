(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Miscellaneous library functions *)

let is_blank c =
  c = ' ' || c = '\012' || c = '\n' || c = '\r' || c =  '\t'

let words s =
  let l = String.length s in
  let i = ref (l-1) and j = ref (l-1) in
  let find_word () =
    while !j >= 0 && (is_blank s.[!j]) do decr j; done;
    if !j>=0 then begin
      i := !j - 1;
      while !i >= 0 && (not (is_blank s.[!i])) do decr i; done;
      let word = String.sub s (!i+1) (!j - !i) in
      j := !i - 1;
      Some word
    end else None in
  let rec f words_acc = match find_word() with
    | None -> words_acc
    | Some word -> f (word :: words_acc) in
  f []

let file_is_empty filename =
  let ic = open_in filename in
  let filesize = in_channel_length ic in
  close_in ic;
  filesize = 0

let string_of_location loc =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  Location.print_loc fmt loc;
  Buffer.contents buf
