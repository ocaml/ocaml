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

(* "Expunge" a toplevel by removing compiler modules from the global map.
   Usage: expunge <source file> <dest file> <names of modules to keep> *)

open Misc
module String = Misc.Stdlib.String

let to_keep = ref String.Set.empty

let negate = Sys.argv.(3) = "-v"

let keep = function
  | Symtable.Global.Glob_predef _ -> true
  | Symtable.Global.Glob_compunit (Cmo_format.Compunit name) ->
    if negate then not (String.Set.mem name !to_keep)
    else (String.Set.mem name !to_keep)

let expunge_map tbl =
  Symtable.filter_global_map keep tbl

let expunge_crcs tbl =
  List.filter (fun (compunit, _crc) ->
    keep (Symtable.Global.Glob_compunit (Cmo_format.Compunit compunit))) tbl

let main () =
  let input_name = Sys.argv.(1) in
  let output_name = Sys.argv.(2) in
  for i = (if negate then 4 else 3) to Array.length Sys.argv - 1 do
    to_keep := String.Set.add (String.capitalize_ascii Sys.argv.(i)) !to_keep
  done;
  let ic = open_in_bin input_name in
  let toc = Bytesections.read_toc ic in
  seek_in ic 0;
  let oc =
    open_out_gen [Open_wronly; Open_creat; Open_trunc; Open_binary] 0o777
      output_name in
  let first_pos = Bytesections.pos_first_section toc in
  (* Copy the file up to the first section as is *)
  copy_file_chunk ic oc first_pos;
  (* Copy each section, modifying the symbol section in passing *)
  let toc_writer = Bytesections.init_record oc in
  List.iter
    (fun {Bytesections.name; pos; len} ->
       seek_in ic pos;
       begin match name with
         SYMB ->
           let global_map : Symtable.global_map = input_value ic in
           output_value oc (expunge_map global_map)
       | CRCS ->
           let crcs : (string * Digest.t option) list = input_value ic in
           output_value oc (expunge_crcs crcs)
       | _ ->
           copy_file_chunk ic oc len
       end;
       Bytesections.record toc_writer name)
    (Bytesections.all toc);
  (* Rewrite the toc and trailer *)
  Bytesections.write_toc_and_trailer toc_writer;
  (* Done *)
  close_in ic;
  close_out oc

let _ = main (); exit 0
