(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of sections in bytecode executable files *)

type section_entry = {
  name : string;
  pos  : int;
  len  : int;
}

type section_table = section_entry list

(* Recording sections *)
type toc_writer = {
  (* List of all sections, in reverse order *)
  mutable section_table_rev : section_table;
  mutable section_prev : int;
  outchan : out_channel;
}

let init_record outchan : toc_writer =
  let pos = pos_out outchan in
  { section_prev = pos;
    section_table_rev = [];
    outchan }

let record t name =
  if String.length name <> 4 then
    invalid_arg "Bytesections.record: section name must be of size 4";
  let pos = pos_out t.outchan in
  if pos < t.section_prev then
    invalid_arg "Bytesections.record: out_channel offset moved backward";
  let entry = {name; pos = t.section_prev; len = pos - t.section_prev} in
  t.section_table_rev <- entry :: t.section_table_rev;
  t.section_prev <- pos

let write_toc_and_trailer t =
  let section_table = List.rev t.section_table_rev in
  List.iter
    (fun {name; pos = _; len} ->
       assert (String.length name = 4);
      output_string t.outchan name; output_binary_int t.outchan len)
    section_table;
  output_binary_int t.outchan (List.length section_table);
  output_string t.outchan Config.exec_magic_number

(* Read the table of sections from a bytecode executable *)

exception Bad_magic_number

let read_toc ic =
  let pos_trailer = in_channel_length ic - 16 in
  seek_in ic pos_trailer;
  let num_sections = input_binary_int ic in
  let header =
    really_input_string ic (String.length Config.exec_magic_number)
  in
  if header <> Config.exec_magic_number then raise Bad_magic_number;
  let toc_pos = (pos_trailer - 8 * num_sections) in
  seek_in ic toc_pos;
  let section_table_rev = ref [] in
  for _i = 1 to num_sections do
    let name = really_input_string ic 4 in
    let len = input_binary_int ic in
    section_table_rev := (name, len) :: !section_table_rev
  done;
  let _first_section, sections =
    List.fold_left (fun (pos, l) (name,len) ->
        let section = {name; pos = pos - len; len} in
        (pos - len, section :: l)) (toc_pos, []) !section_table_rev
  in
  sections

let find_section t name =
  let rec find = function
    | [] -> raise Not_found
    | {name = n; pos; len} :: rest ->
        if n = name
        then pos, len
        else find rest
  in find t

(* Position ic at the beginning of the section named "name",
   and return the length of that section.  Raise Not_found if no
   such section exists. *)

let seek_section t ic name =
  if String.length name <> 4 then
    invalid_arg "Bytesections.seek_section: section name must be of size 4";
  let pos, len = find_section t name in
  seek_in ic pos; len

(* Return the contents of a section, as a string *)

let read_section_string t ic name =
  really_input_string ic (seek_section t ic name)

(* Return the contents of a section, as marshalled data *)

let read_section_struct t ic name =
  ignore (seek_section t ic name);
  input_value ic
