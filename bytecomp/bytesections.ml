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

module Name = struct

  type raw_name = string

  type t =
    | CODE (** bytecode *)
    | CRCS (** crcs for modules *)
    | DATA (** global data (constant) *)
    | DBUG (** debug info *)
    | DLLS (** dll names *)
    | DLPT (** dll paths *)
    | PRIM (** primitives names *)
    | RNTM (** The path to the bytecode interpreter (use_runtime mode) *)
    | SYMB (** global identifiers *)
    | Other of raw_name

  let of_string name =
    match name with
    | "CODE" -> CODE
    | "DLPT" -> DLPT
    | "DLLS" -> DLLS
    | "DATA" -> DATA
    | "PRIM" -> PRIM
    | "SYMB" -> SYMB
    | "DBUG" -> DBUG
    | "CRCS" -> CRCS
    | "RNTM" -> RNTM
    | name   ->
        if String.length name <> 4 then
          invalid_arg "Bytesections.Name.of_string: must be of size 4";
        Other name

  let to_string = function
    | CODE -> "CODE"
    | DLPT -> "DLPT"
    | DLLS -> "DLLS"
    | DATA -> "DATA"
    | PRIM -> "PRIM"
    | SYMB -> "SYMB"
    | DBUG -> "DBUG"
    | CRCS -> "CRCS"
    | RNTM -> "RNTM"
    | Other n -> n
end

type section_entry = {
  name : Name.t;
  pos  : int;
  len  : int;
}

type section_table = {
   sections : section_entry list;
   first_pos : int
}

(* Recording sections *)
type toc_writer = {
  (* List of all sections, in reverse order *)
  mutable section_table_rev : section_entry list;
  mutable section_prev : int;
  outchan : out_channel;
}

let init_record outchan : toc_writer =
  let pos = pos_out outchan in
  { section_prev = pos;
    section_table_rev = [];
    outchan }

let record t name =
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
       let name = Name.to_string name in
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
  let toc_pos = pos_trailer - 8 * num_sections in
  seek_in ic toc_pos;
  let section_table_rev = ref [] in
  for _i = 1 to num_sections do
    let name = Name.of_string (really_input_string ic 4) in
    let len = input_binary_int ic in
    section_table_rev := (name, len) :: !section_table_rev
  done;
  let first_pos, sections =
    List.fold_left (fun (pos, l) (name, len) ->
        let section = {name; pos = pos - len; len} in
        (pos - len, section :: l)) (toc_pos, []) !section_table_rev
  in
  { sections; first_pos }

let all t = t.sections

let pos_first_section t = t.first_pos

let find_section t name =
  let rec find = function
    | [] -> raise Not_found
    | {name = n; pos; len} :: rest ->
        if n = name
        then pos, len
        else find rest
  in find t.sections

(* Position ic at the beginning of the section named "name",
   and return the length of that section.  Raise Not_found if no
   such section exists. *)

let seek_section t ic name =
  let pos, len = find_section t name in
  seek_in ic pos; len

(* Return the contents of a section, as a string *)

let read_section_string t ic name =
  really_input_string ic (seek_section t ic name)

(* Return the contents of a section, as marshalled data *)

let read_section_struct t ic name =
  ignore (seek_section t ic name);
  input_value ic
