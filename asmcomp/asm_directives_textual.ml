(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: fill this file in *)

(* Old code:

type t = {
  emit_string : string -> unit;
  emit_symbol : string -> unit;
  emit_label : Linearize.label -> unit;
  emit_label_declaration : label_name:Linearize.label -> unit;
  emit_section_declaration : section_name:string -> unit;
  emit_switch_to_section : section_name:string -> unit;
  target : [ `MacOS_X | `Other ];
  mutable strings : (string * Linearize.label) list;
}

let create ~emit_string ~emit_symbol ~emit_label ~emit_label_declaration
           ~emit_section_declaration ~emit_switch_to_section ~target =
  { emit_string; emit_symbol; emit_label; emit_label_declaration;
    emit_section_declaration; emit_switch_to_section; target;
    strings = [];
  }

let cache_string t s =
  try List.assoc s t.strings
  with Not_found -> begin
    let label = Linearize.new_label () in
    t.strings <- (s, label)::t.strings;
    label
  end

let emit_strings t =
  ListLabels.iter t.strings
    ~f:(fun (s, label_name) ->
          t.emit_label_declaration ~label_name;
          match t.target with
          | `MacOS_X ->
            t.emit_string (Printf.sprintf "\t.asciz\t\"%s\"\n" s)
          | `Other ->
            t.emit_string (Printf.sprintf "\t.string\t\"%s\"\n" s))

let emit_string t = t.emit_string
let emit_symbol t = t.emit_symbol
let emit_label t = t.emit_label
let emit_label_declaration t = t.emit_label_declaration

let emit_section_declaration t ~section_name =
  t.emit_section_declaration
    ~section_name:(Section_names.name section_name);
  emit_label_declaration t
    ~label_name:(Section_names.starting_label section_name)

let emit_switch_to_section t ~section_name =
  t.emit_switch_to_section ~section_name:(Section_names.name section_name)

let emit_symbol_alias _t ~old_sym:_ ~new_sym:_ = ()
(*
  t.emit_string "\t.global ";
  t.emit_symbol new_sym;
  t.emit_string "\n";
  t.emit_string "\t.set ";
  t.emit_symbol new_sym;
  t.emit_string ",";
  t.emit_symbol old_sym;
  t.emit_string "\n";
  t.emit_string "\t.type ";
  t.emit_symbol new_sym;
  t.emit_string ",@function\n"
*)

let emit_symbol_to_label_alias t ~old_label ~new_sym =
  t.emit_string "\t.global ";
  t.emit_symbol new_sym;
  t.emit_string "\n";
  t.emit_string "\t.set ";
  t.emit_symbol new_sym;
  t.emit_string ",";
  t.emit_label old_label;
  t.emit_string "\n";
  t.emit_string "\t.type ";
  t.emit_symbol new_sym;
  t.emit_string ",@function\n"

let target t = t.target

let emit_directive_for_offset t =
  match Dwarf_format.size () with
  | `Thirty_two -> emit_string t "\t.long\t"
  | `Sixty_four -> emit_string t "\t.quad\t"

let emit_directive_for_nativeint t =
  match Arch.size_addr with
  | 4 -> emit_string t "\t.long\t"
  | 8 -> emit_string t "\t.quad\t"
  | _ -> failwith "DWARF emitter does not understand Arch.size_addr's value"

let emit_as_native_int datum t =
  emit_directive_for_nativeint t;
  match datum with
  | `Native_int n ->
    emit_string t (Printf.sprintf "%nd\n" n)
  | `Label label ->
    emit_label t label;
    emit_string t "\n"
  | `String str ->
    emit_string t (Printf.sprintf "%s\n" str)
  | `Symbol symbol ->
    emit_symbol t symbol;
    emit_string t "\n"

let emit_code_address_from_label t label =
  emit_as_native_int (`Label label) t

let emit_code_address_from_symbol t symbol =
  emit_as_native_int (`Symbol symbol) t

let emit_eight_byte_int t i =
  emit_string t (sprintf "\t.quad\t0x%Lx\n" i)

let emit_four_byte_int t i =
  emit_string t (sprintf "\t.long\t0x%lx\n" i)

let emit_two_byte_int t i =
  let directive =
    match target t with
    | `Other -> "value"
    | `MacOS_X -> "short"
  in
  emit_string t (sprintf "\t.%s\t0x%x\n" directive i)

let emit_byte t i =
  emit_string t (sprintf "\t.byte\t0x%x\n" b)

let emit_byte_exn t i =
  emit_byte (Int8.of_int_exn i)

let emit_uleb128 t i =
  emit_string t (sprintf "\t.uleb128\t0x%x\n" i)

let emit_uleb128_from_int64 t i =
  emit_string t (sprintf "\t.uleb128\t0x%Lx\n" i)

let emit_leb128 t i =
  emit_string t (sprintf "\t.sleb128\t%d\n" i)

let emit_leb128_from_int64 t i =
  emit_string t (sprintf "\t.sleb128\t%Ld\n" i)

let emit_offset_into_section t datum section =
  match target t with
  | `Other ->
    emit_directive_for_offset ~emitter;
    begin match datum with
    | `Label label -> emit_label t label
    | `Symbol symbol -> emit_symbol t symbol
    end;
    emit_string t "\n"
  | `MacOS_X -> assert false

(* offset from label
let emit_offset_from_var t var = (* CR mshinwell: bad name? *)
  emit_directive_for_offset t;
  emit_string t var;
  emit_string t "\n"

let set_counter = ref 0
    begin match Emitter.target emitter with
    | `Other ->
      emit_directive_for_offset ~emitter;
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "\n"
    | `MacOS_X ->
      let count = !set_counter in
      let name = Printf.sprintf "Ldwarf_value%d" count in
      incr set_counter;
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = ";
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "-";
      Emitter.emit_label emitter (Section_names.starting_label section);
      Emitter.emit_string emitter "\n";
      emit (Offset_from_var name) ~emitter
    end

let emit_offset_from_symbol t name symbol =
  match target t with
  | `Other ->
    emit_directive_for_offset t;
    emit_symbol t symbol;
    emit_string t "\n"
  | `MacOS_X ->
    let count = !set_counter in
    let name = Printf.sprintf "Ldwarf_value%d" count in
    incr set_counter;
    emit_string t name;
    emit_string t " = ";
    emit_symbol t symbol;
    emit_string t "-";
    emit_label t (Section_names.starting_label section);
    emit_string t "\n";
    emit_offset_from_var t name
*)
*)
