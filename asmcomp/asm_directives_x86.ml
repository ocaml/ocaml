(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                      Elias Boutaleb, OCamlPro                          *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*   Copyright 2016 OCamlPro                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module D = X86_dsl.D
module X = X86_dsl

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16

let string_of_label label_name =
  match X86_proc.system with
  | S_macosx | S_win64 -> "L" ^ string_of_int label_name
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  | S_linux
  | S_mingw64
  | S_unknown -> ".L" ^ string_of_int label_name

let label label_name =
  D.qword (ConstLabel (string_of_label label_name))

let label_declaration ~label_name =
  (* CR mshinwell: should this always be QWORD?  (taken from emit.mlp) *)
  D.label (string_of_label label_name) ~typ:QWORD

let sections_seen = ref []

let switch_to_section (section : Asm_directives.section) =
  let first_occurrence =
    if List.mem section !sections_seen then false
    else begin
      sections_seen := section::!sections_seen;
      true
    end
  in
  let section_name, middle_part, attrs =
    match section, X86_proc.system with
    | Dwarf dwarf, S_macosx ->
      let name =
        match dwarf with
        | Debug_info -> "__debug_info"
        | Debug_abbrev -> "__debug_abbrev"
        | Debug_aranges -> "__debug_aranges"
        | Debug_loc -> "__debug_loc"
        | Debug_str -> "__debug_str"
        | Debug_line -> "__debug_line"
      in
      ["__DWARF"; name], None, ["regular"; "debug"]
    | Dwarf dwarf, _ ->
      let name =
        match dwarf with
        | Debug_info -> ".debug_info"
        | Debug_abbrev -> ".debug_abbrev"
        | Debug_aranges -> ".debug_aranges"
        | Debug_loc -> ".debug_loc"
        | Debug_str -> ".debug_str"
        | Debug_line -> ".debug_line"
      in
      let middle_part =
        if first_occurrence then
          Some ""
        else
          None
      in
      let attrs =
        if first_occurrence then
          ["%progbits"]
        else
          []
      in
      [name], middle_part, attrs
  in
  D.section section_name middle_part attrs;
  if first_occurrence then begin
    label_declaration ~label_name:(Asm_directives.label_for_section section)
  end

let cached_strings = ref ([] : (string * Linearize.label) list)

let reset () =
  cached_strings := [];
  sections_seen := []

let init () =
  reset ();
  match X86_proc.system with
  | S_macosx -> ()
  | _ ->
    (* Forward label references are illegal in gas. *)
    switch_to_section (Dwarf Debug_info);
    switch_to_section (Dwarf Debug_abbrev);
    switch_to_section (Dwarf Debug_aranges);
    switch_to_section (Dwarf Debug_loc);
    switch_to_section (Dwarf Debug_str);
    switch_to_section (Dwarf Debug_line)

let symbol_prefix = if X86_proc.system = X86_proc.S_macosx then "_" else ""

let escape_symbol s = X86_proc.string_of_symbol symbol_prefix s

let symbol sym =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  D.qword (ConstLabel (escape_symbol sym))

let symbol_plus_offset sym ~offset_in_bytes =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  let offset_in_bytes = Target_system.Address.to_int64 offset_in_bytes in
  D.qword (ConstAdd (ConstLabel (escape_symbol sym), Const offset_in_bytes))

let between_symbols ~upper ~lower =
  let upper = Linkage_name.to_string (Symbol.label upper) in
  let lower = Linkage_name.to_string (Symbol.label lower) in
  D.qword (ConstSub (
    ConstLabel (escape_symbol upper),
    ConstLabel (escape_symbol lower)))

let define_symbol sym =
  let name = Linkage_name.to_string (Symbol.label sym) in
  D.qword (ConstLabel (escape_symbol name));
  D.global name

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let upper = string_of_label upper in
  let lower = Linkage_name.to_string (Symbol.label lower) in
  let offset_upper = Target_system.Address.to_int64 offset_upper in
  D.qword (ConstSub (
    ConstAdd (ConstLabel upper, Const offset_upper),
    ConstLabel (escape_symbol lower)))

let temp_var_counter = ref 0
let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "Ltemp%d" id

let offset_into_section_label ~section ~label:upper
      ~(width : Asm_directives.width) =
  let lower = string_of_label (Asm_directives.label_for_section section) in
  let upper = string_of_label upper in
  let expr : X86_ast.constant =
    match X86_proc.system with
    | S_macosx ->
      let temp = new_temp_var () in
      (* Direct assignment, not ".set": the value of the expression cannot
         be computed until the operands have been relocated. *)
      D.direct_assignment temp
        (ConstSub (ConstLabel upper, ConstLabel lower));
      ConstLabel temp
    | _ ->
      ConstLabel upper
  in
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width.
     Same below. *)
  | Thirty_two -> D.long expr
  | Sixty_four -> D.qword expr

let offset_into_section_symbol ~section ~symbol
      ~(width : Asm_directives.width) =
  let lower = string_of_label (Asm_directives.label_for_section section) in
  let upper = Linkage_name.to_string (Symbol.label symbol) in
  let expr : X86_ast.constant =
    match X86_proc.system with
    | S_macosx ->
      let temp = new_temp_var () in
      D.direct_assignment temp
        (ConstSub (ConstLabel (escape_symbol upper), ConstLabel lower));
      ConstLabel temp
    | _ ->
      ConstLabel (escape_symbol upper)
  in
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width. *)
  | Thirty_two -> D.long expr
  | Sixty_four -> D.qword expr

let int8 i =
  D.byte (X.const (Int8.to_int i))

(* CR mshinwell: check these are the correct directives for both 64 and 32
   (same for the symbol case above). *)

(* CR mshinwell: Apple's doc says that ".word" is i386-specific.  Should
   maybe use ".short" instead everywhere.  Needs X86_dsl fixing *)
let int16 i =
  D.word (X.const (Int16.to_int i))

let int32 i =
  D.long (X.const_32 i)

let int64 i =
  D.qword (Const i)

let target_address (addr : Target_system.Address.t) =
  match addr with
  | Int32 i -> int32 i
  | Int64 i -> int64 i

let uleb128 i =
  D.uleb128 (Const i)

let sleb128 i =
  D.sleb128 (Const i)

let string str =
  D.bytes str

let cache_string str =
  match List.assoc str !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Linearize.new_label () in
    cached_strings := (str, label) :: !cached_strings;
    label

let emit_cached_strings () =
  List.iter (fun (str, label_name) ->
      label_declaration ~label_name;
      string str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := []
