(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  Distributed only by permission.                   *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* To assign numbers to globals and primitives *)

open Misc
open Asttypes
open Lambda
open Emitcode


(* Functions for batch linking *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string

exception Error of error

(* Tables for numbering objects *)

type 'a numtable =
  { num_cnt: int;               (* The next number *)
    num_tbl: ('a, int) Tbl.t } (* The table of already numbered objects *)

let empty_numtable = { num_cnt = 0; num_tbl = Tbl.empty }

let find_numtable nt key =
  Tbl.find key nt.num_tbl

let enter_numtable nt key =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = Tbl.add key n !nt.num_tbl };
  n

let incr_numtable nt =
  let n = !nt.num_cnt in
  nt := { num_cnt = n + 1; num_tbl = !nt.num_tbl };
  n

(* Global variables *)

let global_table = ref(empty_numtable : Ident.t numtable)
and literal_table = ref([] : (int * structured_constant) list)

let slot_for_getglobal id =
  try
    find_numtable !global_table id
  with Not_found ->
    raise(Error(Undefined_global(Ident.name id)))

let slot_for_setglobal id =
  enter_numtable global_table id

let slot_for_literal cst =
  let n = incr_numtable global_table in
  literal_table := (n, cst) :: !literal_table;
  n

(* The C primitives *)

let c_prim_table = ref(empty_numtable : string numtable)

let set_prim_table name =
  ignore(enter_numtable c_prim_table name)

let num_of_prim name =
  try
    find_numtable !c_prim_table name
  with Not_found ->
    if !Clflags.custom_runtime
    then enter_numtable c_prim_table name
    else raise(Error(Unavailable_primitive name))

let require_primitive name =
  if name.[0] <> '%' then ignore(num_of_prim name)

let all_primitives () =
  let prim = Array.create !c_prim_table.num_cnt "" in
  Tbl.iter (fun name number -> prim.(number) <- name) !c_prim_table.num_tbl;
  prim

let output_primitive_names outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    output_string outchan prim.(i); output_char outchan '\000'
  done

open Printf

let output_primitive_table outchan =
  let prim = all_primitives() in
  fprintf outchan "\
    #ifdef __cplusplus\n\
    extern \"C\" {\n\
    #endif\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "extern long %s();\n" prim.(i)
  done;
  fprintf outchan "typedef long (*primitive)();\n";
  fprintf outchan "primitive cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  %s,\n" prim.(i)
  done;
  fprintf outchan "  (primitive) 0 };\n";
  fprintf outchan "char * names_of_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  \"%s\",\n" prim.(i)
  done;
  fprintf outchan "  (char *) 0 };\n";
  fprintf outchan "\
    #ifdef __cplusplus\n\
    }\n\
    #endif\n"

(* Initialization for batch linking *)

let init () =
  (* Enter the predefined exceptions *)
  Array.iter 
    (fun name -> 
      let id =
        try List.assoc name Predef.builtin_values
        with Not_found -> fatal_error "Symtable.init" in
      let c = slot_for_setglobal id in
      let cst = Const_block(0, [Const_base(Const_string name)]) in
      literal_table := (c, cst) :: !literal_table)
    Runtimedef.builtin_exceptions;
  (* Enter the known C primitives *)
  if String.length !Clflags.use_prims > 0 then begin
      let ic = open_in !Clflags.use_prims in
      try
        while true do
          set_prim_table (input_line ic)
        done
      with End_of_file -> close_in ic
         | x -> close_in ic; raise x
  end else if String.length !Clflags.use_runtime > 0 then begin
    let primfile = Filename.temp_file "camlprims" "" in
    try
      if Sys.command(Printf.sprintf "%s -p > %s"
                                    !Clflags.use_runtime primfile) <> 0
      then raise(Error(Wrong_vm !Clflags.use_runtime));
      let ic = open_in primfile in
      try
        while true do
          set_prim_table (input_line ic)
        done
      with End_of_file -> close_in ic
         | x -> close_in ic; raise x
    with x -> remove_file primfile; raise x
  end else begin
    Array.iter set_prim_table
               Runtimedef.builtin_primitives
  end

(* Relocate a block of object bytecode *)

(* Must use the unsafe String.set here because the block may be
   a "fake" string as returned by Meta.static_alloc. *)

let patch_int buff pos n =
  String.unsafe_set buff pos (Char.unsafe_chr n);
  String.unsafe_set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  String.unsafe_set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  String.unsafe_set buff (pos + 3) (Char.unsafe_chr (n asr 24))

let patch_object buff patchlist = 
  List.iter
    (function
        (Reloc_literal sc, pos) ->
          patch_int buff pos (slot_for_literal sc)
      | (Reloc_getglobal id, pos) ->
          patch_int buff pos (slot_for_getglobal id)
      | (Reloc_setglobal id, pos) ->
          patch_int buff pos (slot_for_setglobal id)
      | (Reloc_primitive name, pos) ->
          patch_int buff pos (num_of_prim name))
    patchlist

(* Translate structured constants *)

let rec transl_const = function
    Const_base(Const_int i) -> Obj.repr i
  | Const_base(Const_char c) -> Obj.repr c
  | Const_base(Const_string s) -> Obj.repr s
  | Const_base(Const_float f) -> Obj.repr(float_of_string f)
  | Const_pointer i -> Obj.repr i
  | Const_block(tag, fields) ->
      let block = Obj.new_block tag (List.length fields) in
      let pos = ref 0 in
      List.iter
        (fun c -> Obj.set_field block !pos (transl_const c); incr pos)
        fields;
      block
  | Const_float_array fields ->
      Obj.repr(Array.of_list(List.map (fun f -> float_of_string f) fields))

(* Build the initial table of globals *)

let initial_global_table () =
  let glob = Array.create !global_table.num_cnt (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := [];
  glob

(* Save the table of globals *)

let output_global_map oc =
  output_value oc !global_table

(* Functions for toplevel use *)

(* Update the in-core table of globals *)

let update_global_table () =
  let ng = !global_table.num_cnt in
  if ng > Array.length(Meta.global_data()) then Meta.realloc_global_data ng;
  let glob = Meta.global_data() in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := []

(* Initialize the linker for toplevel use *)

let init_toplevel () =
  (* Read back the known global symbols from the executable file *)
  let ic = open_in_bin Sys.argv.(0) in
  let pos_trailer =
    in_channel_length ic - 24 - String.length Config.exec_magic_number in
  seek_in ic pos_trailer;
  let path_size = input_binary_int ic in
  let code_size = input_binary_int ic in
  let prim_size = input_binary_int ic in
  let data_size = input_binary_int ic in
  let symbol_size = input_binary_int ic in
  let debug_size = input_binary_int ic in
  seek_in ic (pos_trailer - debug_size - symbol_size);
  global_table := (input_value ic : Ident.t numtable);
  close_in ic;
  (* Enter the known C primitives *)
  Array.iter set_prim_table
             (Meta.available_primitives())

(* Find the value of a global identifier *)

let get_global_position id = slot_for_getglobal id

let get_global_value id =
  (Meta.global_data()).(slot_for_getglobal id)
let assign_global_value id v =
  (Meta.global_data()).(slot_for_getglobal id) <- v

(* Save and restore the current state *)

type global_map = Ident.t numtable

let current_state () = !global_table

let restore_state st = global_table := st

let hide_additions st =
  if st.num_cnt > !global_table.num_cnt then
    fatal_error "Symtable.hide_additions";
  global_table :=
    { num_cnt = !global_table.num_cnt;
      num_tbl = st.num_tbl }

(* "Filter" the global map according to some predicate.
   Used to expunge the global map for the toplevel. *)

let filter_global_map p gmap =
  let newtbl = ref Tbl.empty in
  Tbl.iter
    (fun id num -> if p id then newtbl := Tbl.add id num !newtbl)
    gmap.num_tbl;
  {num_cnt = gmap.num_cnt; num_tbl = !newtbl}

(* Error report *)

open Format

let report_error = function
    Undefined_global s ->
      print_string "Reference to undefined global `"; print_string s;
      print_string "'"
  | Unavailable_primitive s ->
      print_string "The external function `"; print_string s;
      print_string "' is not available"
  | Wrong_vm s ->
      print_string "Cannot find or execute the runtime system ";
      print_string s
