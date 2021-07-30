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

[@@@ocaml.warning "-40"]

(* To assign numbers to globals and primitives *)

open Misc
open Asttypes
open Lambda
open Cmo_format

module String = Misc.Stdlib.String

(* Functions for batch linking *)

type error =
    Undefined_global of string
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of string

exception Error of error

module Num_tbl (M : Map.S) = struct

  type t = {
    cnt: int; (* The next number *)
    tbl: int M.t ; (* The table of already numbered objects *)
  }

  let empty = { cnt = 0; tbl = M.empty }

  let find nt key =
    M.find key nt.tbl

  let enter nt key =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = M.add key n !nt.tbl };
    n

  let incr nt =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = !nt.tbl };
    n

end
module GlobalMap = Num_tbl(Ident.Map)
module PrimMap = Num_tbl(Misc.Stdlib.String.Map)

(* Global variables *)

let global_table = ref GlobalMap.empty
and literal_table = ref([] : (int * structured_constant) list)

let is_global_defined id =
  Ident.Map.mem id (!global_table).tbl

let slot_for_getglobal id =
  try
    GlobalMap.find !global_table id
  with Not_found ->
    raise(Error(Undefined_global(Ident.name id)))

let slot_for_setglobal id =
  GlobalMap.enter global_table id

let slot_for_literal cst =
  let n = GlobalMap.incr global_table in
  literal_table := (n, cst) :: !literal_table;
  n

(* The C primitives *)

let c_prim_table = ref PrimMap.empty

let set_prim_table name =
  ignore(PrimMap.enter c_prim_table name)

let of_prim name =
  try
    PrimMap.find !c_prim_table name
  with Not_found ->
    if !Clflags.custom_runtime || Config.host <> Config.target
       || !Clflags.no_check_prims
    then
      PrimMap.enter c_prim_table name
    else begin
      match Dll.find_primitive name with
      | None -> raise(Error(Unavailable_primitive name))
      | Some Prim_exists ->
          PrimMap.enter c_prim_table name
      | Some (Prim_loaded symb) ->
          let num = PrimMap.enter c_prim_table name in
          Dll.synchronize_primitive num symb;
          num
    end

let require_primitive name =
  if name.[0] <> '%' then ignore(of_prim name)

let all_primitives () =
  let prim = Array.make !c_prim_table.cnt "" in
  String.Map.iter (fun name number -> prim.(number) <- name) !c_prim_table.tbl;
  prim

let data_primitive_names () =
  let prim = all_primitives() in
  let b = Buffer.create 512 in
  for i = 0 to Array.length prim - 1 do
    Buffer.add_string b prim.(i); Buffer.add_char b '\000'
  done;
  Buffer.contents b

let output_primitive_names outchan =
  output_string outchan (data_primitive_names())

open Printf

let output_primitive_table outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "extern value %s();\n" prim.(i)
  done;
  fprintf outchan "typedef value (*primitive)();\n";
  fprintf outchan "primitive caml_builtin_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  %s,\n" prim.(i)
  done;
  fprintf outchan "  (primitive) 0 };\n";
  fprintf outchan "const char * caml_names_of_builtin_cprim[] = {\n";
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  \"%s\",\n" prim.(i)
  done;
  fprintf outchan "  (char *) 0 };\n"

(* Initialization for batch linking *)

let init () =
  (* Enter the predefined exceptions *)
  Array.iteri
    (fun i name ->
      let id =
        try List.assoc name Predef.builtin_values
        with Not_found -> fatal_error "Symtable.init" in
      let c = slot_for_setglobal id in
      let cst = Const_block
          (Obj.object_tag,
           [Const_base(Const_string (name, Location.none,None));
            Const_base(Const_int (-i-1))
           ])
      in
      literal_table := (c, cst) :: !literal_table)
    Runtimedef.builtin_exceptions;
  (* Initialize the known C primitives *)
  let set_prim_table_from_file primfile =
    let ic = open_in primfile in
    Misc.try_finally
      ~always:(fun () -> close_in ic)
      (fun () ->
         try
           while true do
             set_prim_table (input_line ic)
           done
         with End_of_file -> ()
      )
  in
  if String.length !Clflags.use_prims > 0 then
    set_prim_table_from_file !Clflags.use_prims
  else if String.length !Clflags.use_runtime > 0 then begin
    let primfile = Filename.temp_file "camlprims" "" in
    Misc.try_finally
      ~always:(fun () -> remove_file primfile)
      (fun () ->
         if Sys.command(Printf.sprintf "%s -p > %s"
                          !Clflags.use_runtime primfile) <> 0
         then raise(Error(Wrong_vm !Clflags.use_runtime));
         set_prim_table_from_file primfile
      )
  end else begin
    Array.iter set_prim_table Runtimedef.builtin_primitives
  end

(* Relocate a block of object bytecode *)

let patch_int buff pos n =
  LongString.set buff pos (Char.unsafe_chr n);
  LongString.set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  LongString.set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  LongString.set buff (pos + 3) (Char.unsafe_chr (n asr 24))

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
          patch_int buff pos (of_prim name))
    patchlist

(* Translate structured constants *)

let rec transl_const = function
    Const_base(Const_int i) -> Obj.repr i
  | Const_base(Const_char c) -> Obj.repr c
  | Const_base(Const_string (s, _, _)) -> Obj.repr s
  | Const_base(Const_float f) -> Obj.repr (float_of_string f)
  | Const_base(Const_int32 i) -> Obj.repr i
  | Const_base(Const_int64 i) -> Obj.repr i
  | Const_base(Const_nativeint i) -> Obj.repr i
  | Const_immstring s -> Obj.repr s
  | Const_block(tag, fields) ->
      let block = Obj.new_block tag (List.length fields) in
      let pos = ref 0 in
      List.iter
        (fun c -> Obj.set_field block !pos (transl_const c); incr pos)
        fields;
      block
  | Const_float_array fields ->
      let res = Array.Floatarray.create (List.length fields) in
      List.iteri (fun i f -> Array.Floatarray.set res i (float_of_string f))
        fields;
      Obj.repr res

(* Build the initial table of globals *)

let initial_global_table () =
  let glob = Array.make !global_table.cnt (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := [];
  glob

(* Save the table of globals *)

let output_global_map oc =
  output_value oc !global_table

let data_global_map () =
  Obj.repr !global_table

(* Functions for toplevel use *)

(* Update the in-core table of globals *)

let update_global_table () =
  let ng = !global_table.cnt in
  if ng > Array.length(Meta.global_data()) then Meta.realloc_global_data ng;
  let glob = Meta.global_data() in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    !literal_table;
  literal_table := []

(* Recover data for toplevel initialization.  Data can come either from
   executable file (normal case) or from linked-in data (-output-obj). *)

type section_reader = {
  read_string: string -> string;
  read_struct: string -> Obj.t;
  close_reader: unit -> unit
}

let read_sections () =
  try
    let sections = Meta.get_section_table () in
    { read_string =
        (fun name -> (Obj.magic(List.assoc name sections) : string));
      read_struct =
        (fun name -> List.assoc name sections);
      close_reader =
        (fun () -> ()) }
  with Not_found ->
    let ic = open_in_bin Sys.executable_name in
    Bytesections.read_toc ic;
    { read_string = Bytesections.read_section_string ic;
      read_struct = Bytesections.read_section_struct ic;
      close_reader = fun () -> close_in ic }

(* Initialize the linker for toplevel use *)

let init_toplevel () =
  try
    let sect = read_sections () in
    (* Locations of globals *)
    global_table := (Obj.magic (sect.read_struct "SYMB") : GlobalMap.t);
    (* Primitives *)
    let prims = sect.read_string "PRIM" in
    c_prim_table := PrimMap.empty;
    let pos = ref 0 in
    while !pos < String.length prims do
      let i = String.index_from prims !pos '\000' in
      set_prim_table (String.sub prims !pos (i - !pos));
      pos := i + 1
    done;
    (* DLL initialization *)
    let dllpath = try sect.read_string "DLPT" with Not_found -> "" in
    Dll.init_toplevel dllpath;
    (* Recover CRC infos for interfaces *)
    let crcintfs =
      try
        (Obj.magic (sect.read_struct "CRCS") : (string * Digest.t option) list)
      with Not_found -> [] in
    (* Done *)
    sect.close_reader();
    crcintfs
  with Bytesections.Bad_magic_number | Not_found | Failure _ ->
    fatal_error "Toplevel bytecode executable is corrupted"

(* Find the value of a global identifier *)

let get_global_position id = slot_for_getglobal id

let get_global_value id =
  (Meta.global_data()).(slot_for_getglobal id)
let assign_global_value id v =
  (Meta.global_data()).(slot_for_getglobal id) <- v

(* Check that all globals referenced in the given patch list
   have been initialized already *)

let defined_globals patchlist =
  List.fold_left (fun accu rel ->
      match rel with
      | (Reloc_setglobal id, _pos) -> id :: accu
      | _ -> accu)
    []
    patchlist

let required_globals patchlist =
  List.fold_left (fun accu rel ->
      match rel with
      | (Reloc_getglobal id, _pos) -> id :: accu
      | _ -> accu)
    []
    patchlist

let check_global_initialized patchlist =
  (* First determine the globals we will define *)
  let defined_globals = defined_globals patchlist in
  (* Then check that all referenced, not defined globals have a value *)
  let check_reference = function
      (Reloc_getglobal id, _pos) ->
        if not (List.mem id defined_globals)
        && Obj.is_int (get_global_value id)
        then raise (Error(Uninitialized_global(Ident.name id)))
    | _ -> () in
  List.iter check_reference patchlist

(* Save and restore the current state *)

type global_map = GlobalMap.t

let current_state () = !global_table

let restore_state st = global_table := st

let hide_additions (st : global_map) =
  if st.cnt > !global_table.cnt then
    fatal_error "Symtable.hide_additions";
  global_table :=
    {GlobalMap.
      cnt = !global_table.cnt;
      tbl = st.tbl }

(* "Filter" the global map according to some predicate.
   Used to expunge the global map for the toplevel. *)

let filter_global_map p (gmap : global_map) =
  let newtbl = ref Ident.Map.empty in
  Ident.Map.iter
    (fun id num -> if p id then newtbl := Ident.Map.add id num !newtbl)
    gmap.tbl;
  {GlobalMap. cnt = gmap.cnt; tbl = !newtbl}

let iter_global_map f (gmap : global_map) =
  Ident.Map.iter f gmap.tbl

let is_defined_in_global_map (gmap : global_map) id =
  Ident.Map.mem id gmap.tbl

let empty_global_map = GlobalMap.empty

(* Error report *)

open Format

let report_error ppf = function
  | Undefined_global s ->
      fprintf ppf "Reference to undefined global `%s'" s
  | Unavailable_primitive s ->
      fprintf ppf "The external function `%s' is not available" s
  | Wrong_vm s ->
      fprintf ppf "Cannot find or execute the runtime system %s" s
  | Uninitialized_global s ->
      fprintf ppf "The value of the global `%s' is not yet computed" s

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )

let reset () =
  global_table := GlobalMap.empty;
  literal_table := [];
  c_prim_table := PrimMap.empty
