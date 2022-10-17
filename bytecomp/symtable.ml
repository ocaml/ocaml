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

module Num_tbl (M : Map.S) : sig
  type t
  val empty : t
  val find : t -> M.key -> int
  val mem : t -> M.key -> bool
  val filter : (M.key -> bool) -> t -> t
  val iter : (M.key -> int -> unit) -> t -> unit
  val enter : t ref -> M.key -> int
  val incr : t ref -> int
  val length : t -> int
  val extend : t -> int -> t option
end = struct

  type t = {
    cnt: int; (* The next number *)
    tbl: int M.t ; (* The table of already numbered objects *)
  }

  let empty = { cnt = 0; tbl = M.empty }

  let find nt key =
    M.find key nt.tbl

  let mem nt key = M.mem key nt.tbl

  let enter nt key =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = M.add key n !nt.tbl };
    n

  let incr nt =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = !nt.tbl };
    n

  let filter p nt =
    let newtbl = ref M.empty in
    M.iter
      (fun id num -> if p id then newtbl := M.add id num !newtbl)
      nt.tbl;
    {cnt = nt.cnt; tbl = !newtbl}

  let iter f nt =
    M.iter f nt.tbl

  let length nt = nt.cnt

  let extend nt size =
    if size < nt.cnt
    then None
    else Some { nt with cnt = size }

end
module GlobalMap = Num_tbl(Ident.Map)
module PrimMap = Num_tbl(Misc.Stdlib.String.Map)

type state = {
  global_table : GlobalMap.t ref;
  mutable literal_table : (int * structured_constant) list;
  c_prim_table : PrimMap.t ref;
}

let create_empty () = {
  global_table = ref GlobalMap.empty;
  literal_table = [];
  c_prim_table = ref PrimMap.empty;
}

let get_globalmap t = !(t.global_table)

let set_globalmap t table = t.global_table := table

let is_global_defined t id =
  GlobalMap.mem !(t.global_table) id

let slot_for_getglobal t id =
  try
    GlobalMap.find !(t.global_table) id
  with Not_found ->
    raise(Error(Undefined_global(Ident.name id)))

let slot_for_setglobal t id =
  GlobalMap.enter t.global_table id

let slot_for_literal t cst =
  let n = GlobalMap.incr t.global_table in
  t.literal_table <- (n, cst) :: t.literal_table;
  n


let get_global_position state id =
  slot_for_getglobal state id

(* The C primitives *)

let set_prim_table t name =
  ignore(PrimMap.enter t.c_prim_table name)

let of_prim t name =
  try
    PrimMap.find !(t.c_prim_table) name
  with Not_found ->
    if !Clflags.custom_runtime || Config.host <> Config.target
       || !Clflags.no_check_prims
    then
      PrimMap.enter t.c_prim_table name
    else begin
      match Dll.find_primitive name with
      | Prim_not_found -> raise(Error(Unavailable_primitive name))
      | Prim_exists ->
          PrimMap.enter t.c_prim_table name
      | Prim_loaded (_symb, num) ->
          let num' = PrimMap.enter t.c_prim_table name in
          assert(num' = num);
          num
    end

let require_primitive t name =
  if name.[0] <> '%' then ignore(of_prim t name)

let all_primitives t =
  let a = Array.make (PrimMap.length !(t.c_prim_table)) "" in
  PrimMap.iter (fun key number -> a.(number) <- key) !(t.c_prim_table);
  Array.to_list a

(* Initialization for batch linking *)

let create_for_linker () =
  let t = create_empty () in
  (* Enter the predefined values *)
  Array.iteri
    (fun i name ->
      let id =
        try List.assoc name Predef.builtin_values
        with Not_found -> fatal_error "Symtable.init" in
      let c = slot_for_setglobal t id in
      let cst = Const_block
          (Obj.object_tag,
           [Const_base(Const_string (name, Location.none,None));
            Const_base(Const_int (-i-1))
           ])
      in
      t.literal_table <- (c, cst) :: t.literal_table)
    Runtimedef.builtin_exceptions;
  (* Initialize the known C primitives *)
  let set_prim_table_from_file primfile =
    let ic = open_in primfile in
    Misc.try_finally
      ~always:(fun () -> close_in ic)
      (fun () ->
         try
           while true do
             set_prim_table t (input_line ic)
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
         let cmd =
           Filename.quote_command
             !Clflags.use_runtime
             ~stdout:primfile
             ["-p"]
         in
         if Sys.command cmd <> 0
         then raise(Error(Wrong_vm !Clflags.use_runtime));
         set_prim_table_from_file primfile
      )
  end else begin
    Array.iter (set_prim_table t) Runtimedef.builtin_primitives
  end;
  t

(* Relocate a block of object bytecode *)

let patch_int buff pos n =
  LongString.set buff pos (Char.unsafe_chr n);
  LongString.set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  LongString.set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  LongString.set buff (pos + 3) (Char.unsafe_chr (n asr 24))

let patch_object t buff patchlist =
  List.iter
    (function
        (Reloc_literal sc, pos) ->
          patch_int buff pos (slot_for_literal t sc)
      | (Reloc_getglobal id, pos) ->
          patch_int buff pos (slot_for_getglobal t id)
      | (Reloc_setglobal id, pos) ->
          patch_int buff pos (slot_for_setglobal t id)
      | (Reloc_primitive name, pos) ->
          patch_int buff pos (of_prim t name))
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

let initial_global_table t =
  let glob = Array.make (GlobalMap.length !(t.global_table)) (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- transl_const cst)
    t.literal_table;
  glob

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


module Toplevel = struct
  (* For toplevel and dynlink use *)

  type status =
    | Uninitialized
    | Initializing
    | Initialized of state

  let the_one = ref Uninitialized

  let get_state () =
    match !the_one with
    | Uninitialized
    | Initializing -> failwith "Symtable.Toplevel.get_state: not initialized"
    | Initialized state -> state

  let get_globalmap () =
    let state = get_state () in
    get_globalmap state

  let restore_globalmap gmap =
    let state = get_state () in
    set_globalmap state gmap

  (* Recover data for toplevel initialization.  Data can come either from
     executable file (normal case) or from linked-in data (-output-obj). *)

  type section_reader = {
    read_string: Bytesections.Name.t -> string;
    read_struct: Bytesections.Name.t -> Obj.t;
    close_reader: unit -> unit
  }

  let read_sections () =
    try
      let sections = Meta.get_section_table () in
      { read_string =
          (fun name -> (Obj.magic(List.assoc (name :> string) sections) : string));
        read_struct =
          (fun name -> List.assoc (name :> string) sections);
        close_reader =
          (fun () -> ()) }
    with Not_found ->
      let ic = open_in_bin Sys.executable_name in
      let section_table = Bytesections.read_toc ic in
      { read_string = Bytesections.read_section_string section_table ic;
        read_struct = Bytesections.read_section_struct section_table ic;
        close_reader = fun () -> close_in ic }

  (* Initialize the linker for toplevel use *)

  let init () =
    match !the_one with
    | Initialized _ | Initializing -> failwith "Symtable.Toplevel.init: already initialized"
    | Uninitialized ->
        the_one := Initializing;
        let state = create_empty () in
        try
          let sect = read_sections () in
          (* Locations of globals *)
          state.global_table := (Obj.magic (sect.read_struct Bytesections.Name.symb) : GlobalMap.t);
          (* Primitives *)
          let prims = split_null_separated (sect.read_string Bytesections.Name.prim) in
          List.iter (set_prim_table state) prims;
          (* DLL initialization *)
          let dllpath = try sect.read_string Bytesections.Name.dlpt with Not_found -> "" in
          Dll.init_toplevel dllpath;
          (* Recover CRC infos for interfaces *)
          let crcintfs =
            try
              (Obj.magic (sect.read_struct Bytesections.Name.crcs) : (string * Digest.t option) list)
            with Not_found -> [] in
          (* Done *)
          sect.close_reader();
          the_one := Initialized state;
          crcintfs
        with Bytesections.Bad_magic_number | Not_found | Failure _ ->
          the_one := Uninitialized;
          fatal_error "Toplevel bytecode executable is corrupted"

  (* Find the value of a global identifier *)

  let get_global_value id =
    let state = get_state () in
    (Meta.global_data()).(slot_for_getglobal state id)

  let is_global_defined id =
    let state = get_state () in
    is_global_defined state id

  let assign_global_value id v =
    let state = get_state () in
    (Meta.global_data()).(slot_for_getglobal state id) <- v

  let hide_additions (st : GlobalMap.t) =
    let state = get_state () in
    let len = GlobalMap.length !(state.global_table) in
    match GlobalMap.extend st len with
    | None -> fatal_error "Symtable.hide_additions"
    | Some st -> state.global_table := st

  (* Check that all globals referenced in the given patch list
     have been initialized already *)

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

  (* Update the in-core table of globals *)

  let update_global_table state =
    let ng = GlobalMap.length !(state.global_table) in
    let glob =
      let glob = Meta.global_data() in
      if ng > Array.length(Meta.global_data())
      then (Meta.realloc_global_data ng;Meta.global_data())
      else glob
    in
    List.iter
      (fun (slot, cst) -> glob.(slot) <- transl_const cst)
      state.literal_table;
    state.literal_table <- []

  let patch_code_and_update_global_table buff patchlist =
    let state = get_state () in
    patch_object state buff patchlist;
    check_global_initialized patchlist;
    update_global_table state

end

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
