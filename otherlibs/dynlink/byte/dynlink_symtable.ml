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

(* To assign numbers to globals and primitives *)

open Dynlink_cmo_format
module Config = Dynlink_config

module Style = struct
  let inline_code = Format.pp_print_string
end

#25 "bytecomp/symtable.ml"
module Compunit = struct
  type t = compunit
  let name (Compunit cu_name) = cu_name
  let is_packed (Compunit name) = String.contains name '.'
#32 "bytecomp/symtable.ml"
end
#42 "bytecomp/symtable.ml"
module Global = struct
  type t =
    | Glob_compunit of compunit
    | Glob_predef of predef

  let name = function
    | Glob_compunit (Compunit cu) -> cu
    | Glob_predef (Predef_exn exn) -> exn

  let quote s = "`" ^ s ^ "'"

  let description ppf g =
#46 "otherlibs/dynlink/byte/dynlink_symtable.ml"
    let open Format in
#55 "bytecomp/symtable.ml"
    match g with
    | Glob_compunit (Compunit cu) ->
        fprintf ppf "compilation unit %a"
          Style.inline_code (quote cu)
    | Glob_predef (Predef_exn exn) ->
        fprintf ppf "predefined exception %a"
          Style.inline_code (quote exn)
#72 "bytecomp/symtable.ml"
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end
#77 "bytecomp/symtable.ml"
type error =
    Undefined_global of Global.t
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of Global.t

exception Error of error
#67 "otherlibs/dynlink/byte/dynlink_symtable.ml"
module Dll = struct
#18 "bytecomp/dll.ml"
type dll_handle
type dll_address
#22 "bytecomp/dll.ml"
external dll_open: string -> dll_handle = "caml_dynlink_open_lib"
#24 "bytecomp/dll.ml"
external dll_sym: dll_handle -> string -> dll_address
                = "caml_dynlink_lookup_symbol"
         (* returned dll_address may be Val_unit *)
external add_primitive: dll_address -> int = "caml_dynlink_add_primitive"
external get_current_dlls: unit -> dll_handle array
                                           = "caml_dynlink_get_current_libs"

(* Current search path for DLLs *)
let search_path = ref ([] : string list)
#42 "bytecomp/dll.ml"
(* DLLs currently opened *)
#86 "otherlibs/dynlink/byte/dynlink_symtable.ml"
let opened_dlls = ref ([] : (string * dll_handle) list)
(* Each known primitive and its ID number *)
let primitives : (string, int) Hashtbl.t = Hashtbl.create 100
#52 "bytecomp/dll.ml"
(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)

let extract_dll_name file =
  if Filename.check_suffix file Config.ext_dll then
    Filename.chop_suffix file Config.ext_dll
  else if String.length file >= 2 && String.sub file 0 2 = "-l" then
    "dll" ^ String.sub file 2 (String.length file - 2)
  else
    file (* will cause error later *)
#100 "otherlibs/dynlink/byte/dynlink_symtable.ml"
(* Specialized version of [Dll.{open_dll,open_dlls,find_primitive}] for the
    execution mode. *)
let open_dll name =
  let name = (extract_dll_name name) ^ Config.ext_dll in
  let fullname =
    if Filename.is_implicit name then
      !search_path
      |> List.find_map (fun dir ->
        let fullname = Filename.concat dir name in
        let fullname =
          if Filename.is_implicit fullname then
            Filename.concat Filename.current_dir_name fullname
          else fullname
        in
        if Sys.file_exists fullname then Some fullname else None)
      |> Option.value ~default:name
    else
      name
  in
  match List.assoc_opt fullname !opened_dlls with
  | Some _ -> ()
  | None ->
      begin match dll_open fullname with
      | dll ->
          opened_dlls := (fullname, dll) :: !opened_dlls
      | exception Failure msg ->
          failwith (fullname ^ ": " ^ msg)
      end

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)

let open_dlls names =
  List.iter open_dll names

let find_primitive prim_name =
  try Hashtbl.find primitives prim_name
  with Not_found ->
    let rec find seen = function
      [] ->
        raise (Error (Unavailable_primitive prim_name))
    | (_, dll) as curr :: rem ->
        let addr = dll_sym dll prim_name in
        if addr == Obj.magic () then find (curr :: seen) rem else begin
          if seen <> [] then opened_dlls := curr :: List.rev_append seen rem;
          let n = add_primitive addr in
          assert (n = Hashtbl.length primitives);
          Hashtbl.add primitives prim_name n;
          n
        end
    in
    find [] !opened_dlls
(* Adapted from Dll.init_toplevel *)
let init ~dllpaths ~prims =
  search_path := dllpaths;
  opened_dlls :=
    List.map (fun dll -> "", dll)
      (Array.to_list (get_current_dlls ()));
  List.iteri (fun n p -> Hashtbl.add primitives p n) prims
end
let of_prim = Dll.find_primitive
let open_dlls = Dll.open_dlls
(* Adapted from "bytecomp/symtable.ml"*)
module GlobalMap = struct

  type t = {
    cnt: int; (* The next number *)
    tbl: int Global.Map.t ; (* The table of already numbered objects *)
  }

  let empty = { cnt = 0; tbl = Global.Map.empty }

  let find nt key =
    Global.Map.find key nt.tbl

  let enter nt key =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = Global.Map.add key n !nt.tbl };
    n

  let incr nt =
    let n = !nt.cnt in
    nt := { cnt = n + 1; tbl = !nt.tbl };
    n

end
#111 "bytecomp/symtable.ml"
(* Global variables *)

let global_table = ref GlobalMap.empty
and literal_table = ref([] : (int * Obj.t) list)
#119 "bytecomp/symtable.ml"
let slot_for_getglobal global =
  try
    GlobalMap.find !global_table global
  with Not_found ->
    raise(Error (Undefined_global global))

let slot_for_setglobal global =
  GlobalMap.enter global_table global

let slot_for_literal cst =
  let n = GlobalMap.incr global_table in
  literal_table := (n, cst) :: !literal_table;
  n
#283 "bytecomp/symtable.ml"
(* Relocate a block of object bytecode *)

let patch_int buff pos n =
  let open Bigarray.Array1 in
  set buff pos (Char.unsafe_chr n);
  set buff (pos + 1) (Char.unsafe_chr (n asr 8));
  set buff (pos + 2) (Char.unsafe_chr (n asr 16));
  set buff (pos + 3) (Char.unsafe_chr (n asr 24))

let patch_object buff patchlist =
  List.iter
    (function
        (Reloc_literal sc, pos) ->
          patch_int buff pos (slot_for_literal sc)
      | (Reloc_getcompunit cu, pos) ->
          let global = Global.Glob_compunit cu in
          patch_int buff pos (slot_for_getglobal global)
      | (Reloc_getpredef pd, pos) ->
          let global = Global.Glob_predef pd in
          patch_int buff pos (slot_for_getglobal global)
      | (Reloc_setcompunit cu, pos) ->
          let global = Global.Glob_compunit cu in
          patch_int buff pos (slot_for_setglobal global)
      | (Reloc_primitive name, pos) ->
          patch_int buff pos (of_prim name))
    patchlist
#328 "bytecomp/symtable.ml"
(* Functions for toplevel use *)

(* Update the in-core table of globals *)
#237 "otherlibs/dynlink/byte/dynlink_symtable.ml"
module Meta = struct
#16 "bytecomp/meta.ml"
external global_data : unit -> Obj.t array = "caml_get_global_data"
external realloc_global_data : int -> unit = "caml_realloc_global"
#242 "otherlibs/dynlink/byte/dynlink_symtable.ml"
end
#332 "bytecomp/symtable.ml"
let update_global_table () =
  let ng = !global_table.cnt in
  if ng > Array.length(Meta.global_data()) then Meta.realloc_global_data ng;
  let glob = Meta.global_data() in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- cst)
    !literal_table;
  literal_table := []

type bytecode_sections =
  { symb: GlobalMap.t;
    crcs: (string * Digest.t option) list;
    prim: string list;
    dlpt: string list }

external get_bytecode_sections : unit -> bytecode_sections =
  "caml_dynlink_get_bytecode_sections"

(* Initialize the linker for toplevel use *)

let init_toplevel () =
  let sect = get_bytecode_sections () in
  global_table := sect.symb;
#268 "otherlibs/dynlink/byte/dynlink_symtable.ml"
  Dll.init ~dllpaths:sect.dlpt ~prims:sect.prim;
#358 "bytecomp/symtable.ml"
  sect.crcs

(* Find the value of a global identifier *)
#364 "bytecomp/symtable.ml"
let get_global_value global =
  (Meta.global_data()).(slot_for_getglobal global)
#369 "bytecomp/symtable.ml"
(* Check that all compilation units referenced in the given patch list
   have already been initialized *)

let initialized_compunits patchlist =
  List.fold_left (fun compunits rel ->
      match fst rel with
      | Reloc_setcompunit compunit -> compunit :: compunits
      | Reloc_literal _ | Reloc_getcompunit _ | Reloc_getpredef _
      | Reloc_primitive _ -> compunits)
    []
    patchlist

let required_compunits patchlist =
  List.fold_left (fun compunits rel ->
      match fst rel with
      | Reloc_getcompunit compunit -> compunit :: compunits
      | Reloc_literal _ | Reloc_getpredef _ | Reloc_setcompunit _
      | Reloc_primitive _ -> compunits)
    []
    patchlist

let check_global_initialized patchlist =
  (* First determine the compilation units we will define *)
  let initialized_compunits = initialized_compunits patchlist in
  (* Then check that all referenced, not defined comp units have a value *)
  let check_reference (rel, _) = match rel with
      Reloc_getcompunit compunit ->
        let global = Global.Glob_compunit compunit in
        if not (List.mem compunit initialized_compunits)
        && Obj.is_int (get_global_value global)
        then raise (Error(Uninitialized_global global))
    | Reloc_literal _ | Reloc_getpredef _ | Reloc_setcompunit _
    | Reloc_primitive _ -> () in
  List.iter check_reference patchlist

(* Save and restore the current state *)

type global_map = GlobalMap.t

let current_state () = !global_table
#412 "bytecomp/symtable.ml"
let hide_additions (st : global_map) =
  if st.cnt > !global_table.cnt then
#321 "otherlibs/dynlink/byte/dynlink_symtable.ml"
    failwith "Symtable.hide_additions";
#415 "bytecomp/symtable.ml"
  global_table :=
    {GlobalMap.
      cnt = !global_table.cnt;
      tbl = st.tbl }
#434 "bytecomp/symtable.ml"
let is_defined_in_global_map (gmap : global_map) global =
  Global.Map.mem global gmap.tbl

let empty_global_map = GlobalMap.empty
