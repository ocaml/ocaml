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

open Misc
open Lambda
open Cmo_format

module String = Misc.Stdlib.String
module Style = Misc.Style

module Compunit = struct
  type t = compunit
  let name (Compunit cu_name) = cu_name
  let is_packed (Compunit name) = String.contains name '.'
  let to_ident (Compunit cu_name) = Ident.create_persistent cu_name
  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

let builtin_values = Predef.builtin_values

module Predef = struct
  type t = predef
  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

module Global = struct
  type t =
    | Glob_compunit of compunit
    | Glob_predef of predef

  let name = function
    | Glob_compunit (Compunit cu) -> cu
    | Glob_predef (Predef_exn exn) -> exn

  let quote s = "`" ^ s ^ "'"

  let description ppf g =
    let open Format_doc in
    match g with
    | Glob_compunit (Compunit cu) ->
        fprintf ppf "compilation unit %a"
          Style.inline_code (quote cu)
    | Glob_predef (Predef_exn exn) ->
        fprintf ppf "predefined exception %a"
          Style.inline_code (quote exn)

  let of_ident id =
    let name = Ident.name id in
    if (Ident.is_predef id)
    then Some (Glob_predef (Predef_exn name))
    else if (Ident.global id)
    then Some (Glob_compunit (Compunit name))
    else None

  module Set = Set.Make(struct type nonrec t = t let compare = compare end)
  module Map = Map.Make(struct type nonrec t = t let compare = compare end)
end

(* Functions for batch linking *)

type error =
    Undefined_global of Global.t
  | Unavailable_primitive of string
  | Wrong_vm of string
  | Uninitialized_global of Global.t

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
module GlobalMap = Num_tbl(Global.Map)
module PrimMap = Num_tbl(Misc.Stdlib.String.Map)

(* Global variables *)

let global_table = ref GlobalMap.empty
and literal_table = ref([] : (int * Obj.t) list)

let is_global_defined global =
  Global.Map.mem global (!global_table).tbl

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
  all_primitives()
  |> Array.to_list

let output_primitive_names outchan =
  output_string outchan (data_primitive_names() |> concat_null_terminated)

open Printf

let output_primitive_table outchan =
  let prim = all_primitives() in
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "extern value %s(void);\n" prim.(i)
  done;
  fprintf outchan {|
typedef value (*c_primitive)(void);

#if defined __cplusplus
extern
#endif
const c_primitive caml_builtin_cprim[] = {
|};
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  %s,\n" prim.(i)
  done;
  fprintf outchan
{|  0 };

#if defined __cplusplus
extern
#endif
const char * const caml_names_of_builtin_cprim[] = {
|};
  for i = 0 to Array.length prim - 1 do
    fprintf outchan "  \"%s\",\n" prim.(i)
  done;
  fprintf outchan "  0 };\n"

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
      let transl_field pos cst =
        Obj.set_field block pos (transl_const cst)
      in
      List.iteri transl_field fields;
      block
  | Const_float_array fields ->
      let res = Array.Floatarray.create (List.length fields) in
      List.iteri (fun i f -> Array.Floatarray.set res i (float_of_string f))
        fields;
      Obj.repr res

(* Initialization for batch linking *)

let init () =
  (* Enter the predefined exceptions *)
  Array.iteri
    (fun i name ->
        if not (List.mem_assoc name builtin_values)
        then fatal_error "Symtable.init";
      let global = Global.Glob_predef (Predef_exn name) in
      let c = slot_for_setglobal global in
      let cst = Const_block
          (Obj.object_tag,
           [Const_base(Const_string (name, Location.none,None));
            Const_base(Const_int (-i-1))
           ])
      in
      literal_table := (c, transl_const cst) :: !literal_table)
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
         let cmd =
           Filename.quote_command
             !Clflags.use_runtime
             ~stdout:primfile
             ["-p"]
         in
         if !Clflags.verbose then
           Printf.eprintf "+ %s\n%!" cmd;
         if Sys.command cmd <> 0
         then raise(Error(Wrong_vm !Clflags.use_runtime));
         set_prim_table_from_file primfile
      )
  end else begin
    Array.iter set_prim_table Runtimedef.builtin_primitives
  end

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

(* Build the initial table of globals *)

let initial_global_table () =
  let glob = Array.make !global_table.cnt (Obj.repr 0) in
  List.iter
    (fun (slot, cst) -> glob.(slot) <- cst)
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
  c_prim_table := PrimMap.empty;
  List.iter set_prim_table sect.prim;
  Dll.init_toplevel sect.dlpt;
  sect.crcs

(* Find the value of a global identifier *)

let get_global_position = slot_for_getglobal

let get_global_value global =
  (Meta.global_data()).(slot_for_getglobal global)
let assign_global_value global v =
  (Meta.global_data()).(slot_for_getglobal global) <- v

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
  let newtbl = ref Global.Map.empty in
  Global.Map.iter
    (fun global num ->
      if p global then newtbl := Global.Map.add global num !newtbl)
    gmap.tbl;
  {GlobalMap. cnt = gmap.cnt; tbl = !newtbl}

let iter_global_map f (gmap : global_map) =
  Global.Map.iter f gmap.tbl

let is_defined_in_global_map (gmap : global_map) global =
  Global.Map.mem global gmap.tbl

let empty_global_map = GlobalMap.empty

(* Error report *)

open Format_doc

let report_error_doc ppf = function
  | Undefined_global global ->
      fprintf ppf "Reference to undefined %a" Global.description global
  | Unavailable_primitive s ->
      fprintf ppf "The external function %a is not available"
        Style.inline_code s
  | Wrong_vm s ->
      fprintf ppf "Cannot find or execute the runtime system %a"
      Style.inline_code s
  | Uninitialized_global global ->
      fprintf ppf "The value of the %a is not yet computed"
        Global.description global

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error_doc err)
      | _ -> None
    )

let report_error = Format_doc.compat report_error_doc

let reset () =
  global_table := GlobalMap.empty;
  literal_table := [];
  c_prim_table := PrimMap.empty
