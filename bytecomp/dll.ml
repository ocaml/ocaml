(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Handling of dynamically-linked libraries *)

type dll_handle
type dll_address

external dll_open: string -> dll_handle = "dynlink_open_lib"
external dll_close: dll_handle -> unit = "dynlink_close_lib"
external dll_sym: dll_handle -> string -> dll_address = "dynlink_lookup_symbol"
external add_primitive: dll_address -> int = "dynlink_add_primitive"
external get_current_dlls: unit -> dll_handle array
                                           = "dynlink_get_current_libs"

(* Current search path for DLLs *)
let search_path = ref ([] : string list)

(* DLLs currently opened *)
let opened_dlls = ref ([] : dll_handle list)

(* File names for those DLLs *)
let names_of_opened_dlls = ref ([] : string list)

(* Add the given directories to the search path for DLLs. *)
let add_path dirs =
  search_path := dirs @ !search_path

(* Extract names of DLLs from a list of C object files and libraries *)

let extract_dll_names files =
  List.fold_right
    (fun file res ->
      if Filename.check_suffix file Config.ext_dll then
        Filename.chop_suffix file Config.ext_dll :: res
      else if String.length file >= 2 && String.sub file 0 2 = "-l" then
        ("lib" ^ String.sub file 2 (String.length file - 2)) :: res
      else
        res)
    files []

(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)

let open_dll name =
  let name = name ^ Config.ext_dll in
  let fullname =
    try Misc.find_in_path !search_path name with Not_found -> name in
  if not (List.mem fullname !names_of_opened_dlls) then begin
    let dll = dll_open fullname in
    names_of_opened_dlls := fullname :: !names_of_opened_dlls;
    opened_dlls := dll :: !opened_dlls
  end

let open_dlls names =
  List.iter open_dll (List.rev names)

(* Close all DLLs *)

let close_all_dlls () =
  List.iter dll_close !opened_dlls;
  opened_dlls := [];
  names_of_opened_dlls := []

(* Find a primitive in the currently opened DLLs.
   Raise [Not_found] if not found. *)

let find_primitive prim_name =
  let rec find = function
    [] ->
      raise Not_found
  | dll :: rem ->
      try dll_sym dll prim_name with Failure _ -> find rem in
  find !opened_dlls

(* If linking in core (dynlink or toplevel), synchronize the VM
   table of primitive with the linker's table of primitive
   by storing the given primitive function at the given position
   in the VM table of primitives.  *)

let linking_in_core = ref false

let synchronize_primitive num symb =
  if !linking_in_core then begin
    let actual_num = add_primitive symb in
    assert (actual_num = num)
  end

(* Initialization for linking in core (dynlink or toplevel) *)

let init_toplevel dllpath =
  search_path :=
    Dllpath.ld_library_path_contents() @
    Dllpath.split_dll_path dllpath @
    Dllpath.ld_conf_contents();
  opened_dlls := Array.to_list (get_current_dlls());
  names_of_opened_dlls := [];
  linking_in_core := true

