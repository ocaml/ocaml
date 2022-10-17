(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2001 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Split the CAML_LD_LIBRARY_PATH environment variable and return
   the corresponding list of directories.  *)
let ld_library_path_contents () =
  match Sys.getenv "CAML_LD_LIBRARY_PATH" with
  | exception Not_found ->
      []
  | s ->
      Misc.split_path_contents s

(* Read the [ld.conf] file and return the corresponding list of directories *)

let ld_conf_contents () =
  let path = ref [] in
  begin try
    let ic = open_in (Filename.concat Config.standard_library "ld.conf") in
    begin try
      while true do
        path := input_line ic :: !path
      done
    with End_of_file -> ()
    end;
    close_in ic
  with Sys_error _ -> ()
  end;
  List.rev !path


type state = {
  mutable search_path : string list; (* Current search path for DLLs *)
  mutable opened_dlls : (string * Binutils.t) list (* DLLs currently opened *)

}

(* Extract the name of a DLLs from its external name (xxx.so or -lxxx) *)
let extract_dll_name file =
  if Filename.check_suffix file Config.ext_dll then
    Filename.chop_suffix file Config.ext_dll
  else if String.length file >= 2 && String.sub file 0 2 = "-l" then
    "dll" ^ String.sub file 2 (String.length file - 2)
  else
    file (* will cause error later *)

(* Initialization for separate compilation *)

let init nostdlib = {
  opened_dlls = [];
  search_path =
    ld_library_path_contents() @
    (if nostdlib then [] else ld_conf_contents())
}

(* Add the given directories to the search path for DLLs. *)
let add_path t dirs =
  t.search_path <- dirs @ t.search_path

let remove_path t dirs =
  t.search_path <- List.filter (fun d -> not (List.mem d dirs)) t.search_path


(* Open a list of DLLs, adding them to opened_dlls.
   Raise [Failure msg] in case of error. *)
let open_dll t name =
  let name = name ^ Config.ext_dll in
  let fullname =
    try
      let fullname = Misc.find_in_path t.search_path name in
      if Filename.is_implicit fullname then
        Filename.concat Filename.current_dir_name fullname
      else fullname
    with Not_found -> name in
  if not (List.mem_assoc fullname t.opened_dlls)
  then
    begin match Binutils.read fullname with
    | Ok bin -> t.opened_dlls <- (fullname, bin) :: t.opened_dlls
    | Error err ->
        failwith (fullname ^ ": " ^ Binutils.error_to_string err)
    end

(* Find a primitive in the currently opened DLLs. *)

let primitive_exists t prim_name =
  List.exists (fun (_fname, bin) ->
      Binutils.defines_symbol bin prim_name) t.opened_dlls


module Toplevel = struct


  (* Handling of dynamically-linked libraries *)

  type dll_handle
  type dll_address

  external dll_open: string -> dll_handle = "caml_dynlink_open_lib"
  external dll_close: dll_handle -> unit = "caml_dynlink_close_lib"
  external dll_sym: dll_handle -> string -> dll_address
    = "caml_dynlink_lookup_symbol"
  (* returned dll_address may be Val_unit *)
  external add_primitive: dll_address -> int = "caml_dynlink_add_primitive"
  external get_current_dlls: unit -> dll_handle array
    = "caml_dynlink_get_current_libs"

  type state = {
    mutable search_path : string list; (* Current search path for DLLs *)
    mutable opened_dlls : (string * dll_handle) list (* DLLs currently opened *)
  }


  (* Initialization for linking in core (dynlink or toplevel) *)

  let state = { search_path = []; opened_dlls = []}

  let split_dll_path path =
    Misc.split_path_contents ~sep:'\000' path

  let close_all_dlls () =
    List.iter (fun (_, dll) -> dll_close dll) state.opened_dlls;
    state.opened_dlls <- []

  let init dllpath =
    state.search_path <-
      ld_library_path_contents() @
      split_dll_path dllpath @
      ld_conf_contents();
    state.opened_dlls <-
      List.map (fun dll -> "", dll)
        (Array.to_list (get_current_dlls()))

  (* Add the given directories to the search path for DLLs. *)
  let add_path dirs =
    state.search_path <- dirs @ state.search_path

  let remove_path dirs =
    state.search_path <- List.filter (fun d -> not (List.mem d dirs)) state.search_path

  let open_dll name =
    let name = name ^ Config.ext_dll in
    let fullname =
      try
        let fullname = Misc.find_in_path state.search_path name in
        if Filename.is_implicit fullname then
          Filename.concat Filename.current_dir_name fullname
        else fullname
      with Not_found -> name in
    match List.assoc_opt fullname state.opened_dlls with
    | None ->
        begin match dll_open fullname with
        | dll ->
            state.opened_dlls <- (fullname, dll) :: state.opened_dlls
        | exception Failure msg ->
            failwith (fullname ^ ": " ^ msg)
        end
    | Some dll ->
        let l = List.remove_assoc fullname state.opened_dlls in
        state.opened_dlls <- ((fullname, dll) :: l)

  (* Close all DLLs *)


  let find_primitive prim_name =
    match List.find_map (fun (fname,dll) ->
        let addr = dll_sym dll prim_name in
        if addr == Obj.magic ()
        then None
        else Some (fname, dll, addr)) state.opened_dlls with
    | None -> None
    | Some (fname, dll, addr) ->
        let l = List.remove_assoc fname state.opened_dlls in
        state.opened_dlls <- (fname, dll) :: l;
        let num = add_primitive addr in
        Some (addr, num)
          
end
