(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Local_store

module SMap = Misc.Stdlib.String.Map

(* Mapping from basenames to full filenames *)
type registry = string SMap.t ref

let files : registry = s_ref SMap.empty
let files_uncap : registry = s_ref SMap.empty

module Dir = struct
  type t = {
    path : string;
    files : string list;
  }

  let path t = t.path
  let files t = t.files

  (* For backward compatibility reason, simulate the behavior of
     [Misc.find_in_path]: silently ignore directories that don't exist
     + treat [""] as the current directory. *)
  let readdir_compat dir =
    try
      Sys.readdir (if dir = "" then Filename.current_dir_name else dir)
    with Sys_error _ ->
      [||]

  let create path =
    { path; files = Array.to_list (readdir_compat path) }
end

let dirs = s_ref []

let reset () =
  assert (not Config.merlin || Local_store.is_bound ());
  files := SMap.empty;
  files_uncap := SMap.empty;
  dirs := []

let get () = List.rev !dirs
let get_paths () = List.rev_map Dir.path !dirs

let add_to_maps fn basenames files files_uncap =
  List.fold_left (fun (files, files_uncap) base ->
      let fn = fn base in
      SMap.add base fn files,
      SMap.add (String.uncapitalize_ascii base) fn files_uncap
    ) (files, files_uncap) basenames

(* Optimized version of [add] below, for use in [init] and [remove_dir]: since
   we are starting from an empty cache, we can avoid checking whether a unit
   name already exists in the cache simply by adding entries in reverse
   order. *)
let add dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path)
      dir.Dir.files !files !files_uncap
  in
  files := new_files;
  files_uncap := new_files_uncap

let init l =
  reset ();
  dirs := List.rev_map Dir.create l;
  List.iter add !dirs

let remove_dir dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_dirs = List.filter (fun d -> Dir.path d <> dir) !dirs in
  if List.compare_lengths new_dirs !dirs <> 0 then begin
    reset ();
    List.iter add new_dirs;
    dirs := new_dirs
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present in the cache, in
   order to enforce left-to-right precedence. *)
let add dir =
  assert (not Config.merlin || Local_store.is_bound ());
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path) dir.Dir.files
      SMap.empty SMap.empty
  in
  let first _ fn _ = Some fn in
  files := SMap.union first !files new_files;
  files_uncap := SMap.union first !files_uncap new_files_uncap;
  dirs := dir :: !dirs

let add_dir dir = add (Dir.create dir)

let is_basename fn = Filename.basename fn = fn

let find fn =
  assert (not Config.merlin || Local_store.is_bound ());
  if is_basename fn then
    SMap.find fn !files
  else
    Misc.find_in_path (get_paths ()) fn

let find_uncap fn =
  assert (not Config.merlin || Local_store.is_bound ());
  if is_basename fn then
    SMap.find (String.uncapitalize_ascii fn) !files_uncap
  else
    Misc.find_in_path_uncap (get_paths ()) fn
