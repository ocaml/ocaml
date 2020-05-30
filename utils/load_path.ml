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

module SMap = Misc.Stdlib.String.Map
module RevList = Misc.RevList

(* Mapping from basenames to full filenames *)
type registry = string SMap.t ref

let files : registry = ref SMap.empty
let files_uncap : registry = ref SMap.empty

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

let dirs = ref RevList.empty

let reset () =
  files := SMap.empty;
  files_uncap := SMap.empty;
  dirs := RevList.empty

let get () = RevList.to_list !dirs
let get_paths () = RevList.to_list (RevList.map Dir.path !dirs)

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
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path)
      dir.Dir.files !files !files_uncap
  in
  files := new_files;
  files_uncap := new_files_uncap;
  dirs := RevList.snoc dir !dirs

let init l =
  reset ();
  let add_dir dir = add (Dir.create dir) in
  List.iter add_dir (List.rev l)

let remove_dir dir =
  let new_dirs = RevList.filter (fun d -> Dir.path d <> dir) !dirs in
  if RevList.compare_length new_dirs !dirs <> 0 then begin
    reset ();
    RevList.rev_iter add new_dirs
  end

(* General purpose version of function to add a new entry to load path: We only
   add a basename to the cache if it is not already present in the cache, in
   order to enforce left-to-right precedence. *)
let add dir =
  let new_files, new_files_uncap =
    add_to_maps (Filename.concat dir.Dir.path) dir.Dir.files
      SMap.empty SMap.empty
  in
  let first _ fn _ = Some fn in
  files := SMap.union first !files new_files;
  files_uncap := SMap.union first !files_uncap new_files_uncap;
  dirs := RevList.snoc dir !dirs

let add_dir dir = add (Dir.create dir)

let is_basename fn = Filename.basename fn = fn

let find fn =
  if is_basename fn then
    SMap.find fn !files
  else
    Misc.find_in_path (get_paths ()) fn

let find_uncap fn =
  if is_basename fn then
    SMap.find (String.uncapitalize_ascii fn) !files_uncap
  else
    Misc.find_in_path_uncap (get_paths ()) fn
