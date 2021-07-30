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


type path = Dir of string | File of string

let path_to_string (Dir s | File s) = s

type t = path list
(* Kept in reverse order *)

let empty = []

let of_dirs l = List.rev_map (fun dir -> Dir dir) l

let of_paths l = List.rev l

let add_dir t dir = Dir dir :: t

let add_file t file = File file :: t

let dirs t =
  List.rev (List.filter_map (function Dir dir -> Some dir | File _ -> None) t)

let paths t = List.rev t

let mem = List.mem

let concat ts = List.concat (List.rev ts)

let expand_directory s t =
  List.map (function
      | Dir dir -> Dir (Misc.expand_directory s dir)
      | File file -> File (Misc.expand_directory s file)
    ) t

let find fn t = Misc.find_in_path (dirs t) fn

let find_uncap fn t = Misc.find_in_path_uncap (dirs t) fn

let find_rel fn t = Misc.find_in_path_rel (dirs t) fn

module Cache = struct
  open Local_store

  module STbl = Misc.Stdlib.String.Tbl

  (* Mapping from basenames to full filenames *)
  type registry = string STbl.t

  let files : registry ref = s_table STbl.create 42
  let files_uncap : registry ref = s_table STbl.create 42

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

  module Path = struct
    type t =
      | Dir of Dir.t
      | File of string

    let create : path -> t = function
      | Dir dir -> Dir (Dir.create dir)
      | File file -> File file

    let dir dir = Dir (Dir.create dir)

    let file file = File file

    let path : t -> path = function
      | Dir dir -> Dir (Dir.path dir)
      | File file -> File file

    let files : t -> string list = function
      | Dir dir -> Dir.files dir
      | File file -> [Filename.basename file]
  end

  let paths : Path.t list ref = ref []

  let reset () =
    assert (not Config.merlin || Local_store.is_bound ());
    STbl.clear !files;
    STbl.clear !files_uncap;
    paths := []

  let get () = List.rev !paths
  let get_paths () = List.map Path.path !paths

  (* Optimized version of [add] below, for use in [init] and [remove_dir]: since
     we are starting from an empty cache, we can avoid checking whether a unit
     name already exists in the cache simply by adding entries in reverse
     order. *)
  let add = function
    | Path.Dir dir ->
        List.iter (fun base ->
            let fn = Filename.concat dir.Dir.path base in
            STbl.replace !files base fn;
            STbl.replace !files_uncap (String.uncapitalize_ascii base) fn
          ) dir.Dir.files
    | Path.File fn ->
        let base = Filename.basename fn in
        STbl.replace !files base fn;
        STbl.replace !files_uncap (String.uncapitalize_ascii base) fn

  let init l =
    reset ();
    paths := List.rev_map Path.create l;
    List.iter add !paths

  let remove_dir dir =
    assert (not Config.merlin || Local_store.is_bound ());
    let new_paths = List.filter (function Path.Dir d -> Dir.path d <> dir | Path.File _ -> true) !paths in
    if List.compare_lengths new_paths !paths <> 0 then begin
      reset ();
      List.iter add new_paths;
      paths := new_paths
    end

  (* General purpose version of function to add a new entry to load path: We only
     add a basename to the cache if it is not already present in the cache, in
     order to enforce left-to-right precedence. *)
  let add path =
    assert (not Config.merlin || Local_store.is_bound ());
    begin match path with
    | Path.Dir dir ->
        List.iter
          (fun base ->
             let fn = Filename.concat dir.Dir.path base in
             if not (STbl.mem !files base) then
               STbl.replace !files base fn;
             let ubase = String.uncapitalize_ascii base in
             if not (STbl.mem !files_uncap ubase) then
               STbl.replace !files_uncap ubase fn)
          dir.Dir.files
    | Path.File fn ->
        let base = Filename.basename fn in
        if not (STbl.mem !files base) then
          STbl.replace !files base fn;
        let ubase = String.uncapitalize_ascii base in
        if not (STbl.mem !files_uncap ubase) then
          STbl.replace !files_uncap ubase fn
    end;
    paths := path :: !paths

  let add_dir dir = add (Path.dir dir)

  let add_file file = add (Path.file file)

  let add_path = function
    | Dir dir -> add_dir dir
    | File file -> add_file file

  let is_basename fn = Filename.basename fn = fn

  let find fn =
    assert (not Config.merlin || Local_store.is_bound ());
    if is_basename fn && not !Sys.interactive then
      STbl.find !files fn
    else
      find fn (get_paths ())

  let find_uncap fn =
    assert (not Config.merlin || Local_store.is_bound ());
    if is_basename fn && not !Sys.interactive then
      STbl.find !files_uncap (String.uncapitalize_ascii fn)
    else
      find_uncap fn (get_paths ())
end
