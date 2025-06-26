(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Import = struct
  type launch_mode = Header_exe | Header_shebang

  type executable =
  | Tendered of {header: launch_mode; dlls: bool; runtime: string}
  | Custom
  | Vanilla

  type phase = Original | Renamed

  type mode = Bytecode | Native

  type config = {
    has_ocamlnat: bool;
    has_ocamlopt: bool;
    has_relative_libdir: string option;
    has_runtime_search: bool option;
    launcher_searches_for_ocamlrun: bool;
    target_launcher_searches_for_ocamlrun: bool;
    bytecode_shebangs_by_default: bool;
    libraries: string list list
  }
end

open Import

let exe =
  if Sys.win32 then
    Fun.flip (^) ".exe"
  else
    Fun.id

external proc_self_exe : unit -> string option = "caml_sys_proc_self_exe"
let no_caml_executable_name = (proc_self_exe () = None)

(* Belt-and-braces file removal function - allow up to 30 seconds for
   Windows Defender and other nonsense *)
let rec erase_file retries path =
  try Sys.remove path
  with Sys_error _ when Sys.win32 ->
    (* Deal with read-only attribute on Windows. Ignore any error from chmod
       so that the message always come from Sys.remove *)
    let () = try Unix.chmod path 0o666 with Sys_error _ -> () in
    try Sys.remove path
    with Sys_error _ when retries > 0 ->
      Unix.sleep 1;
      erase_file (pred retries) path

let erase_file path = erase_file 30 path

let lib mode name =
  if mode = Native then
    name ^ ".cmxa"
  else
    name ^ ".cma"

let files_for ?(source_and_cmi = true) mode name files =
  let add_if cond item files = if cond then item :: files else files in
  files
  |> add_if (mode = Native) (name ^ Config.ext_obj)
  |> add_if (mode = Bytecode) (name ^ ".cmo")
  |> add_if (mode = Native) (name ^ ".cmx")
  |> add_if source_and_cmi (name ^ ".cmi")
  |> add_if source_and_cmi (name ^ ".ml")

let fail_because fmt =
  Format.ksprintf (fun s -> prerr_endline s; exit 1) fmt

(* ocamlc cannot be directly executed after renaming the prefix if native
   compilation is disabled (because ocamlc will be ocamlc.byte, since ocamlc.opt
   isn't built) and the bytecode launcher can't search for the runtime. *)
let ocamlc_fails_after_rename config =
  not config.has_ocamlopt && not config.launcher_searches_for_ocamlrun

module Filename = struct
  include Filename

  let is_dir_sep =
    if Sys.win32 then
      function '\\' | '/' -> true | _ -> false
    else
      (=) '/'
end

module String = struct
  include String

  let path_starts_with =
    if Sys.win32 then
      fun ~prefix s ->
        if String.length s < String.length prefix then
          false
        else
          let f = function '\\' -> '/' | c -> c in
          let prefix = String.map f prefix in
          let s = String.map f s in
          String.starts_with ~prefix s
    else
      String.starts_with

  let remove_prefix ~prefix s =
    if path_starts_with ~prefix s then
      let l = String.length prefix in
      Some (String.sub s l (String.length s - l))
    else
      None

  let find s p =
    let max = length s - 1 in
    if max = -1 then
      None
    else
      let rec loop i =
        if p s.[i] then
          Some i
        else if i < max then
          loop (succ i)
        else
          None
      in
      loop 0
end

let pp_path ~prefix ~bindir_suffix ~libdir_suffix ~test_root f path =
  match String.remove_prefix ~prefix path with
  | Some remainder ->
      if remainder = "" then
        Format.pp_print_string f "$prefix"
      else begin
        match String.find remainder Filename.is_dir_sep with
        | None ->
            Format.fprintf f "$prefix%s" remainder
        | Some idx ->
            let suffix, path =
              let idx = idx + 1 in
              let suffix = String.sub remainder 0 idx in
              let path =
                String.sub remainder idx (String.length remainder - idx)
              in
              suffix, path
            in
            match String.remove_prefix ~prefix:bindir_suffix path with
            | Some path when path = "" || Filename.is_dir_sep path.[0] ->
                Format.fprintf f "$prefix%s$bindir%s" suffix path
            | _ ->
                match String.remove_prefix ~prefix:libdir_suffix path with
                | Some path when path = "" || Filename.is_dir_sep path.[0] ->
                    Format.fprintf f "$prefix%s$libdir%s" suffix path
                | _ ->
                    Format.pp_print_string f ("$prefix" ^ remainder)
      end
  | None ->
      match String.remove_prefix ~prefix:test_root path with
      | Some path when path = "" || Filename.is_dir_sep path.[0] ->
          Format.pp_print_string f ("$PWD" ^ path)
      | _ ->
          if String.remove_prefix ~prefix:libdir_suffix path = Some "" then
            Format.pp_print_string f "$libdir"
          else if String.remove_prefix ~prefix:bindir_suffix path = Some "" then
            Format.pp_print_string f "$bindir"
          else
            Format.pp_print_string f path
