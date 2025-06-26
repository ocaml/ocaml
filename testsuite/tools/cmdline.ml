(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2024 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Harness.Import

(* Split a directory into a list of directory portions, removing all the
   separators. The path can be constructed by folding [Filename.concat] over the
   list. e.g. [split_dir [] "/usr/local/bin" = ["/"; "/usr"; "local"; "bin"]] *)
let rec split_dir acc dir =
  let dirname = Filename.dirname dir in
  if dirname = dir || dirname = Filename.current_dir_name then
    (* Deal with the oddity that [Filename.dirname "C:/" = "C:/"] (i.e. that
       the terminating slash remains and so reconstituting the path will end
       up with one slash at the start and then backslashes). *)
    if Sys.win32 && dir.[String.length dir - 1] = '/' then
      (String.sub dir 0 (String.length dir - 1) ^ "\\")::acc
    else
      dir::acc
  else
    split_dir (Filename.basename dir :: acc) dirname

(* Both split_to_prefix and walk_to_prefix return the common prefix of two
   paths, along with the suffix of each path relative to that prefix and each of
   the paths themselves.

   It is required that the two paths have at least one non-root directory in
   common (otherwise [`Nothing_in_common] is returned) and neither directory
   must be rooted in the other (otherwise [`First_in_second] or
   [`Second_in_first] is returned, as appropriate).

   split_to_prefix expects two absolute paths; walk_to_prefix expects the first
   path to be absolute and the second path to contain a number of parent
   directories at the start start (i.e. be of the form ../../second).

   For example, both [split_to_prefix "/usr/bin" "/usr/lib/ocaml"] and
   [walk_to_prefix "/usr/bin" "../lib/ocaml"] give
   [(~prefix:"/usr",
     ~first:"/usr/bin", ~first_suffix:"bin",
     ~second:"/usr/lib/ocaml", ~second_suffix:"lib/ocaml")]

   Each path is split using [split_dir] and the results are re-joined using
   [Filename.concat] which, on Windows, has the effect of normalising slashes to
   backslashes. *)

let split_to_prefix first second =
  let rec loop prefix = function
  | (dir1::dirs1), (dir2::dirs2) ->
      if dir1 <> dir2 then
        match List.rev prefix with
        | [] | [_] ->
            Result.error `Nothing_in_common
        | dir::dirs ->
            let prefix = List.fold_left Filename.concat dir dirs in
            let first_suffix = List.fold_left Filename.concat dir1 dirs1 in
            let second_suffix = List.fold_left Filename.concat dir2 dirs2 in
            Result.ok (~prefix, ~first, ~first_suffix, ~second, ~second_suffix)
      else
        loop (dir1::prefix) (dirs1, dirs2)
  | [], _ ->
      Result.error `Second_in_first
  | _, [] ->
      Result.error `First_in_second
  in
  loop [] (split_dir [] first, split_dir [] second)

let concat_all empty = function
| hd::tl -> Result.ok (List.fold_left Filename.concat hd tl)
| [] -> Result.error empty

let walk_to_prefix first second =
  let rec loop suffix1 = function
  | rev_first_hd::rev_first_tl, second_hd::second_tl
    when second_hd = Filename.parent_dir_name ->
      loop (rev_first_hd::suffix1) (rev_first_tl, second_tl)
  | rev_first, suffix2 ->
      let open Result.Syntax in
      let+ prefix = concat_all `Nothing_in_common (List.rev rev_first)
      and+ first_suffix = concat_all `Second_in_first suffix1
      and+ second_suffix = concat_all `First_in_second suffix2 in
      let second = Filename.concat prefix second_suffix in
      (~prefix, ~first, ~first_suffix, ~second, ~second_suffix)
  in
  loop [] (List.rev (split_dir [] first), split_dir [] second)

let parse argv =
  let summary = ref false in
  let verbose = ref false in
  let pwd = ref "" in
  let bindir = ref "" in
  let libdir = ref "" in
  let tree =
    ref (~prefix:"", ~first:"", ~first_suffix:"", ~second:"", ~second_suffix:"")
  in
  let config =
    ref {has_ocamlnat = false; has_ocamlopt = false; has_relative_libdir = None;
         has_runtime_search = None; launcher_searches_for_ocamlrun = false;
         target_launcher_searches_for_ocamlrun = false;
         bytecode_shebangs_by_default = false; libraries = []}
  in
  let error fmt = Printf.ksprintf (fun s -> raise (Arg.Bad s)) fmt in
  let check_tree () =
    let bindir, libdir = !bindir, !libdir in
    if bindir <> "" && libdir <> "" then
      let has_relative_libdir, result =
        if Filename.is_relative libdir then
          Some libdir, walk_to_prefix bindir libdir
        else
          None, split_to_prefix bindir libdir
      in
      match result with
      | Result.Error `Nothing_in_common ->
          (* The prefix is either the root directory (/, C:\, etc.) or, on
             Windows, the two directories are actually on different drives *)
          error "directories given for --bindir and --libdir do not have a \
                 common prefix"
      | Result.Error `First_in_second ->
          error "directory given for --bindir inside that given for --libdir"
      | Result.Error `Second_in_first ->
          error "directory given for --libdir inside that given for --bindir"
      | Result.Ok ((~prefix, ~first:_, ~first_suffix:_,
                    ~second:libdir, ~second_suffix:_) as result) ->
          if Sys.file_exists (prefix ^ ".new") then
            error "can't rename %s to %s.new as the latter already exists!"
                  prefix prefix
          else if Sys.file_exists (Filename.concat libdir "ld.conf.bak") then
            error "can't backup ld.conf to ld.conf.bak as the latter already \
                   exists!"
          else begin
            tree := result;
            config := {!config with has_relative_libdir}
          end
  in
  let check_exists ~absolute r dir =
    if Filename.is_relative dir then
      if absolute then
        raise (Arg.Bad (dir ^ ": is not an absolute path"))
      else if Filename.is_implicit dir then
        raise (Arg.Bad (dir ^ ": is not an explicit-relative path"))
      else
        check_tree (r := dir)
    else if Sys.file_exists dir then
      if Sys.is_directory dir then
        check_tree (r := dir)
      else
        raise (Arg.Bad (dir ^ ": not a directory"))
    else
      raise (Arg.Bad (dir ^ ": directory not found"))
  in
  let has_ocamlnat has_ocamlnat () = config := {!config with has_ocamlnat} in
  let has_ocamlopt has_ocamlopt () = config := {!config with has_ocamlopt} in
  let parse_search = function
  | "enable" -> true
  | "always" -> false
  | _ ->
      raise (Arg.Bad
        "--with-runtime-search: argument should be either enable or always")
  in
  let has_runtime_search arg =
    let has_runtime_search = Option.map parse_search arg in
    if has_runtime_search <> None then
      error "--with-runtime-search is not implemented!";
    config := {!config with has_runtime_search}
  in
  let args = Arg.align [
    "--pwd", Arg.Set_string pwd, "<pwd>\tCurrent working directory to use";
    "--bindir", Arg.String (check_exists ~absolute:true bindir), "\
<bindir>\tDirectory containing programs (must share a prefix with --libdir)";
    "--libdir", Arg.String (check_exists ~absolute:true libdir), "\
<libdir>\tDirectory containing stdlib.cma (must share a prefix with --bindir)";
    "--summary", Arg.Set summary, "";
    "--verbose", Arg.Set verbose, "";
    "--with-ocamlnat", Arg.Unit (has_ocamlnat true), "\
\tNative toplevel (ocamlnat) is installed in the directory given in --bindir";
    "--without-ocamlnat", Arg.Unit (has_ocamlnat false), "";
    "--with-ocamlopt", Arg.Unit (has_ocamlopt true), "\
\tNative compiler (ocamlopt) is installed in the directory given in --bindir";
    "--without-ocamlopt", Arg.Unit (has_ocamlopt false), "";
    "--with-runtime-search",
      Arg.String (fun s -> has_runtime_search (Some s)), "\
\tCompiler bytecode binaries can search for their runtimes";
    "--without-runtime-search",
      Arg.Unit (fun () -> has_runtime_search None), "";
  ] in
  let libraries lib =
    config := {!config with libraries = [lib]::config.contents.libraries}
  in
  let usage = "\n\
Usage: test_install --bindir <bindir> --libdir <libdir> <options> [libraries]\n\
options are:" in
  match Arg.parse_argv ~current:(ref 0) argv args libraries usage with
  | exception Arg.Bad msg ->
      Result.error (2, msg)
  | exception Arg.Help msg ->
      Result.error (0, msg)
  | () ->
      let config, pwd, summarise_only, verbose =
        !config, !pwd, !summary, !verbose in
      let ~prefix,
          ~first:bindir, ~first_suffix:bindir_suffix,
          ~second:libdir, ~second_suffix:libdir_suffix = !tree in
      Result.ok (~config, ~pwd, ~prefix, ~bindir, ~bindir_suffix, ~libdir,
                 ~libdir_suffix, ~summarise_only, ~verbose)
