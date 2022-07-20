(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A few extensions to OCaml's standard library *)

module Unix = Ocamltest_unix

let input_line_opt ic =
  try Some (input_line ic) with End_of_file -> None

module Char = struct
  include Char
  let is_blank c =
    c = ' ' || c = '\012' || c = '\n' || c = '\r' || c =  '\t'
end

module Filename = struct
  include Filename
  let path_sep = if Sys.win32 then ";" else ":"
  (* This function comes from otherlibs/unix/unix_win32.ml *)
  let maybe_quote f =
    if f = ""
    || String.exists (function ' ' | '\"' | '\t' -> true | _ -> false) f
    then Filename.quote f
    else f

  let make_filename name ext = String.concat "." [name; ext]

  let make_path components = List.fold_left Filename.concat "" components

  let mkexe filename = filename ^ Ocamltest_config.exe
end

module List = struct
  include List
  let rec concatmap f = function
    | [] -> []
    | x::xs -> (f x) @ (concatmap f xs)
end

module String = struct
  include Misc.Stdlib.String
  let string_of_char = String.make 1

  let words s =
    let l = String.length s in
    let rec f quote w ws i =
      if i>=l then begin
        if w<>"" then List.rev (w::ws)
        else List.rev ws
      end else begin
        let j = i+1 in
        match s.[i] with
          | '\''
          | '"' as c ->
            begin
              match quote with
              | None ->
                (* Begin quoted word *)
                f (Some c) w ws j
              | Some quote_char when quote_char = c ->
                (* End quoted word *)
                f None w ws j
              | _ ->
                (* Continue string *)
                f quote (w ^ (string_of_char c)) ws j
            end
          | ' ' ->
            begin
              if quote <> None
              then f quote (w ^ (string_of_char ' ')) ws j
              else begin
                if w=""
                then f None w ws j
                else f None "" (w::ws) j
              end
            end
          | _ as c -> f quote (w ^ (string_of_char c)) ws j
      end in
    if l=0 then [] else f None "" [] 0
end

module Sys = struct
  include Sys

  let erase_file path =
    try Sys.remove path
    with Sys_error _ when Sys.win32 && Ocamltest_config.libunix <> None ->
      (* Deal with read-only attribute on Windows. Ignore any error from chmod
         so that the message always come from Sys.remove *)
      let () = try Unix.chmod path 0o666 with Sys_error _ -> () in
      Sys.remove path

  let rm_rf path =
    let rec erase path =
      (* Sys.file_exists will return false for dangling symlinks *)
      if Sys.file_exists path then
        if Sys.is_directory path then begin
          (* path might be a symlink to a directory *)
          try Sys.remove path
          with Sys_error _ ->
            (* path is definitely a directory, not a symlink to a directory *)
            Array.iter (fun entry -> erase (Filename.concat path entry))
                       (Sys.readdir path);
            Sys.rmdir path
        end else erase_file path
      else erase_file path
    in
      if Sys.file_exists path then
        try erase path
        with Sys_error err ->
          raise (Sys_error (Printf.sprintf "Failed to remove %S (%s)" path err))
      else
        (* path could be a dangling symlink *)
        try Sys.remove path
        with Sys_error _ -> ()

  let rec make_directory dir =
    if Sys.file_exists dir then ()
    else let () = make_directory (Filename.dirname dir) in
         if not (Sys.file_exists dir) then
           Sys.mkdir dir 0o777
         else ()

  let make_directory dir =
    try make_directory dir
    with Sys_error err ->
      raise (Sys_error (Printf.sprintf "Failed to create %S (%s)" dir err))

  let with_input_file ?(bin=false) x f =
    let ic = (if bin then open_in_bin else open_in) x in
    Fun.protect ~finally:(fun () -> close_in_noerr ic)
      (fun () -> f ic)

  let file_is_empty filename =
    not (Sys.file_exists filename) ||
    with_input_file filename in_channel_length = 0

  let string_of_file filename =
    with_input_file ~bin:true filename @@ fun chan ->
    let filesize = in_channel_length chan in
    if filesize > Sys.max_string_length then
      failwith
        ("The file " ^ filename ^ " is too large to be loaded into a string")
    else begin
      try really_input_string chan filesize
      with End_of_file ->
        failwith ("Got unexpected end of file while reading " ^ filename)
    end

  let iter_lines_of_file f filename =
    let rec go ic =
      match input_line ic with
      | exception End_of_file -> ()
      | l -> f l; go ic
    in
    with_input_file filename go

  let dump_file oc ?(prefix = "") filename =
    let f s =
      output_string oc prefix; output_string oc s; output_char oc '\n' in
    iter_lines_of_file f filename

  let with_output_file ?(bin=false) x f =
    let oc = (if bin then open_out_bin else open_out) x in
    Fun.protect ~finally:(fun () -> close_out_noerr oc)
      (fun () -> f oc)

  let copy_chan ic oc =
    let m = in_channel_length ic in
    let m = (m lsr 12) lsl 12 in
    let m = Int.max 16384 (Int.min Sys.max_string_length m) in
    let buf = Bytes.create m in
    let rec loop () =
      let len = input ic buf 0 m in
      if len > 0 then begin
        output oc buf 0 len;
        loop ()
      end
    in loop ()

  let copy_file src dest =
    with_input_file ~bin:true src @@ fun ic ->
    with_output_file ~bin:true dest @@ fun oc ->
    copy_chan ic oc

  let rec copy_directory src dst =
    let full_src_path name = Filename.concat src name in
    let full_dst_path name = Filename.concat dst name in
    make_directory dst;
    let content = Array.to_list (readdir src) in
    let is_directory d = is_directory (full_src_path d) in
    let (subdirs, files) = List.partition is_directory content in
    let cp_file name = copy_file (full_src_path name) (full_dst_path name) in
    List.iter cp_file files;
    let cp_dir name =
      copy_directory (full_src_path name) (full_dst_path name)
    in
    List.iter cp_dir subdirs

  let force_remove file =
    if file_exists file then remove file

  let with_chdir path f =
    let oldcwd = Sys.getcwd () in
    Sys.chdir path;
    Fun.protect ~finally:(fun () -> Sys.chdir oldcwd) f

  let getenv_with_default_value variable default_value =
    try Sys.getenv variable with Not_found -> default_value
  let safe_getenv variable = getenv_with_default_value variable ""
end

module Seq = struct
  include Seq

  let rec equal s1 s2 =
    match s1 (), s2 () with
    | Nil, Nil -> true
    | Cons(e1, s1), Cons(e2, s2) -> e1 = e2 && equal s1 s2
    | _, _ -> false
end
