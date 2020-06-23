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

(* Pervaisive *)

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
  (* This function comes from otherlibs/win32unix/unix.ml *)
  let maybe_quote f =
    if String.contains f ' ' ||
      String.contains f '\"' ||
      String.contains f '\t' ||
       f = ""
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
          | '\'' -> f (not quote) w ws j
          | ' ' ->
            begin
              if quote
              then f true (w ^ (string_of_char ' ')) ws j
              else begin
                if w=""
                then f false w ws j
                else f false "" (w::ws) j
              end
            end
          | _ as c -> f quote (w ^ (string_of_char c)) ws j
      end in
    if l=0 then [] else f false "" [] 0
end

module Sys = struct
  include Sys

  let run_system_command prog args =
    let command = Filename.quote_command prog args in
    match Sys.command command with
    | 0 -> ()
    | _ as exitcode ->
      Printf.eprintf "System command %s failed with status %d\n%!"
        command exitcode;
      exit 3

  let mkdir dir =
    if not (Sys.file_exists dir) then
      run_system_command "mkdir" [dir]

  let rec make_directory dir =
    if Sys.file_exists dir then ()
    else (make_directory (Filename.dirname dir); mkdir dir)

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

  let dump_file oc filename =
    let f s = output_string oc "> "; output_string oc s; output_char oc '\n' in
    iter_lines_of_file f filename

  let with_output_file ?(bin=false) x f =
    let oc = (if bin then open_out_bin else open_out) x in
    Fun.protect ~finally:(fun () -> close_out_noerr oc)
      (fun () -> f oc)

  let copy_chan ic oc =
    let m = in_channel_length ic in
    let m = (m lsr 12) lsl 12 in
    let m = max 16384 (min Sys.max_string_length m) in
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

  let force_remove file =
    if file_exists file then remove file

  external has_symlink : unit -> bool = "caml_has_symlink"

  let with_chdir path f =
    let oldcwd = Sys.getcwd () in
    Sys.chdir path;
    Fun.protect ~finally:(fun () -> Sys.chdir oldcwd) f

  let getenv_with_default_value variable default_value =
    try Sys.getenv variable with Not_found -> default_value
  let safe_getenv variable = getenv_with_default_value variable ""
end
