(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*          Xavier Leroy and Damien Doligez, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let generic_quote quotequote s =
  let l = String.length s in
  let b = Buffer.create (l + 20) in
  Buffer.add_char b '\'';
  for i = 0 to l - 1 do
    if s.[i] = '\''
    then Buffer.add_string b quotequote
    else Buffer.add_char b  s.[i]
  done;
  Buffer.add_char b '\'';
  Buffer.contents b

module Unix = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let concat dirname filename =
    let l = String.length dirname in
    if l = 0 || dirname.[l-1] = '/'
    then dirname ^ filename
    else dirname ^ "/" ^ filename
  let is_relative n = String.length n < 1 || n.[0] <> '/';;
  let is_implicit n =
    is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
  let check_suffix name suff =
    String.length name >= String.length suff &&
    String.sub name (String.length name - String.length suff)
                    (String.length suff) = suff
  let basename name =
    try
      let p = String.rindex name '/' + 1 in
      String.sub name p (String.length name - p)
    with Not_found ->
      name
  let dirname name =
    try
      match String.rindex name '/' with
        0 -> "/"
      | n -> String.sub name 0 n
    with Not_found ->
      "."
  let temporary_directory =
    try Sys.getenv "TMPDIR" with Not_found -> "/tmp"
  let quote = generic_quote "'\\''"  
end

module Win32 = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let concat dirname filename =
    let l = String.length dirname in
    if l = 0 || (let c = dirname.[l-1] in c = '/' || c = '\\' || c = ':')
    then dirname ^ filename
    else dirname ^ "\\" ^ filename
  let is_relative n =
    (String.length n < 1 || n.[0] <> '/')
    && (String.length n < 1 || n.[0] <> '\\')
    && (String.length n < 2 || n.[1] <> ':')
  let is_implicit n =
    is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 2 || String.sub n 0 2 <> ".\\")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
    && (String.length n < 3 || String.sub n 0 3 <> "..\\")
  let check_suffix name suff =
   String.length name >= String.length suff &&
   (let s = String.sub name (String.length name - String.length suff)
                            (String.length suff) in
    String.lowercase s = String.lowercase suff)
  let rindexsep s =
    let rec pos i =
      if i < 0 then raise Not_found
      else if (let c = s.[i] in c = '/' || c = '\\' || c = ':') then i
      else pos (i - 1)
    in pos (String.length s - 1)
  let basename name =
    try
      let p = rindexsep name + 1 in
      String.sub name p (String.length name - p)
    with Not_found ->
      name
  let dirname name =
    try
      match rindexsep name with
        0 -> "\\"
      | n ->
          let n =
            if name.[n] = ':' || (n > 0 && name.[n-1] = ':')
            then n+1 else n in
          String.sub name 0 n
    with Not_found ->
      "."
  let temporary_directory =
    try Sys.getenv "TEMP" with Not_found -> "."
  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    for i = 0 to l - 1 do
      match s.[i] with
        '\"' -> Buffer.add_string b "\\\""
      | '\\' -> if i + 1 = l then Buffer.add_string b "\\\\"
                else if s.[i + 1] = '\"' then Buffer.add_string b "\\\\\\\""
                else Buffer.add_char b '\\'
      |   c  -> Buffer.add_char b c
    done;
    Buffer.add_char b '\"';
    Buffer.contents b
end

module Cygwin = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let concat dirname filename =
    let l = String.length dirname in
    if l = 0 || (let c = dirname.[l-1] in c = '/' || c = '\\' || c = ':')
    then dirname ^ filename
    else dirname ^ "/" ^ filename
  let is_relative = Win32.is_relative
  let is_implicit = Win32.is_implicit
  let check_suffix = Win32.check_suffix
  let basename = Win32.basename
  let dirname name =
    try
      match Win32.rindexsep name with
        0 -> "/"
      | n ->
          let n =
            if name.[n] = ':' || (n > 0 && name.[n-1] = ':')
            then n+1 else n in
          String.sub name 0 n
    with Not_found ->
      "."
  let temporary_directory = Unix.temporary_directory
  let quote = Unix.quote
end

module MacOS = struct
  let current_dir_name = "."
  let parent_dir_name = ".."
  let concat dirname filename =
    let l = String.length dirname in
    if l = 0 || dirname.[l-1] = ':'
    then dirname ^ filename
    else dirname ^ ":" ^ filename
  let contains_colon n = String.contains n ':'
  let is_relative n =
    (String.length n >= 1 && n.[0] = ':')
    || not (contains_colon n)
  let is_implicit n = not (contains_colon n)
  let check_suffix = Unix.check_suffix
  let basename name =
    try
      let p = String.rindex name ':' + 1 in
      String.sub name p (String.length name - p)
    with Not_found -> name
  let dirname name =
    try match String.rindex name ':' with
        | 0 -> ":"
        | n -> String.sub name 0 n
    with Not_found -> ":"
  let temporary_directory =
    try Sys.getenv "TempFolder" with Not_found -> ":"
  let quote = generic_quote "'\182''"
end

let (current_dir_name, parent_dir_name, concat, is_relative, is_implicit,
     check_suffix, basename, dirname, temporary_directory, quote) =
  match Sys.os_type with
    "Unix" ->
      (Unix.current_dir_name, Unix.parent_dir_name, Unix.concat,
       Unix.is_relative, Unix.is_implicit, Unix.check_suffix,
       Unix.basename, Unix.dirname, Unix.temporary_directory, Unix.quote)
  | "Win32" ->
      (Win32.current_dir_name, Win32.parent_dir_name, Win32.concat,
       Win32.is_relative, Win32.is_implicit, Win32.check_suffix,
       Win32.basename, Win32.dirname, Win32.temporary_directory, Win32.quote)
  | "Cygwin" ->
      (Cygwin.current_dir_name, Cygwin.parent_dir_name, Cygwin.concat,
       Cygwin.is_relative, Cygwin.is_implicit, Cygwin.check_suffix,
       Cygwin.basename, Cygwin.dirname, 
       Cygwin.temporary_directory, Cygwin.quote)
  | "MacOS" ->
      (MacOS.current_dir_name, MacOS.parent_dir_name, MacOS.concat,
       MacOS.is_relative, MacOS.is_implicit, MacOS.check_suffix,
       MacOS.basename, MacOS.dirname, MacOS.temporary_directory, MacOS.quote)
  | _ -> assert false

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "Filename.chop_suffix" else String.sub name 0 n

let chop_extension name =
  try
    String.sub name 0 (String.rindex name '.')
  with Not_found ->
    invalid_arg "Filename.chop_extension"

external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc: int -> unit = "sys_close"

let prng = Random.State.make_self_init ();;

let temp_file_name prefix suffix =
  let rnd = (Random.State.bits prng) land 0xFFFFFF in
  concat temporary_directory (Printf.sprintf "%s%06x%s" prefix rnd suffix)
;;

let temp_file prefix suffix =
  let rec try_name counter =
    if counter >= 1000 then
      invalid_arg "Filename.temp_file: temp dir nonexistent or full";
    let name = temp_file_name prefix suffix in
    try
      close_desc(open_desc name [Open_wronly; Open_creat; Open_excl] 0o600);
      name
    with Sys_error _ ->
      try_name (counter + 1)
  in try_name 0

let open_temp_file ?(mode = [Open_text]) prefix suffix =
  let rec try_name counter =
    if counter >= 1000 then
      invalid_arg "Filename.open_temp_file: temp dir nonexistent or full";
    let name = temp_file_name prefix suffix in
    try
      (name,
       open_out_gen (Open_wronly::Open_creat::Open_excl::mode) 0o600 name)
    with Sys_error _ ->
      try_name (counter + 1)
  in try_name 0
