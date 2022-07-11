(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Xavier Leroy and Damien Doligez, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

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

(* This function implements the Open Group specification found here:
  [[1]] http://pubs.opengroup.org/onlinepubs/9699919799/utilities/basename.html
  In step 1 of [[1]], we choose to return "." for empty input.
    (for compatibility with previous versions of OCaml)
  In step 2, we choose to process "//" normally.
  Step 6 is not implemented: we consider that the [suffix] operand is
    always absent.  Suffixes are handled by [chop_suffix] and [chop_extension].
*)
let generic_basename is_dir_sep current_dir_name name =
  let rec find_end n =
    if n < 0 then String.sub name 0 1
    else if is_dir_sep name n then find_end (n - 1)
    else find_beg n (n + 1)
  and find_beg n p =
    if n < 0 then String.sub name 0 p
    else if is_dir_sep name n then String.sub name (n + 1) (p - n - 1)
    else find_beg (n - 1) p
  in
  if name = ""
  then current_dir_name
  else find_end (String.length name - 1)

(* This function implements the Open Group specification found here:
  [[2]] http://pubs.opengroup.org/onlinepubs/9699919799/utilities/dirname.html
  In step 6 of [[2]], we choose to process "//" normally.
*)
let generic_dirname is_dir_sep current_dir_name name =
  let rec trailing_sep n =
    if n < 0 then String.sub name 0 1
    else if is_dir_sep name n then trailing_sep (n - 1)
    else base n
  and base n =
    if n < 0 then current_dir_name
    else if is_dir_sep name n then intermediate_sep n
    else base (n - 1)
  and intermediate_sep n =
    if n < 0 then String.sub name 0 1
    else if is_dir_sep name n then intermediate_sep (n - 1)
    else String.sub name 0 (n + 1)
  in
  if name = ""
  then current_dir_name
  else trailing_sep (String.length name - 1)

module type SYSDEPS = sig
  val null : string
  val current_dir_name : string
  val parent_dir_name : string
  val dir_sep : string
  val is_dir_sep : string -> int -> bool
  val is_relative : string -> bool
  val is_implicit : string -> bool
  val check_suffix : string -> string -> bool
  val chop_suffix_opt : suffix:string -> string -> string option
  val temp_dir_name : string
  val quote : string -> string
  val quote_command :
    string -> ?stdin: string -> ?stdout: string -> ?stderr: string
           -> string list -> string
  val basename : string -> string
  val dirname : string -> string
end

module Unix : SYSDEPS = struct
  let null = "/dev/null"
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep s i = s.[i] = '/'
  let is_relative n = String.length n < 1 || n.[0] <> '/'
  let is_implicit n =
    is_relative n
    && (String.length n < 2 || String.sub n 0 2 <> "./")
    && (String.length n < 3 || String.sub n 0 3 <> "../")
  let check_suffix name suff =
    String.ends_with ~suffix:suff name

  let chop_suffix_opt ~suffix filename =
    let len_s = String.length suffix and len_f = String.length filename in
    if len_f >= len_s then
      let r = String.sub filename (len_f - len_s) len_s in
      if r = suffix then
        Some (String.sub filename 0 (len_f - len_s))
      else
        None
    else
      None

  let temp_dir_name =
    try Sys.getenv "TMPDIR" with Not_found -> "/tmp"
  let quote = generic_quote "'\\''"
  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat " " (List.map quote (cmd :: args))
    ^ (match stdin  with None -> "" | Some f -> " <" ^ quote f)
    ^ (match stdout with None -> "" | Some f -> " >" ^ quote f)
    ^ (match stderr with None -> "" | Some f -> if stderr = stdout
                                                then " 2>&1"
                                                else " 2>" ^ quote f)
  let basename = generic_basename is_dir_sep current_dir_name
  let dirname = generic_dirname is_dir_sep current_dir_name
end

module Win32 : SYSDEPS = struct
  let null = "NUL"
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "\\"
  let is_dir_sep s i = let c = s.[i] in c = '/' || c = '\\' || c = ':'
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
    String.lowercase_ascii s = String.lowercase_ascii suff)

  let chop_suffix_opt ~suffix filename =
    let len_s = String.length suffix and len_f = String.length filename in
    if len_f >= len_s then
      let r = String.sub filename (len_f - len_s) len_s in
      if String.lowercase_ascii r = String.lowercase_ascii suffix then
        Some (String.sub filename 0 (len_f - len_s))
      else
        None
    else
      None


  let temp_dir_name =
    try Sys.getenv "TEMP" with Not_found -> "."
  let quote s =
    let l = String.length s in
    let b = Buffer.create (l + 20) in
    Buffer.add_char b '\"';
    let rec loop i =
      if i = l then Buffer.add_char b '\"' else
      match s.[i] with
      | '\"' -> loop_bs 0 i;
      | '\\' -> loop_bs 0 i;
      | c    -> Buffer.add_char b c; loop (i+1);
    and loop_bs n i =
      if i = l then begin
        Buffer.add_char b '\"';
        add_bs n;
      end else begin
        match s.[i] with
        | '\"' -> add_bs (2*n+1); Buffer.add_char b '\"'; loop (i+1);
        | '\\' -> loop_bs (n+1) (i+1);
        | _    -> add_bs n; loop i
      end
    and add_bs n = for _j = 1 to n do Buffer.add_char b '\\'; done
    in
    loop 0;
    Buffer.contents b
(*
Quoting commands for execution by cmd.exe is difficult.
1- Each argument is first quoted using the "quote" function above, to
   protect it against the processing performed by the C runtime system,
   then cmd.exe's special characters are escaped with '^', using
   the "quote_cmd" function below.  For more details, see
   https://blogs.msdn.microsoft.com/twistylittlepassagesallalike/2011/04/23
2- The command and the redirection files, if any, must be double-quoted
   in case they contain spaces.  This quoting is interpreted by cmd.exe,
   not by the C runtime system, hence the "quote" function above
   cannot be used.  The two characters we don't know how to quote
   inside a double-quoted cmd.exe string are double-quote and percent.
   We just fail if the command name or the redirection file names
   contain a double quote (not allowed in Windows file names, anyway)
   or a percent.  See function "quote_cmd_filename" below.
3- The whole string passed to Sys.command is then enclosed in double
   quotes, which are immediately stripped by cmd.exe.  Otherwise,
   some of the double quotes from step 2 above can be misparsed.
   See e.g. https://stackoverflow.com/a/9965141
*)
  let quote_cmd s =
    let b = Buffer.create (String.length s + 20) in
    String.iter
      (fun c ->
        match c with
        | '(' | ')' | '!' | '^' | '%' | '\"' | '<' | '>' | '&' | '|' ->
            Buffer.add_char b '^'; Buffer.add_char b c
        | _ ->
            Buffer.add_char b c)
      s;
    Buffer.contents b
  let quote_cmd_filename f =
    if String.exists (function '\"' | '%' -> true | _ -> false) f then
      failwith ("Filename.quote_command: bad file name " ^ f)
    else if String.contains f ' ' then
      String.concat "" ["\""; f; "\""]
    else
      f
  (* Redirections in cmd.exe: see https://ss64.com/nt/syntax-redirection.html
     and https://docs.microsoft.com/en-us/previous-versions/windows/it-pro/windows-xp/bb490982(v=technet.10)
  *)
  let quote_command cmd ?stdin ?stdout ?stderr args =
    String.concat "" [
      "\"";
      quote_cmd_filename cmd;
      " ";
      quote_cmd (String.concat " " (List.map quote args));
      (match stdin  with None -> "" | Some f -> " <" ^ quote_cmd_filename f);
      (match stdout with None -> "" | Some f -> " >" ^ quote_cmd_filename f);
      (match stderr with None -> "" | Some f ->
                                        if stderr = stdout
                                        then " 2>&1"
                                        else " 2>" ^ quote_cmd_filename f);
      "\""
    ]
  let has_drive s =
    let is_letter = function
      | 'A' .. 'Z' | 'a' .. 'z' -> true
      | _ -> false
    in
    String.length s >= 2 && is_letter s.[0] && s.[1] = ':'
  let drive_and_path s =
    if has_drive s
    then (String.sub s 0 2, String.sub s 2 (String.length s - 2))
    else ("", s)
  let dirname s =
    let (drive, path) = drive_and_path s in
    let dir = generic_dirname is_dir_sep current_dir_name path in
    drive ^ dir
  let basename s =
    let (_drive, path) = drive_and_path s in
    generic_basename is_dir_sep current_dir_name path
end

module Cygwin : SYSDEPS = struct
  let null = "/dev/null"
  let current_dir_name = "."
  let parent_dir_name = ".."
  let dir_sep = "/"
  let is_dir_sep = Win32.is_dir_sep
  let is_relative = Win32.is_relative
  let is_implicit = Win32.is_implicit
  let check_suffix = Win32.check_suffix
  let chop_suffix_opt = Win32.chop_suffix_opt
  let temp_dir_name = Unix.temp_dir_name
  let quote = Unix.quote
  let quote_command = Unix.quote_command
  let basename = generic_basename is_dir_sep current_dir_name
  let dirname = generic_dirname is_dir_sep current_dir_name
end

module Sysdeps =
  (val (match Sys.os_type with
       | "Win32" -> (module Win32: SYSDEPS)
       | "Cygwin" -> (module Cygwin: SYSDEPS)
       | _ -> (module Unix: SYSDEPS)))

include Sysdeps

let concat dirname filename =
  let l = String.length dirname in
  if l = 0 || is_dir_sep dirname (l-1)
  then dirname ^ filename
  else dirname ^ dir_sep ^ filename

let chop_suffix name suff =
  if check_suffix name suff
  then String.sub name 0 (String.length name - String.length suff)
  else invalid_arg "Filename.chop_suffix"

let extension_len name =
  let rec check i0 i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i0 (i - 1)
    else String.length name - i0
  in
  let rec search_dot i =
    if i < 0 || is_dir_sep name i then 0
    else if name.[i] = '.' then check i (i - 1)
    else search_dot (i - 1)
  in
  search_dot (String.length name - 1)

let extension name =
  let l = extension_len name in
  if l = 0 then "" else String.sub name (String.length name - l) l

let chop_extension name =
  let l = extension_len name in
  if l = 0 then invalid_arg "Filename.chop_extension"
  else String.sub name 0 (String.length name - l)

let remove_extension name =
  let l = extension_len name in
  if l = 0 then name else String.sub name 0 (String.length name - l)

external open_desc: string -> open_flag list -> int -> int = "caml_sys_open"
external close_desc: int -> unit = "caml_sys_close"

let prng_key =
  Domain.DLS.new_key Random.State.make_self_init

let temp_file_name temp_dir prefix suffix =
  let random_state = Domain.DLS.get prng_key in
  let rnd = (Random.State.bits random_state) land 0xFFFFFF in
  concat temp_dir (Printf.sprintf "%s%06x%s" prefix rnd suffix)

let current_temp_dir_name =
  Domain.DLS.new_key ~split_from_parent:Fun.id (fun () -> temp_dir_name)

let set_temp_dir_name s = Domain.DLS.set current_temp_dir_name s
let get_temp_dir_name () = Domain.DLS.get current_temp_dir_name

let temp_file ?(temp_dir = Domain.DLS.get current_temp_dir_name) prefix suffix =
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      close_desc(open_desc name [Open_wronly; Open_creat; Open_excl] 0o600);
      name
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0

let open_temp_file ?(mode = [Open_text]) ?(perms = 0o600)
    ?(temp_dir = Domain.DLS.get current_temp_dir_name)
    prefix suffix =
  let rec try_name counter =
    let name = temp_file_name temp_dir prefix suffix in
    try
      (name,
       open_out_gen (Open_wronly::Open_creat::Open_excl::mode) perms name)
    with Sys_error _ as e ->
      if counter >= 1000 then raise e else try_name (counter + 1)
  in try_name 0
