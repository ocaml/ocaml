(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let current_dir_name =
  match Sys.get_os_type () with
  | "Unix" -> "."
  | "Windows NT" -> "."
  | "Macintosh" -> ":"
  | _ -> failwith "Filename.current_dir_name: unknown system"
;;

let unix_concat dirname filename =
  let l = String.length dirname in
  if l = 0 or dirname.[l-1] = '/'
  then dirname ^ filename
  else dirname ^ "/" ^ filename
;;
let wnt_concat dirname filename =
  let l = String.length dirname in
  if l = 0 or (let c = dirname.[l-1] in c = '/' or c = '\\' or c = ':')
  then dirname ^ filename
  else dirname ^ "\\" ^ filename
;;
let mac_concat dirname filename =
  let l = String.length dirname in
  if l = 0 or dirname.[l-1] = ':'
  then dirname ^ filename
  else dirname ^ ":" ^ filename
;;

let concat =
  match Sys.get_os_type () with
  | "Unix" -> unix_concat
  | "Windows NT" -> wnt_concat
  | "Macintosh" -> mac_concat
  | _ -> failwith "Filename.concat: unknown system"
;;

let unix_is_absolute n =
     (String.length n >= 1 & String.sub n 0 1 = "/")
  or (String.length n >= 2 & String.sub n 0 2 = "./")
  or (String.length n >= 3 & String.sub n 0 3 = "../")
;;
let wnt_is_absolute n =
     (String.length n >= 1 &
       (let s = String.sub n 0 1 in s = "/" or s = "\\"))
  or (String.length n >= 2 &
       (let s = String.sub n 0 2 in s = "./" or s = ".\\"))
  or (String.length n >= 3 &
       (let s = String.sub n 0 3 in s = "../" or s = "..\\"))
  or (String.length n >= 2 & String.get n 1 = ':')
;;
let mac_is_absolute n =
  try
    for i = 0 to String.length n - 1 do
      if n.[i] = ':' then raise Exit
    done;
    false
  with Exit -> true
;;

let is_absolute =
  match Sys.get_os_type () with
  | "Unix" -> unix_is_absolute
  | "Windows NT" -> wnt_is_absolute
  | "Macintosh" -> mac_is_absolute
  | _ -> failwith "Filename.is_absolute: unknown system"
;;

let lowercase s =
  let l = String.length s in
  let n = String.create l in
  for i = 0 to l - 1 do
    let c = s.[i] in
    n.[i] <- (if c >= 'A' & c <= 'Z' then Char.chr(Char.code c + 32) else c)
  done;
  n
;;

let unix_check_suffix name suff =
 String.length name >= String.length suff &
 String.sub name (String.length name - String.length suff) (String.length suff)
    = suff
;;
let wnt_check_suffix name suff =
 String.length name >= String.length suff &
 (let s = String.sub name (String.length name - String.length suff)
                          (String.length suff) in
  lowercase s = lowercase suff)
;;
let mac_check_suffix = unix_check_suffix;;

let check_suffix =
  match Sys.get_os_type () with
  | "Unix" -> unix_check_suffix
  | "Windows NT" -> wnt_check_suffix
  | "Macintosh" -> mac_check_suffix
  | _ -> failwith "Filename.check_suffix: unknown system"
;;

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "Filename.chop_suffix" else String.sub name 0 n
;;

let rindex s c =
  let rec pos i =
    if i < 0 then raise Not_found
    else if s.[i] = c then i
    else pos (i - 1)
  in pos (String.length s - 1)
;;
let rindexsep s =
  let rec pos i =
    if i < 0 then raise Not_found
    else if (let c = s.[i] in c = '/' or c = '\\' or c = ':') then i
    else pos (i - 1)
  in pos (String.length s - 1)
;;

let chop_extension name =
  try
    String.sub name 0 (rindex name '.')
  with Not_found ->
    invalid_arg "Filename.chop_extension"
;;

let unix_basename name =
  try
    let p = rindex name '/' + 1 in
    String.sub name p (String.length name - p)
  with Not_found ->
    name
;;
let unix_dirname name =
  try
    match rindex name '/' with
      0 -> "/"
    | n -> String.sub name 0 n
  with Not_found ->
    "."
;;
let wnt_basename name =
  try
    let p = rindexsep name + 1 in
    String.sub name p (String.length name - p)
  with Not_found ->
    name
;;
let wnt_dirname name =
  try
    match rindexsep name with
      0 -> "\\"
    | n -> String.sub name 0 n
  with Not_found ->
    "."
;;
let mac_basename name =
  try
    let p = rindex name ':' + 1 in
    String.sub name p (String.length name - p)
  with Not_found -> name
;;
let mac_dirname name =
  try match rindex name ':' with
      | 0 -> ":"
      | n -> String.sub name 0 n
  with Not_found -> ":"
;;

let basename =
  match Sys.get_os_type () with
  | "Unix" -> unix_basename
  | "Windows NT" -> wnt_basename
  | "Macintosh" -> mac_basename
  | _ -> failwith "Filename.basename: unknown system"
;;
let dirname =
  match Sys.get_os_type () with
  | "Unix" -> unix_dirname
  | "Windows NT" -> wnt_dirname
  | "Macintosh" -> mac_dirname
  | _ -> failwith "Filename.dirname: unknown system"
;;

let temporary_directory =
  match Sys.get_os_type () with
  | "Unix" -> (try Sys.getenv "TMPDIR" with Not_found -> "/tmp")
  | "Windows NT" -> (try Sys.getenv "TEMP" with Not_found -> "C:\\temp")
  | "Macintosh" -> (try Sys.getenv "TempFolder" with Not_found -> ":")
  | _ -> failwith "Filename.temporary_directory: unknown system"
;;

let temp_file prefix suffix =
  let rec try_name counter =
    let name =
      concat temporary_directory (prefix ^ string_of_int counter ^ suffix) in
    if Sys.file_exists name then try_name (counter + 1) else name
  in try_name 0
;;
