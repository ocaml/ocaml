(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

let current_dir_name = "."

let concat dirname filename =
  let l = String.length dirname - 1 in
  if l < 0 or String.get dirname l = '/'
  then dirname ^ filename
  else dirname ^ "/" ^ filename

let is_absolute n =
     (String.length n >= 1 & String.sub n 0 1 = "/")
  or (String.length n >= 2 & String.sub n 0 2 = "./")
  or (String.length n >= 3 & String.sub n 0 3 = "../")

let rindex s c =
  let rec pos i =
    if i < 0 then raise Not_found
    else if String.get s i = c then i
    else pos (i - 1)
  in pos (String.length s - 1)

let check_suffix name suff =
 String.length name >= String.length suff &
 String.sub name (String.length name - String.length suff) (String.length suff)
    = suff

let chop_suffix name suff =
  let n = String.length name - String.length suff in
  if n < 0 then invalid_arg "chop_suffix" else String.sub name 0 n

let chop_extension name =
  try
    String.sub name 0 (rindex name '.')
  with Not_found ->
    invalid_arg "Filename.chop_extension"

let basename name =
  try
    let p = rindex name '/' + 1 in
    String.sub name p (String.length name - p)
  with Not_found ->
    name

let dirname name =
  try
    match rindex name '/' with
      0 -> "/"
    | n -> String.sub name 0 n
  with Not_found ->
    "."

let temp_file prefix suffix =
  let temp_dir =
    try
      Sys.getenv "TMPDIR"
    with Not_found ->
      "/tmp" in
  let rec try_name counter =
    let name = concat temp_dir (prefix ^ string_of_int counter ^ suffix) in
    if Sys.file_exists name then try_name (counter + 1) else name
  in try_name 0

