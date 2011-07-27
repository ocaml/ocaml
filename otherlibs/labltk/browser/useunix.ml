(*************************************************************************)
(*                                                                       *)
(*                         OCaml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open StdLabels
open UnixLabels

let get_files_in_directory dir =
  let len = String.length dir in
  let dir =
    if len > 0 && Sys.os_type = "Win32" &&
     (dir.[len-1] = '/' || dir.[len-1] = '\\')
    then String.sub dir ~pos:0 ~len:(len-1)
    else dir
  in match
    try Some(opendir dir) with Unix_error _ -> None
  with
    None -> []
  | Some dirh ->
      let rec get_them l =
        match
          try Some(readdir dirh) with _ -> None
        with
        | Some x ->
            get_them (x::l)
        | None ->
            closedir dirh; l
      in
      List.sort ~cmp:compare (get_them [])

let is_directory name =
  try
    (stat name).st_kind = S_DIR
  with _ -> false

let concat dir name =
  let len = String.length dir in
  if len = 0 then name else
  if dir.[len-1] = '/' then dir ^ name
  else dir ^ "/" ^ name

let get_directories_in_files ~path =
  List.filter ~f:(fun x -> is_directory  (concat path x))

(************************************************** Subshell call *)
let subshell ~cmd =
  let rc = open_process_in cmd in
  let rec it l =
    match
      try Some(input_line rc) with _ -> None
    with
      Some x -> it (x::l)
    | None -> List.rev l
  in
  let answer = it [] in
  ignore (close_process_in rc);
  answer
