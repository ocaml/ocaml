(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License.                                             *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

open Unix

let get_files_in_directory dir =
  try
  let dirh = opendir dir in
  let rec get_them () =
    try
      let x = readdir dirh in
      x :: get_them ()
    with
      _ -> closedir dirh; [] 
  in
    Sort.list order:(<) (get_them ())
  with Unix_error _ -> []

let is_directory name =
  try
    (stat name).st_kind = S_DIR
  with _ -> false

let get_directories_in_files :path =
  List.filter pred:(fun x -> is_directory  (path ^ "/" ^ x))

(************************************************** Subshell call *)
let subshell :cmd =
  let rc = open_process_in cmd in
  let rec it () =
    try 
      let x = input_line rc in x :: it ()
    with _ -> []
  in 
  let answer = it () in
  ignore (close_process_in rc);
  answer
