(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Nicolas Ojeda Bar, LexiFi                         *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let _ =
  let prefix = Sys.argv.(1) in
  let name = Filename.basename (Filename.remove_extension Sys.argv.(2)) in
  let cmo = Filename.check_suffix Sys.argv.(2) ".cmo" in
  let s = String.capitalize_ascii prefix ^ "." ^ String.capitalize_ascii name in
  Printf.printf "[@@@ocaml.deprecated \"Use %s instead.\"]\n" s;
  if cmo then
    Printf.printf "include %s\n" s
  else
    Printf.printf "include module type of struct include %s end\n" s
