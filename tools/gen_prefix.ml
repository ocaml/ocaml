(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nicolas Ojeda Bar, LexiFi SAS                      *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let () =
  let pref, mods =
    match Array.to_list Sys.argv with
    | _ :: pref :: mods ->
        pref, mods
    | _ ->
        assert false
  in
  let mods =
    List.map (fun m -> Filename.remove_extension (Filename.basename m)) mods
    |> List.sort_uniq String.compare
  in
  let max_width =
    List.fold_left (fun acc s -> max acc (String.length s)) 0 mods
  in
  List.iter (fun m ->
      Printf.printf "module %-*s = Ocaml_%s__%s\n"
        max_width (String.capitalize_ascii m) pref m
    ) mods
