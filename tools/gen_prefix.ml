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

let modules = ref []
let prefix = ref ""
let unprefix = ref ""
let mli = ref false

let read_module m =
  modules := (!mli, m) :: !modules

let gen_unprefix () =
  let modules =
    List.map (fun (mli, m) ->
        mli, Filename.remove_extension (Filename.basename m)
      ) (List.rev !modules)
  in
  List.iter (fun (mli, m) ->
      let oc =
        open_out_bin
          (Filename.concat !unprefix m ^ if mli then ".mli" else ".ml")
      in
      if mli then
        Printf.fprintf oc "include module type of struct include %s.%s end\n"
          (String.capitalize_ascii !prefix) (String.capitalize_ascii m)
      else
        Printf.fprintf oc "include %s.%s\n"
          (String.capitalize_ascii !prefix) (String.capitalize_ascii m);
      close_out oc
    ) modules

let gen_prefix () =
  let mods =
    List.map (fun (_, m) -> Filename.remove_extension (Filename.basename m))
      (List.rev !modules)
    |> List.sort_uniq String.compare
  in
  let max_width =
    List.fold_left (fun acc s -> max acc (String.length s)) 0 mods
  in
  List.iter (fun m ->
      Printf.printf "module %-*s = Ocaml_%s__%s\n"
        max_width (String.capitalize_ascii m) !prefix m
    ) mods

let spec =
  [ "-prefix", Arg.Set_string prefix, "";
    "-unprefix", Set_string unprefix, "";
    "-mli", Set mli, "" ]

let () =
  Arg.parse (Arg.align spec) read_module "";
  if !unprefix = "" then
    (if !prefix <> "" then gen_prefix ())
  else
    gen_unprefix ()
