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

let short name =
  let rec loop i =
    if i + 1 >= String.length name then
      0
    else
    if name.[i] = '_' && name.[i+1] = '_' then
      i+2
    else
      loop (i + 1)
  in
  let pos = loop 0 in
  String.sub name pos (String.length name - pos)

let _ =
  let names =
    read_line ()
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter ((<>) "")
    |> List.map Filename.basename
    |> List.map Filename.remove_extension
    |> List.map (fun s -> s, short s)
    |> List.sort_uniq (fun (_, s1) (_, s2) -> String.compare s1 s2)
  in
  let maxlen =
    let f accu (_, short) = max accu (String.length short) in
    List.fold_left f 0 names
  in
  let f (long, short) =
    Printf.printf "module %*s = %s\n" maxlen
      (String.capitalize_ascii short)
      (String.capitalize_ascii long)
  in
  List.iter f names
