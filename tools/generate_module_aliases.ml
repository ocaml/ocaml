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

let cmp s1 s2 =
  String.compare (String.capitalize_ascii s1) (String.capitalize_ascii s2)

let _ =
  let names =
    read_line ()
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter ((<>) "")
    |> List.map Filename.basename
    |> List.map Filename.remove_extension
    |> List.sort_uniq cmp
  in
  let maxlen =
    let f accu s = max accu (String.length s) in
    List.fold_left f 0 names
  in
  let f s =
    Printf.printf "module %*s = Compilerlibs__%s\n" maxlen
      (String.capitalize_ascii s) s
  in
  List.iter f names
