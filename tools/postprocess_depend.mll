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

{
module SMap = Map.Make (String)

let addprefix s =
  Filename.dirname s ^ "/compilerlibs__" ^ Filename.basename s

let map =
  Postprocess_depend_data.v
  |> String.split_on_char ' '
  |> List.map String.trim
  |> List.filter ((<>) "")
  |> List.map Filename.remove_extension
  |> List.fold_left (fun map s -> SMap.add s (addprefix s) map) SMap.empty

let subst s =
  Option.value ~default:s (SMap.find_opt s map)
}

rule replace = parse
| ("../"* as pre) (['/''a'-'z''A'-'Z''_''0'-'9''$''('')']+ as s) ('.' as post)
    { print_string pre;
      print_string (subst s);
      print_char post;
      replace lexbuf }
| _ as c { print_char c; replace lexbuf }
| eof { () }

{
let () =
  replace @@ Lexing.from_channel stdin
}
