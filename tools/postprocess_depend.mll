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

rule replace subst = parse
| ("../"* as pre) (['/''a'-'z''A'-'Z''_''0'-'9''$''('')']+ as s) ('.' as post)
    { print_string pre;
      print_string (subst s);
      print_char post;
      replace subst lexbuf }
| _ as c { print_char c; replace subst lexbuf }
| eof { () }

{
let with_open_in fn f =
  let ic = open_in fn in
  Fun.protect ~finally:(fun () -> close_in_noerr ic)
    (fun () -> f ic)

module SMap = Map.Make (String)

let subst = ref SMap.empty

let readlist fn =
  let addprefix s =
    Filename.dirname s ^ "/compilerlibs__" ^ Filename.basename s in
  let entries =
    with_open_in fn input_line
    |> String.split_on_char ' '
    |> List.map String.trim
    |> List.filter ((<>) "")
    |> List.map Filename.remove_extension
  in
  subst :=
    List.fold_left (fun map s -> SMap.add s (addprefix s) map)
      !subst entries

let () =
  List.iter readlist @@ List.tl @@ Array.to_list Sys.argv;
  let lexbuf = Lexing.from_channel stdin in
  let f s = Option.value ~default:s (SMap.find_opt s !subst) in
  replace f lexbuf
}
