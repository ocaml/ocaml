(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                 Damien Doligez, Jane Street Group, LLC                 *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let rec build_string f n accu =
  if n <= 0
    then String.concat "" accu
    else build_string f (n-1) (f (n-1) :: accu)
;;

let char n = String.make 1 (Char.chr n);;

let reference n =
  if n = 8 then "\\b"
  else if n = 9 then "\\t"
  else if n = 10 then "\\n"
  else if n = 13 then "\\r"
  else if n = Char.code '\"' then "\\\""
  else if n = Char.code '\\' then "\\\\"
  else if n < 32 || n > 126 then Printf.sprintf "\\%03d" n
  else char n
;;

let raw_string = build_string char 256 [];;
let ref_string = build_string reference 256 [];;

if String.escaped raw_string <> ref_string then failwith "test:String.escaped";;
