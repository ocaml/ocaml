(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                            John Whitington                             *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let barrier = "(* STRING.ML ONLY FROM HERE *)"
let sb = open_in "../stdlib/templates/stringbytes.template.ml"
let s = open_out "../stdlib/string.ml"
let b = open_out "../stdlib/bytes.ml"
let write_s = ref false
let write_b = ref true
let string_only_from_now_on = ref false

let write_line l =
  if !write_b && not !string_only_from_now_on then output_string b (l ^ "\n");
  if !write_s || !string_only_from_now_on then output_string s (l ^ "\n")

let () =
  try
    while true do
      let l = ref (input_line sb) in
        begin
          if !l = "" then
            begin if !write_s then write_s := false; write_b := true end
          else
          if !l = barrier then string_only_from_now_on := true else
          if String.length !l > 3 then
            match !l.[0], !l.[1], !l.[2], !l.[3] with
            | '(', '*', 'N', 'N' ->
                write_s := false; write_b := false;
            | '(', '*', 'S', 'B' ->
                write_s := true; write_b := true;
                l := "(*" ^ String.sub !l 4 (String.length !l - 4)
            | '(', '*', 'S', _ ->
                write_s := true; write_b := false;
                l := "(*" ^ String.sub !l 3 (String.length !l - 3)
            | 'S', 'B', '.', _ ->
                write_s := true; write_b := true;
                l := String.sub !l 3 (String.length !l - 3)
            | 'S', '.', _, _ ->
                write_s := true; write_b := false;
                l := String.sub !l 2 (String.length !l - 2)
            | _ -> ()
        end;
        if !l <> barrier then write_line !l
    done
  with
    End_of_file ->
      close_in sb; close_out s; close_out b
