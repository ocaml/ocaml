(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2019 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Parsing of modifier (response) files created by hooks and scripts *)

open Ocamltest_stdlib

let modifier_of_string str =
  let lexbuf = Lexing.from_string str in
  let variable_name, result = Tsl_lexer.modifier lexbuf in
  let variable =
    match Variables.find_variable variable_name with
    | None -> raise (Variables.No_such_variable variable_name)
    | Some variable -> variable
  in
  match result with
  | `Remove -> Environments.Remove variable
  | `Add value -> Environments.Add (variable, value)
  | `Append value -> Environments.Append (variable, value)

let modifiers_of_file filename =
  let ic = open_in filename in
  let rec modifiers_of_lines acc = match input_line_opt ic with
    | None -> acc
    | Some line ->
      modifiers_of_lines ((modifier_of_string (String.trim line)) :: acc) in
  let modifiers = modifiers_of_lines [] in
  close_in ic;
  List.rev modifiers
