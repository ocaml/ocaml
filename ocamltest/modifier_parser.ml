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

let modifier_of_string env str =
  let lexbuf = Lexing.from_string str in
  let variable_name, result = Tsl_lexer.modifier lexbuf in
  let variable =
    match Variables.find_variable variable_name with
    | Some variable -> variable
    | None ->
      (* [variable_name] is not defined in the global env (of builtin
         variables), but may have been declared in the test's environment. *)
      let variable = Variables.from_name variable_name in
      if Environments.is_variable_defined variable env then variable
      else raise (Variables.No_such_variable variable_name)
  in
  match result with
  | `Remove -> Environments.Remove variable
  | `Add value -> Environments.Add (variable, value)
  | `Append value -> Environments.Append (variable, value)

let modifiers_of_file env filename =
  let ic = open_in filename in
  let rec modifiers_of_lines acc = match input_line_opt ic with
    | None -> acc
    | Some line ->
      modifiers_of_lines
        ((modifier_of_string env (String.trim line)) :: acc) in
  let modifiers = modifiers_of_lines [] in
  close_in ic;
  List.rev modifiers
