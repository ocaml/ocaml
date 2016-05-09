(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL parse trees *)

open Tsl_ast

let interprete_statement env = function
  | Assignment (variable, value) ->
    Environments.add variable value env
  | Include env_name -> Environments.include_ env_name env

let interprete_statements env l =
  List.fold_left interprete_statement env l

let compile_test_spec ppf env spec =
  let action_of_name name = match Actions.lookup name with
    | None -> Format.fprintf ppf "Unknown action %s\n" name; exit 1
    | Some action -> action in
  let test = match spec.test_kind, (Tests.lookup spec.test_name) with
    | Declared_test, Some test -> test
    | Declared_test, None ->
      Format.fprintf ppf "No such test %s\n" spec.test_name;
      exit 1
    | New_test _, Some _ ->
      Format.fprintf ppf "Test %s already exists\n" spec.test_name;
      exit 1
    | New_test action_names, None ->
      begin
        let t = {
          Tests.test_name = spec.test_name;
          test_run_by_default = false;
          test_actions = List.map action_of_name action_names
        } in
        Tests.register
          t.Tests.test_name
          t.Tests.test_run_by_default
          t.Tests.test_actions;
        t
      end in
  let test_env = interprete_statements env spec.test_environemnt in
  (test, env)

let rec compile_test_specs ppf env = function
  | [] -> []
  | spec::specs ->
    (compile_test_spec ppf env spec)::
    (compile_test_specs ppf env specs)
