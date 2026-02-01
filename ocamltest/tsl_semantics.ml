(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sebastien Hinderer, projet Gallium, INRIA Paris            *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Interpretation of TSL blocks and operations on test trees *)

open Ocamltest_stdlib
open Tsl_ast

let apply_modifiers env modifiers_name =
  let name = modifiers_name.node in
  let modifier = Environments.Include name in
  Environments.apply_modifier env modifier

(* `decl` is true iff the variable is being assigned with `set`. *)
let add_to_env decl variable_name value env =
  let var = Variables.from_name variable_name in
  let builtin = Option.is_some (Variables.find_variable variable_name) in
  let defined = Environments.is_variable_defined var env in
  let known = builtin || defined in
  if decl then begin
    (* Defining a new variable with `set var = value` *)
    if known then raise (Variables.Variable_already_registered variable_name)
  end else begin
    (* Changing the value of an existing variable with `var = value` *)
    if not known then raise (Variables.No_such_variable variable_name)
  end;
  Environments.add var value env

let append_to_env variable_name value env =
  let variable = Variables.from_name variable_name in
  let builtin = Option.is_some (Variables.find_variable variable_name) in
  let defined = Environments.is_variable_defined variable env in
  if builtin || defined then
    Environments.append variable value env
  else
    raise (Variables.No_such_variable variable_name)

let interpret_environment_statement env statement = match statement.node with
  | Assignment (decl, var, value) ->
      add_to_env decl var.node value.node env
  | Append (var, value) ->
      append_to_env var.node value.node env
  | Include modifiers_name ->
      apply_modifiers env modifiers_name
  | Unset var ->
      Environments.unsetenv (Variables.from_name var.node) env

exception No_such_test_or_action of string

let lookup_test located_name =
  let name = located_name.node in
  match Tests.lookup name with
  | None ->
    begin match Actions.lookup name with
    | None -> raise (No_such_test_or_action name)
    | Some action ->
      Tests.test_of_action action
    end
  | Some test -> test

type behavior =
  | Skip_all
  | Run

type summary = Test_result.status = Pass | Skip | Fail
let string_of_summary = Test_result.string_of_status

(* 'not' preserves failure and negates skips *)
let skip_negation = function
  | Fail -> Fail
  | Pass -> Skip
  | Skip -> Pass

(* The sequential join passes if both tests pass.

   This implies that a linear sequence of actions, a path along the
   test tree, is considered successful if all actions passed. *)
let join_sequential r1 r2 =
  match r1, r2 with
  | Fail, _ | _, Fail -> Fail
  | Pass, Pass -> Pass
  | Skip, _ | _, Skip -> Skip

(* The parallel join passes if either test passes.

   This implies that a test formed of several parallel branches is
   considered successful if at least one of the branches is successful.
*)
let join_parallel r1 r2 =
  match r1, r2 with
  | Fail, _ | _, Fail -> Fail
  | Pass, _ | _, Pass -> Pass
  | Skip, Skip -> Skip

let run_environment_statement ~add_msg ~report_error env s =
  match interpret_environment_statement env s with
  | env -> Ok env
  | exception e ->
    let bt = Printexc.get_backtrace () in
    let line = s.loc.Location.loc_start.Lexing.pos_lnum in
    Printf.ksprintf add_msg "line %d %s" line (report_error s.loc e bt);
    Error ()

let run ~log ~add_msg ~report_error behavior env summ ast =
  let run_statement (behavior, env, summ) = function
    | Environment_statement s ->
      begin match run_environment_statement ~add_msg ~report_error env s with
      | Ok env' -> Ok (behavior, env', summ)
      | Error () -> Error Fail
      end
    | Test (sign, name, mods) ->
      let locstr =
        if name.loc = Location.none then
          "default"
        else
          Printf.sprintf "line %d" name.loc.Location.loc_start.Lexing.pos_lnum
      in
      let (msg, behavior, env, result) =
        match behavior with
        | Skip_all -> ("=> n/a", Skip_all, env, Test_result.skip)
        | Run ->
          begin try
            let testenv = List.fold_left apply_modifiers env mods in
            let test = lookup_test name in
            let (result, newenv) = Tests.run log testenv test in
            let result =
              match sign with
              | Pos -> result
              | Neg -> { result with status = skip_negation result.status }
            in
            let msg = Test_result.string_of_result result in
            let sub_behavior =
              if Test_result.is_pass result then Run else Skip_all in
            (msg, sub_behavior, newenv, result)
          with e ->
            let bt = Printexc.get_backtrace () in
            (report_error name.loc e bt, Skip_all, env, Test_result.fail)
          end
      in
      Printf.ksprintf add_msg "%s (%s) %s" locstr name.node msg;
      let summ = join_sequential summ result.status in
      Ok (behavior, env, summ)
  in
  let rec run_tree behavior env summ (Ast (stmts, subs)) =
    match List.fold_left_result run_statement (behavior, env, summ) stmts with
    | Error e -> e
    | Ok (behavior, env, summ) ->
        (* If [subs] is empty, there are no further test actions to
           perform: we are at the end of a test path and can report
           our current summary. Otherwise we continue with each
           branch, and parallel-join the result summaries. *)
        begin match subs with
        | [] -> summ
        | _ ->
            List.fold_left join_parallel Skip
              (List.map (run_tree behavior env summ) subs)
        end
  in run_tree behavior env summ ast
