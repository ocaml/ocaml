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

open Printf
open Tsl_ast

let print_tsl_ast ~compact oc ast =
  let pr fmt (*args*) = fprintf oc fmt (*args*) in

  let rec print_ast indent (Ast (stmts, subs)) =
    print_statements indent stmts;
    print_forest indent subs;

  and print_sub indent ast =
    pr "{\n";
    print_ast (indent ^ "  ") ast;
    pr "%s}" indent;

  and print_statement stmt =
    let rec print_test test =
      print_test_if test
    and print_test_if test =
      let print_self = print_test_if in
      let print_next = print_test_or in
      match test with
      | If (t1, t2, t3) ->
        pr "if "; print_next t1;
        pr " then "; print_next t2;
        begin match t3 with
        | None -> ()
        | Some t3 ->
            pr " else "; print_self t3
        end;
      | other -> print_next other
    and print_test_or test =
      let print_self = print_test_or in
      let print_next = print_test_and in
      match test with
      | Or (t1, t2) ->
        print_self t1; pr " || "; print_self t2
      | other -> print_next other
    and print_test_and test =
      let print_self = print_test_and in
      let print_next = print_test_not in
      match test with
      | And (t1, t2) ->
        print_self t1; pr " && "; print_self t2
      | other -> print_next other
    and print_test_not test =
      let print_next = print_test_atom in
      match test with
      | Not t ->
        pr "not "; print_next t
      | other -> print_next other
    and print_test_atom = function
      | Action act -> print_action act
      | Environment_statement env -> print_env env
      | (
        Not _ | And _ | Or _ | If _
        ) as other -> pr "("; print_test other; pr ")"
    and print_action { name; modifiers } =
      pr "%s" name.node;
      begin match modifiers with
      | m :: tl ->
        pr " with %s" m.node;
        List.iter (fun m -> pr ", %s" m.node) tl;
      | [] -> ()
      end;
    in
    print_test stmt

  and print_statements indent stmts =
    match stmts with
    | stmt :: tl ->
      pr "%s" indent;
      print_statement stmt;
      pr ";\n";
      if tl <> [] && not compact then pr "\n";
      print_statements indent tl;
    | [] -> ()

  and print_forest indent subs =
    if subs <> [] then begin
      pr "%s" indent;
      List.iter (print_sub indent) subs;
      pr "\n";
    end

  and print_env e =
    match e.node with
    | Assignment (set, variable, value) ->
      if set then pr "set ";
      pr "%s = \"%s\"" variable.node value.node;
    | Append (variable, value) ->
      pr "%s += \"%s\"" variable.node value.node;
    | Include ls ->
      pr "include %s" ls.node;
    | Unset ls ->
      pr "unset %s" ls.node;
  in
  print_ast " " ast;
