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

  and print_statements indent stmts =
    match stmts with
    | Test (sign, name, mods) :: tl ->
      pr (match sign with
          | Pos -> ""
          | Neg -> "not ");
      pr "%s%s" indent name.node;
      begin match mods with
      | m :: tl ->
        pr " with %s" m.node;
        List.iter (fun m -> pr ", %s" m.node) tl;
      | [] -> ()
      end;
      pr ";\n";
      if tl <> [] && not compact then pr "\n";
      print_statements indent tl;
    | Environment_statement env :: tl->
      print_env indent env;
      print_statements indent tl;
    | [] -> ()

  and print_forest indent subs =
    if subs <> [] then begin
      pr "%s" indent;
      List.iter (print_sub indent) subs;
      pr "\n";
    end

  and print_env indent e =
    match e.node with
    | Assignment (set, variable, value) ->
      pr "%s" indent;
      if set then pr "set ";
      pr "%s = \"%s\";\n" variable.node value.node;
    | Append (variable, value) ->
      pr "%s%s += \"%s\";\n" indent variable.node value.node;
    | Include ls ->
      pr "%sinclude %s;\n" indent ls.node;
    | Unset ls ->
      pr "%sunset %s;\n" indent ls.node;
  in
  print_ast " " ast;
