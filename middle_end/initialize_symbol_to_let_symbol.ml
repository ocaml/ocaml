(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let constant_field (expr:Flambda.t)
  : Flambda.constant_defining_value_block_field option =
  match expr with
  | Let { var; defining_expr = Const c; body = Var var' ; _ } ->
    assert(Variable.equal var var');
    (* This must be true since var is the only variable in scope *)
    Some (Flambda.Const c)
  | Let { var; defining_expr = Symbol s; body = Var var' ; _ } ->
    assert(Variable.equal var var');
    Some (Flambda.Symbol s)
  | _ ->
    None

let rec loop (program : Flambda.program_body) : Flambda.program_body =
  match program with
  | Initialize_symbol (symbol, tag, fields, program) ->
    let constant_fields = List.map constant_field fields in
    begin match Misc.some_if_all_elements_are_some constant_fields with
    | None ->
      Initialize_symbol (symbol, tag, fields, loop program)
    | Some fields ->
      Let_symbol (symbol, Block (tag, fields), loop program)
    end
  | Let_symbol (symbol, const, program) ->
    Let_symbol (symbol, const, loop program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, loop program)
  | Effect (expr, program) ->
    Effect (expr, loop program)
  | End symbol ->
    End symbol

let run (program : Flambda.program) =
  { program with
    program_body = loop program.program_body;
  }
