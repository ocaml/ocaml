(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let find_declaration cf ({ funs } : Flambda.function_declarations) =
  Variable.Map.find (Closure_id.unwrap cf) funs

let find_declaration_variable cf ({ funs } : Flambda.function_declarations) =
  let var = Closure_id.unwrap cf in
  if not (Variable.Map.mem var funs)
  then raise Not_found
  else var

let find_free_variable cv ({ free_vars } : Flambda.set_of_closures) =
  Variable.Map.find (Var_within_closure.unwrap cv) free_vars

let function_arity (f : Flambda.function_declaration) = List.length f.params

let variables_bound_by_the_closure cf
      (decls : Flambda.function_declarations) =
  let func = find_declaration cf decls in
  let params = Variable.Set.of_list func.params in
  let functions = Variable.Map.keys decls.funs in
  Variable.Set.diff
    (Variable.Set.diff func.free_variables params)
    functions

let description_of_toplevel_node (expr : Flambda.t) =
  match expr with
  | Var id -> Format.asprintf "var %a" Variable.print id
  | Apply _ -> "apply"
  | Assign _ -> "assign"
  | Send _ -> "send"
  | Proved_unreachable -> "unreachable"
  | Let (_, id, _, _) -> Format.asprintf "let %a" Variable.print id
  | Let_rec _ -> "letrec"
  | If_then_else _ -> "if"
  | Switch _ -> "switch"
  | String_switch _ -> "stringswitch"
  | Static_raise  _ -> "staticraise"
  | Static_catch  _ -> "catch"
  | Try_with _ -> "trywith"
  | While _ -> "while"
  | For _ -> "for"

let rec same (l1 : Flambda.t) (l2 : Flambda.t) =
  l1 == l2 || (* it is ok for the string case: if they are physically the same,
                 it is the same original branch *)
  match (l1, l2) with
  | Var v1 , Var v2  -> Variable.equal v1 v2
  | Var _, _ | _, Var _ -> false
  | Apply a1 , Apply a2  ->
    a1.kind = a2.kind
      && Variable.equal a1.func a2.func
      && Misc.samelist Variable.equal a1.args a2.args
  | Apply _, _ | _, Apply _ -> false
  | Let (k1, v1, a1, b1), Let (k2, v2, a2, b2) ->
    k1 = k2 && Variable.equal v1 v2 && same_named a1 a2 && same b1 b2
  | Let _, _ | _, Let _ -> false
  | Let_rec (bl1, a1), Let_rec (bl2, a2) ->
    Misc.samelist samebinding bl1 bl2 && same a1 a2
  | Let_rec _, _ | _, Let_rec _ -> false
  | Switch (a1, s1), Switch (a2, s2) ->
    Variable.equal a1 a2 && sameswitch s1 s2
  | Switch _, _ | _, Switch _ -> false
  | String_switch (a1, s1, d1), String_switch (a2, s2, d2) ->
    Variable.equal a1 a2 &&
      Misc.samelist (fun (s1, e1) (s2, e2) -> s1 = s2 && same e1 e2) s1 s2 &&
      Misc.sameoption same d1 d2
  | String_switch _, _ | _, String_switch _ -> false
  | Static_raise (e1, a1), Static_raise (e2, a2) ->
    Static_exception.equal e1 e2 && Misc.samelist same a1 a2
  | Static_raise _, _ | _, Static_raise _ -> false
  | Static_catch (s1, v1, a1, b1), Static_catch (s2, v2, a2, b2) ->
    Static_exception.equal s1 s2 && Misc.samelist Variable.equal v1 v2 &&
      same a1 a2 && same b1 b2
  | Static_catch _, _ | _, Static_catch _ -> false
  | Try_with (a1, v1, b1), Try_with (a2, v2, b2) ->
    same a1 a2 && Variable.equal v1 v2 && same b1 b2
  | Try_with _, _ | _, Try_with _ -> false
  | If_then_else (a1, b1, c1), If_then_else (a2, b2, c2) ->
    Variable.equal a1 a2 && same b1 b2 && same c1 c2
  | If_then_else _, _ | _, If_then_else _ -> false
  | While (a1, b1), While (a2, b2) ->
    same a1 a2 && same b1 b2
  | While _, _ | _, While _ -> false
  | For { bound_var = bound_var1; from_value = from_value1;
          to_value = to_value1; direction = direction1; body = body1; },
    For { bound_var = bound_var2; from_value = from_value2;
          to_value = to_value2; direction = direction2; body = body2; } ->
    Variable.equal bound_var1 bound_var2
      && Variable.equal from_value1 from_value2
      && Variable.equal to_value1 to_value2
      && direction1 = direction2
      && same body1 body2
  | For _, _ | _, For _ -> false
  | Assign { being_assigned = being_assigned1; new_value = new_value1; },
    Assign { being_assigned = being_assigned2; new_value = new_value2; } ->
    Variable.equal being_assigned1 being_assigned2
      && Variable.equal new_value1 new_value2
  | Assign _, _ | _, Assign _ -> false
  | Send (k1, a1, b1, cl1, _), Send (k2, a2, b2, cl2, _) ->
    k1 = k2 && same a1 a2 && same b1 b2 && Misc.samelist same cl1 cl2
  | Send _, _ | _, Send _ -> false
  | Proved_unreachable, Proved_unreachable -> true

and same_named (named1 : Flambda.named) (named2 : Flambda.named) =
  match named1, named2 with
  | Symbol s1 , Symbol s2  -> Symbol.equal s1 s2
  | Symbol _, _ | _, Symbol _ -> false
  | Const c1 , Const c2 ->
    begin match c1, c2 with
    | Const_base (Const_string (s1, _)), Const_base (Const_string (s2, _)) ->
      s1 == s2 (* string constants can't be merged: they are mutable,
                  but if they are physically the same, it comes from a
                  safe case *)
    | Const_base (Const_string _), _ -> false
    | Const_base (Const_int _ | Const_char _ | Const_float _ |
                  Const_int32 _ | Const_int64 _ | Const_nativeint _), _
    | Const_pointer _, _ | Const_float _, _ | Const_float_array _, _
    | Const_immstring _, _ -> c1 = c2
    end
  | Const _, _ | _, Const _ -> false
  | Set_of_closures s1, Set_of_closures s2 -> same_set_of_closures s1 s2
  | Set_of_closures _, _ | _, Set_of_closures _ -> false
  | Project_closure f1, Project_closure f2 -> same_project_closure f1 f2
  | Project_closure _, _ | _, Project_closure _ -> false
  | Project_var v1, Project_var v2 ->
    Variable.equal v1.closure v2.closure
      && Closure_id.equal v1.closure_id v2.closure_id
      && Var_within_closure.equal v1.var v2.var
  | Project_var _, _ | _, Project_var _ -> false
  | Move_within_set_of_closures m1, Move_within_set_of_closures m2 ->
    same_move_within_set_of_closures m1 m2
  | Move_within_set_of_closures _, _ | _, Move_within_set_of_closures _ ->
    false
  | Prim (p1, al1, _), Prim (p2, al2, _) ->
    p1 = p2 && Misc.samelist Variable.equal al1 al2
  | Prim _, _ | _, Prim _ -> false
  | Expr e1, Expr e2 -> same e1 e2

and sameclosure (c1 : Flambda.function_declaration)
      (c2 : Flambda.function_declaration) =
  Misc.samelist Variable.equal c1.params c2.params
    && same c1.body c2.body

and same_set_of_closures (c1 : Flambda.set_of_closures)
      (c2 : Flambda.set_of_closures) =
  Variable.Map.equal sameclosure c1.function_decls.funs c2.function_decls.funs
    && Variable.Map.equal Variable.equal c1.free_vars c2.free_vars
    && Variable.Map.equal Variable.equal c1.specialised_args
        c2.specialised_args

and same_project_closure (s1 : Flambda.project_closure)
      (s2 : Flambda.project_closure) =
  Variable.equal s1.set_of_closures s2.set_of_closures
    && Closure_id.equal s1.closure_id s2.closure_id

and same_move_within_set_of_closures (m1 : Flambda.move_within_set_of_closures)
      (m2 : Flambda.move_within_set_of_closures) =
  Variable.equal m1.closure m2.closure
    && Closure_id.equal m1.start_from m2.start_from
    && Closure_id.equal m1.move_to m2.move_to

and samebinding (v1, n1) (v2, n2) =
  Variable.equal v1 v2 && same_named n1 n2

and sameswitch (fs1 : Flambda.switch) (fs2 : Flambda.switch) =
  let samecase (n1, a1) (n2, a2) = n1 = n2 && same a1 a2 in
  fs1.numconsts = fs2.numconsts
    && fs1.numblocks = fs2.numblocks
    && Misc.samelist samecase fs1.consts fs2.consts
    && Misc.samelist samecase fs1.blocks fs2.blocks
    && Misc.sameoption same fs1.failaction fs2.failaction

let can_be_merged = same

(* Sharing key TODO
   Not implemented yet: this avoids sharing anything *)
(* CR mshinwell for pchambart: What is happening about this? *)

type sharing_key = unit
let make_key _ = None

(* CR mshinwell: change "toplevel" name, potentially misleading *)
(* CR mshinwell: this should use the explicit ignore functions *)
let toplevel_substitution sb tree =
  let sb v = try Variable.Map.find v sb with Not_found -> v in
  let aux (flam : Flambda.t) : Flambda.t =
    match flam with
    | Var var -> Var (sb var)
    | Assign { being_assigned; new_value; } ->
      Assign { being_assigned = sb being_assigned; new_value = sb new_value; }
    | Apply { func; args; kind; dbg; } ->
      Apply { func = sb func; args = List.map sb args; kind; dbg; }
    | If_then_else (cond, e1, e2) -> If_then_else (sb cond, e1, e2)
    | Let _ | Let_rec _ | Send _ | Switch _
    | String_switch _ | Static_raise _ | Static_catch _ | Try_with _
    | While _ | For _ | Proved_unreachable -> flam
  in
  let aux_named (named : Flambda.named) : Flambda.named =
    match named with
    | Symbol _ | Const _ | Expr _ -> named
    | Set_of_closures set_of_closures ->
      Set_of_closures {
        set_of_closures with
        specialised_args =
          Variable.Map.map sb set_of_closures.specialised_args;
      }
    | Project_closure project_closure ->
      Project_closure {
        project_closure with
        set_of_closures = sb project_closure.set_of_closures;
      }
    | Move_within_set_of_closures move_within_set_of_closures ->
      Move_within_set_of_closures {
        move_within_set_of_closures with
        closure = sb move_within_set_of_closures.closure;
      }
    | Project_var project_var ->
      Project_var {
        project_var with
        closure = sb project_var.closure;
      }
    | Prim (prim, args, dbg) ->
      Prim (prim, List.map sb args, dbg)
  in
  Flambda_iterators.map_toplevel aux aux_named tree

let make_closure_declaration ~id ~body ~params : Flambda.t =
  let free_variables = Free_variables.calculate body in
  let param_set = Variable.Set.of_list params in
  if not (Variable.Set.subset param_set free_variables) then begin
    Misc.fatal_error "Flambda_utils.make_closure_declaration"
  end;
  let sb =
    Variable.Set.fold
      (fun id sb -> Variable.Map.add id (Variable.freshen id) sb)
      free_variables Variable.Map.empty in
  let body = toplevel_substitution sb body in
  let subst id = Variable.Map.find id sb in
  let function_declaration : Flambda.function_declaration =
    { stub = false;
      params = List.map subst params;
      free_variables = Variable.Set.map subst free_variables;
      body;
      dbg = Debuginfo.none;
    }
  in
  let free_vars =
    (* CR mshinwell: can be simplified now *)
    Variable.Map.fold (fun id id' fv' ->
        Variable.Map.add id' id fv')
      (Variable.Map.filter (fun id _ -> not (Variable.Set.mem id param_set)) sb)
      Variable.Map.empty
  in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let set_of_closures_var =
    Variable.create "set_of_closures"
      ~current_compilation_unit:compilation_unit
  in
  let set_of_closures =
    { Flambda.
      function_decls = {
        set_of_closures_id = Set_of_closures_id.create compilation_unit;
        funs = Variable.Map.singleton id function_declaration;
        compilation_unit;
      };
      free_vars;
      specialised_args = Variable.Map.empty;
    }
  in
  let project_closure : Flambda.named =
    Project_closure {
        set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap id;
      }
  in
  let project_closure_var =
    Variable.create "project_closure"
      ~current_compilation_unit:compilation_unit
  in
  Let (Immutable, set_of_closures_var, Set_of_closures (set_of_closures),
    Let (Immutable, project_closure_var, project_closure,
      Var (project_closure_var)))

let bind ~bindings ~body =
  List.fold_left (fun expr (var, var_def) ->
      Flambda.Let (Immutable, var, var_def, expr))
    body bindings

let name_expr (named : Flambda.named) : Flambda.t =
  let var =
    Variable.create
      ~current_compilation_unit:(Compilation_unit.get_current_exn ())
      "named"
  in
  Let (Immutable, var, named, Var var)
