(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Joel Reymont, Claude AI Assistant                          *)
(*                                                                        *)
(*   Copyright 2025                                                       *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Variable entry: a variable and where it's defined *)
type var_entry = {
  ve_name: string;
  ve_reg: Reg.t;
  ve_typ: Cmm.machtype;
  ve_is_param: bool;
  ve_start_label: int option [@warning "-69"];
  mutable ve_end_label: int option;
}

(* Scope: a lexical scope with variables defined in it *)
type scope = {
  scope_vars: var_entry list ref;
  scope_parent: scope option;
}

(* Tracker state *)
type tracker = {
  fun_name: string [@warning "-69"];
  parameters: var_entry list ref;
  current_scope: scope ref;
  label_counter: int ref;
}

let new_label tracker =
  let l = !(tracker.label_counter) in
  tracker.label_counter := l + 1;
  l

let create ~fun_name =
  let root_scope = {
    scope_vars = ref [];
    scope_parent = None;
  } in
  {
    fun_name;
    parameters = ref [];
    current_scope = ref root_scope;
    label_counter = ref 0;
  }

let add_parameter tracker ~name ~reg ~typ =
  let entry = {
    ve_name = name;
    ve_reg = reg;
    ve_typ = typ;
    ve_is_param = true;
    ve_start_label = Some 0; (* Parameters live from function start *)
    ve_end_label = None; (* Will be set to function end *)
  } in
  tracker.parameters := entry :: !(tracker.parameters)

let add_local tracker ~name ~reg ~typ =
  let entry = {
    ve_name = name;
    ve_reg = reg;
    ve_typ = typ;
    ve_is_param = false;
    ve_start_label = Some (new_label tracker);
    ve_end_label = None; (* Will be set when scope exits *)
  } in
  let scope = !(tracker.current_scope) in
  scope.scope_vars := entry :: !(scope.scope_vars)

let enter_scope tracker =
  let parent = !(tracker.current_scope) in
  let new_scope = {
    scope_vars = ref [];
    scope_parent = Some parent;
  } in
  tracker.current_scope := new_scope

let exit_scope tracker =
  let scope = !(tracker.current_scope) in
  let end_label = new_label tracker in
  (* Set end labels for all variables in this scope *)
  List.iter (fun ve -> ve.ve_end_label <- Some end_label) !(scope.scope_vars);
  match scope.scope_parent with
  | Some parent -> tracker.current_scope := parent
  | None -> () (* At root scope *)

let reg_to_location (reg : Reg.t) : Var_tracking.var_location =
  match reg.Reg.loc with
  | Reg.Reg n -> Var_tracking.VL_Register n
  | Reg.Stack (Reg.Local n) -> Var_tracking.VL_Stack n
  | Reg.Stack (Reg.Incoming n) -> Var_tracking.VL_Stack n
  | Reg.Stack (Reg.Outgoing n) -> Var_tracking.VL_Stack n
  | Reg.Stack (Reg.Domainstate n) -> Var_tracking.VL_Stack n
  | Reg.Unknown -> Var_tracking.VL_Optimized_away

let entry_to_var_info (entry : var_entry) : Var_tracking.var_info =
  let locations = match entry.ve_start_label, entry.ve_end_label with
    | Some start, Some end_label ->
        [{
          Var_tracking.start_label = start;
          end_label;
          location = reg_to_location entry.ve_reg;
        }]
    | _ -> [] (* No lifetime info *)
  in
  {
    Var_tracking.var_name = entry.ve_name;
    var_reg = entry.ve_reg;
    var_type = entry.ve_typ;
    locations;
    is_parameter = entry.ve_is_param;
  }

let rec collect_scope_vars scope acc =
  let vars = List.map entry_to_var_info !(scope.scope_vars) in
  let acc_with_vars = vars @ acc in
  match scope.scope_parent with
  | Some parent -> collect_scope_vars parent acc_with_vars
  | None -> acc_with_vars

let finalize tracker =
  (* Set end labels for parameters (function end) *)
  let final_label = new_label tracker in
  List.iter (fun ve -> ve.ve_end_label <- Some final_label) !(tracker.parameters);

  (* Set end labels for all locals in all scopes (function end) *)
  let rec set_end_labels scope =
    List.iter (fun ve -> ve.ve_end_label <- Some final_label) !(scope.scope_vars);
    match scope.scope_parent with
    | Some parent -> set_end_labels parent
    | None -> ()
  in
  set_end_labels !(tracker.current_scope);

  (* Convert parameters *)
  let parameters = List.map entry_to_var_info (List.rev !(tracker.parameters)) in

  (* Build root scope with all collected variables *)
  let scope = !(tracker.current_scope) in
  let all_locals = collect_scope_vars scope [] in

  let root_scope = {
    Var_tracking.scope_start = 0;
    scope_end = final_label;
    scope_vars = all_locals;
    nested_scopes = []; (* TODO: Track nested scopes properly *)
  } in

  {
    Var_tracking.parameters;
    root_scope;
  }
