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
  scope_children: scope list ref;
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
    scope_children = ref [];
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
    scope_children = ref [];
  } in
  (* Register as child of parent *)
  parent.scope_children := new_scope :: !(parent.scope_children);
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

let rec build_lexical_scope (scope : scope) : Var_tracking.lexical_scope =
  (* Convert this scope's variables *)
  let vars = List.map entry_to_var_info (List.rev !(scope.scope_vars)) in

  (* Get scope bounds *)
  let start_label, end_label =
    match !(scope.scope_vars) with
    | [] ->
        (* No variables in this scope - use bounds from children or 0 *)
        (match !(scope.scope_children) with
         | [] -> (0, 0)
         | child :: _ ->
             (* Use first child's bounds *)
             (match !(child.scope_vars) with
              | [] -> (0, 0)
              | first :: _ ->
                  let start = match first.ve_start_label with Some l -> l | None -> 0 in
                  let end_l = match first.ve_end_label with Some l -> l | None -> 0 in
                  (start, end_l)))
    | first :: _ ->
        let start = match first.ve_start_label with Some l -> l | None -> 0 in
        let end_l = match first.ve_end_label with Some l -> l | None -> 0 in
        (start, end_l)
  in

  (* Recursively build nested scopes *)
  let nested = List.map build_lexical_scope (List.rev !(scope.scope_children)) in

  {
    Var_tracking.scope_start = start_label;
    scope_end = end_label;
    scope_vars = vars;
    nested_scopes = nested;
  }

let finalize tracker =
  (* Set end labels for parameters (function end) *)
  let final_label = new_label tracker in
  List.iter (fun ve -> ve.ve_end_label <- Some final_label) !(tracker.parameters);

  (* Set end labels for any locals that don't have them set *)
  let rec ensure_end_labels scope =
    List.iter (fun ve ->
      if ve.ve_end_label = None then
        ve.ve_end_label <- Some final_label
    ) !(scope.scope_vars);
    (* Recursively set for children *)
    List.iter ensure_end_labels !(scope.scope_children);
  in
  let root_scope = !(tracker.current_scope) in
  (* Walk up to find actual root *)
  let rec find_root s =
    match s.scope_parent with
    | None -> s
    | Some parent -> find_root parent
  in
  let root_scope = find_root root_scope in
  ensure_end_labels root_scope;

  (* Convert parameters *)
  let parameters = List.map entry_to_var_info (List.rev !(tracker.parameters)) in

  (* Build root scope with proper nesting *)
  let root_scope_info = build_lexical_scope root_scope in

  {
    Var_tracking.parameters;
    root_scope = root_scope_info;
  }
