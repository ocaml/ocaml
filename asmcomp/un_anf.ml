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

(* We say that an [Ident.t] is "linear" iff:
   (a) it is used exactly once;
   (b) it is never assigned to (using [Uassign]).
*)
type ident_info =
  { used : Ident.Set.t;
    linear : Ident.Set.t;
    assigned : Ident.Set.t;
  }

let ignore_uconstant (_ : Clambda.uconstant) = ()
let ignore_function_label (_ : Clambda.function_label) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_int (_ : int) = ()
let ignore_ident (_ : Ident.t) = ()
let ignore_primitive (_ : Lambda.primitive) = ()
let ignore_string (_ : string) = ()
let ignore_int_array (_ : int array) = ()
let ignore_ident_list (_ : Ident.t list) = ()
let ignore_direction_flag (_ : Asttypes.direction_flag) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()

let assigned_idents (clam : Clambda.ulambda) =
  let assigned_idents = ref Ident.Set.empty in
  let rec loop : Clambda.ulambda -> unit = function
    (* No underscores in the pattern match, to reduce the chance of failing
       to traverse some subexpression. *)
    | Uvar id ->
      ignore_ident id
    | Uconst const ->
      (* The only variables that might occur in [const] are those in constant
         closures---and those are all bound by such closures.  It follows that
         [const] cannot contain any variables that are bound in the current
         scope, so we do not need to count them here.  (The function bodies
         of the closures will be traversed when this function is called from
         [Cmmgen.transl_function].) *)
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      List.iter loop args;
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      loop func;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      List.iter loop captured_variables;
      List.iter (fun { Clambda. label; arity; params; body; dbg } ->
          ignore_function_label label;
          ignore_int arity;
          ignore_ident_list params;
          ignore (body:Clambda.ulambda);
          ignore_debuginfo dbg)
        functions
    | Uoffset (expr, offset) ->
      loop expr;
      ignore_int offset
    | Ulet (ident, def, body) ->
      ignore_ident ident;
      loop def;
      loop body
    | Uletrec (defs, body) ->
      List.iter (fun (ident, def) ->
          ignore_ident ident;
          loop def)
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }) ->
      loop cond;
      ignore_int_array us_index_consts;
      Array.iter loop us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter loop us_actions_blocks
    | Ustringswitch (cond, branches, default) ->
      loop cond;
      List.iter (fun (str, branch) ->
          ignore_string str;
          loop branch)
        branches;
      Misc.may loop default
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      List.iter loop args
    | Ucatch (static_exn, idents, body, handler) ->
      ignore_int static_exn;
      ignore_ident_list idents;
      loop body;
      loop handler
    | Utrywith (body, ident, handler) ->
      loop body;
      ignore_ident ident;
      loop handler
    | Uifthenelse (cond, ifso, ifnot) ->
      loop cond;
      loop ifso;
      loop ifnot
    | Usequence (e1, e2) ->
      loop e1;
      loop e2
    | Uwhile (cond, body) ->
      loop cond;
      loop body
    | Ufor (ident, low, high, direction_flag, body) ->
      ignore_ident ident;
      loop low;
      loop high;
      ignore_direction_flag direction_flag;
      loop body
    | Uassign (ident, expr) ->
      assigned_idents := Ident.Set.add ident !assigned_idents;
      loop expr
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      loop e1;
      loop e2;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uunreachable ->
      ()
  in
  loop clam;
  !assigned_idents

let make_ident_info (clam : Clambda.ulambda) : ident_info =
  let t : int Ident.Tbl.t = Ident.Tbl.create 42 in
  let assigned_idents = ref Ident.Set.empty in
  let rec loop : Clambda.ulambda -> unit = function
    (* No underscores in the pattern match, to reduce the chance of failing
       to traverse some subexpression. *)
    | Uvar id ->
      begin match Ident.Tbl.find t id with
      | n -> Ident.Tbl.replace t id (n + 1)
      | exception Not_found -> Ident.Tbl.add t id 1
      end
    | Uconst const ->
      (* The only variables that might occur in [const] are those in constant
         closures---and those are all bound by such closures.  It follows that
         [const] cannot contain any variables that are bound in the current
         scope, so we do not need to count them here.  (The function bodies
         of the closures will be traversed when this function is called from
         [Cmmgen.transl_function].) *)
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      List.iter loop args;
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      loop func;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      List.iter loop captured_variables;
      List.iter (fun { Clambda. label; arity; params; body; dbg } ->
          ignore_function_label label;
          ignore_int arity;
          ignore_ident_list params;
          loop body;
          ignore_debuginfo dbg)
        functions
    | Uoffset (expr, offset) ->
      loop expr;
      ignore_int offset
    | Ulet (ident, def, body) ->
      ignore_ident ident;
      loop def;
      loop body
    | Uletrec (defs, body) ->
      List.iter (fun (ident, def) ->
          ignore_ident ident;
          loop def)
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }) ->
      loop cond;
      ignore_int_array us_index_consts;
      Array.iter loop us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter loop us_actions_blocks
    | Ustringswitch (cond, branches, default) ->
      loop cond;
      List.iter (fun (str, branch) ->
          ignore_string str;
          loop branch)
        branches;
      Misc.may loop default
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      List.iter loop args
    | Ucatch (static_exn, idents, body, handler) ->
      ignore_int static_exn;
      ignore_ident_list idents;
      loop body;
      loop handler
    | Utrywith (body, ident, handler) ->
      loop body;
      ignore_ident ident;
      loop handler
    | Uifthenelse (cond, ifso, ifnot) ->
      loop cond;
      loop ifso;
      loop ifnot
    | Usequence (e1, e2) ->
      loop e1;
      loop e2
    | Uwhile (cond, body) ->
      loop cond;
      loop body
    | Ufor (ident, low, high, direction_flag, body) ->
      ignore_ident ident;
      loop low;
      loop high;
      ignore_direction_flag direction_flag;
      loop body
    | Uassign (ident, expr) ->
      assigned_idents := Ident.Set.add ident !assigned_idents;
      loop expr
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      loop e1;
      loop e2;
      List.iter loop args;
      ignore_debuginfo dbg
    | Uunreachable ->
      ()
  in
  loop clam;
  let linear =
    Ident.Tbl.fold (fun id n acc ->
        assert (n >= 1);
        if n = 1 && not (Ident.Set.mem id !assigned_idents)
        then Ident.Set.add id acc
        else acc)
      t Ident.Set.empty
  in
  let assigned = !assigned_idents in
  let used =
    (* This doesn't work transitively and thus is somewhat restricted.  In
       particular, it does not allow us to get rid of useless chains of [let]s.
       However it should be sufficient to remove the majority of unnecessary
       [let] bindings that might hinder [Cmmgen]. *)
    Ident.Tbl.fold (fun id _n acc -> Ident.Set.add id acc)
      t assigned
  in
  { used; linear; assigned; }

(* We say that an expression is "moveable" iff it has neither effects nor
   coeffects.  (See semantics_of_primitives.mli.) *)

type moveable = Fixed | Moveable

let both_moveable a b =
  match a, b with
  | Moveable, Moveable -> Moveable
  | Moveable, Fixed
  | Fixed, Moveable
  | Fixed, Fixed -> Fixed

let primitive_moveable (prim : Lambda.primitive)
      (args : Clambda.ulambda list) =
  let second_arg_is_definitely_not_zero =
    match args with
    | _::(Uconst
         (Uconst_ref (_,
                      Some ( Uconst_int32 0l
                           | Uconst_int64 0L
                           | Uconst_nativeint 0n))
         | Uconst_int 0
         | Uconst_ptr 0))::_ ->
      false
    | _::(Uconst
        (Uconst_ref (_,
                      Some ( Uconst_int32 _
                           | Uconst_int64 _
                           | Uconst_nativeint _))
         | Uconst_int _
         | Uconst_ptr _))::_ ->
      true
    | _ ->
      false
  in
  match prim, args with
  | Pfield _, [Uconst (Uconst_ref (_, _))] ->
    (* CR mshinwell: Actually, maybe this shouldn't be needed; these should
       have been simplified to [Read_symbol_field], which doesn't yield a
       Clambda let.  This might be fixed when Inline_and_simplify can
       turn Pfield into Read_symbol_field. *)
    (* Allow field access of symbols to be moveable.  (The comment in
       flambda.mli on [Read_symbol_field] may be helpful to the reader.) *)
    Moveable
  | _ ->
    match
      Semantics_of_primitives.for_primitive prim
        ~second_arg_is_definitely_not_zero
    with
    | No_effects, No_coeffects -> Moveable
    | No_effects, Has_coeffects
    | Only_generative_effects, No_coeffects
    | Only_generative_effects, Has_coeffects
    | Arbitrary_effects, No_coeffects
    | Arbitrary_effects, Has_coeffects -> Fixed


(* valid_mutable_aliases_map and valid_mutable_aliases_revmap contains
   information about variables containing values read from a mutable
   variable. It is not always legal to do this kind of substitution:

   For instance:
   let mut a = 0 in
   let x = read a in
   (* x is a valid mutable alias here *)
   a := 1
   (* x is now an invalid mutable alias *)

   In the case

   let mut a = 0 in
   let x = read a in
   a := x + 1

   here x is fit for substitution, this is a correct transformation:

   let mut a = 0 in
   a := read a + 1

   But

   let mut a = 0 in
   let x = read a in
   a := x + 1;
   x

   here x is unfit for substitution, this is incorrect:

   let mut a = 0 in
   a := read a + 1;
   read a

   When the evaluation order is not perfectly defined, or for
   simplicity reason, the set of invalid mutable aliases may be
   over-approximated. For instance in case of a let-rec, the
   assignements of all the definitions are considered to prevent
   every use of alias variable in every definition of the let-rec.
*)

type env = {
  rewrite_mutable_aliases : bool;
  definitions : Clambda.ulambda Ident.Map.t;
  valid_mutable_aliases_map : Ident.Set.t Ident.Map.t;
  valid_mutable_aliases_revmap : Ident.t Ident.Map.t;
}

let remove_assigned env assigned =
  let valid_mutable_aliases_revmap,
      valid_mutable_aliases_map =
    Ident.Set.fold (fun id (revmap, map) ->
        match Ident.Map.find id env.valid_mutable_aliases_map with
        | exception Not_found ->
          (revmap, map)
        | invalid_set ->
          Ident.Set.fold Ident.Map.remove invalid_set revmap,
          Ident.Map.remove id map)
      assigned
      (env.valid_mutable_aliases_revmap, env.valid_mutable_aliases_map)
  in
  { env with
    valid_mutable_aliases_revmap;
    valid_mutable_aliases_map;
  }

let check_set_equality set1 set2 =
  if not (Ident.Set.equal set1 set2) then begin
    let more_left = Ident.Set.diff set1 set2 in
    let more_right = Ident.Set.diff set2 set1 in
    Misc.fatal_errorf
      "Assigned variables collection problem:@.\
       set 1:@ %a@.\
       set 2:@ %a@.\
       1 - 2:@ %a@.\
       2 - 1:@ %a@."
      Ident.Set.print set1 Ident.Set.print set2
      Ident.Set.print more_left Ident.Set.print more_right
  end

(** Eliminate, through substitution, [let]-bindings of linear variables with
    moveable defining expressions. *)
let rec un_anf_and_moveable ident_info (env:env) (clam : Clambda.ulambda)
      : Clambda.ulambda * Ident.Set.t * moveable =
  match clam with
  | Uvar id when
      env.rewrite_mutable_aliases &&
      Ident.Map.mem id env.valid_mutable_aliases_revmap ->
    let alias = Ident.Map.find id env.valid_mutable_aliases_revmap in
    un_anf_and_moveable ident_info env (Clambda.Uvar alias)
  | Uvar id ->
    begin match Ident.Map.find id env.definitions with
    (* [clam] is necessarily pure since an expression must be pure to be in the
       environment *)
    | e -> e, Ident.Set.empty, Moveable
    | exception Not_found ->
      let moveable =
        if Ident.Set.mem id ident_info.assigned then
          Fixed
        else
          Moveable
      in
      clam, Ident.Set.empty, moveable
    end
  | Uconst _ ->
    (* Constant closures are rewritten separately. *)
    clam, Ident.Set.empty, Moveable
  | Udirect_apply (label, args, dbg) ->
    let args, assigned = un_anf_list ident_info env args in
    Udirect_apply (label, args, dbg), assigned, Fixed
  | Ugeneric_apply (func, args, dbg) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let func, assigned_func = un_anf ident_info env func in
    let args, assigned_args = un_anf_list ident_info env args in
    assert(Ident.Set.equal assigned
             (Ident.Set.union assigned_func assigned_args));
    Ugeneric_apply (func, args, dbg), assigned, Fixed
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with body = fst (un_anf ident_info env ufunction.body) })
        functions
    in
    let variables_bound_by_the_closure, assigned_vars, moveable =
      un_anf_list_and_moveable ident_info env variables_bound_by_the_closure
    in
    assert(Ident.Set.equal assigned assigned_vars);
    Uclosure (functions, variables_bound_by_the_closure), assigned, moveable
  | Uoffset (clam, n) ->
    let clam, assigned, moveable = un_anf_and_moveable ident_info env clam in
    Uoffset (clam, n), assigned, moveable
  | Ulet (id, def, Uvar id') when Ident.same id id' ->
    un_anf_and_moveable ident_info env def
  | Ulet (id, Uvar _def, body) when
      not (Ident.Set.mem id ident_info.used) ->
    un_anf_and_moveable ident_info env body
  | Ulet (id, Uvar def, body) when
      Ident.Set.mem def ident_info.assigned &&
      Ident.Set.mem id ident_info.used ->
    let aliases =
      try Ident.Map.find def env.valid_mutable_aliases_map with
      | Not_found -> Ident.Set.empty
    in
    let valid_mutable_aliases_map =
      Ident.Map.add def (Ident.Set.add id aliases)
        env.valid_mutable_aliases_map
    in
    let valid_mutable_aliases_revmap =
      Ident.Map.add id def
        env.valid_mutable_aliases_revmap
    in
    let env =
      { env with
        valid_mutable_aliases_map;
        valid_mutable_aliases_revmap
      }
    in
    let body, assigned, moveable = un_anf_and_moveable ident_info env body in
    Ulet (id, Uvar def, body), assigned, moveable
  | Ulet (id, def, body) ->
    let def, assigned_def, def_moveable = un_anf_and_moveable ident_info env def in
    let env = remove_assigned env assigned_def in
    let is_linear = Ident.Set.mem id ident_info.linear in
    let is_used = Ident.Set.mem id ident_info.used in
    (* If the expression is movable, then assigned_def is empty *)
    begin match def_moveable, is_linear, is_used with
    | Moveable, _, false ->
      (* A moveable expression that is never used may be eliminated. *)
      un_anf_and_moveable ident_info env body
    | Moveable, true, true ->
      (* A moveable expression bound to a linear [Ident.t] may replace the
         single occurrence of the identifier. *)
      let definitions = Ident.Map.add id def env.definitions in
      un_anf_and_moveable ident_info { env with definitions } body
    | Moveable, false, true  (* Moveable but not used linearly. *)
    | Fixed, _, _ ->
      let body, assigned_body, body_moveable = un_anf_and_moveable ident_info env body in
      let assigned = Ident.Set.union assigned_def assigned_body in
      Ulet (id, def, body), assigned, both_moveable def_moveable body_moveable
    end
  | Uletrec (defs, body) ->
    let assigned_defs =
      List.fold_left (fun assigned (_, def) ->
          Ident.Set.union assigned (assigned_idents def))
        Ident.Set.empty defs
    in
    let env = remove_assigned env assigned_defs in
    let defs =
      List.map (fun (id, def) -> id, fst (un_anf ident_info env def)) defs
    in
    let body, assigned_body = un_anf ident_info env body in
    let assigned = Ident.Set.union assigned_defs assigned_body in
    Uletrec (defs, body), assigned, Fixed
  | Uprim (prim, args, dbg) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let args, assigned_args, args_moveable =
      un_anf_list_and_moveable ident_info env args
    in
    assert(Ident.Set.equal assigned assigned_args);
    let moveable =
      both_moveable args_moveable (primitive_moveable prim args)
    in
    Uprim (prim, args, dbg), assigned, moveable
  | Uswitch (cond, sw) ->
    let cond, assigned_cond = un_anf ident_info env cond in
    let env = remove_assigned env assigned_cond in
    let us_actions_consts, assigned_consts =
      un_anf_array ident_info env sw.us_actions_consts
    in
    let us_actions_blocks, assigned_blocks =
      un_anf_array ident_info env sw.us_actions_blocks
    in
    let sw =
      { sw with
        us_actions_consts;
        us_actions_blocks;
      }
    in
    let assigned =
      Ident.Set.union assigned_cond
        (Ident.Set.union assigned_consts assigned_blocks)
    in
    Uswitch (cond, sw), assigned, Fixed
  | Ustringswitch (cond, branches, default) ->
    let cond, assigned_cond = un_anf ident_info env cond in
    let env = remove_assigned env assigned_cond in
    let branches, assigned_branches =
      un_anf_list_snd ident_info env branches
    in
    let default, assigned_default =
      match default with
      | None -> None, Ident.Set.empty
      | Some default ->
        let default, assigned = un_anf ident_info env default in
        Some default, assigned
    in
    let assigned =
      Ident.Set.union assigned_cond
        (Ident.Set.union assigned_branches assigned_default)
    in
    Ustringswitch (cond, branches, default), assigned, Fixed
  | Ustaticfail (n, args) ->
    let args, assigned = un_anf_list ident_info env args in
    Ustaticfail (n, args), assigned, Fixed
  | Ucatch (n, ids, body, handler) ->
    let body, assigned_body = un_anf ident_info env body in
    let env = remove_assigned env assigned_body in
    let handler, assigned_handler = un_anf ident_info env handler in
    let assigned = Ident.Set.union assigned_body assigned_handler in
    Ucatch (n, ids, body, handler), assigned, Fixed
  | Utrywith (body, id, handler) ->
    let body, assigned_body = un_anf ident_info env body in
    let env = remove_assigned env assigned_body in
    let handler, assigned_handler = un_anf ident_info env handler in
    let assigned = Ident.Set.union assigned_body assigned_handler in
    Utrywith (body, id, handler), assigned, Fixed
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond, assigned_cond, cond_moveable = un_anf_and_moveable ident_info env cond in
    let env = remove_assigned env assigned_cond in
    let ifso, assigned_ifso, ifso_moveable = un_anf_and_moveable ident_info env ifso in
    let ifnot, assigned_ifnot, ifnot_moveable = un_anf_and_moveable ident_info env ifnot in
    let moveable =
      both_moveable cond_moveable
        (both_moveable ifso_moveable ifnot_moveable)
    in
    let assigned =
      Ident.Set.union assigned_cond
        (Ident.Set.union assigned_ifso assigned_ifnot)
    in
    Uifthenelse (cond, ifso, ifnot), assigned, moveable
  | Usequence (e1, e2) ->
    let e1, assigned1 = un_anf ident_info env e1 in
    let env = remove_assigned env assigned1 in
    let e2, assigned2 = un_anf ident_info env e2 in
    let assigned = Ident.Set.union assigned1 assigned2 in
    Usequence (e1, e2), assigned, Fixed
  | Uwhile (cond, body) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let cond, assigned_cond = un_anf ident_info env cond in
    let body, assigned_body = un_anf ident_info env body in
    check_set_equality assigned (Ident.Set.union assigned_cond assigned_body);
    Uwhile (cond, body), assigned, Fixed
  | Ufor (id, low, high, direction, body) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let low, assigned_low = un_anf ident_info env low in
    let high, assigned_high = un_anf ident_info env high in
    let body, assigned_body = un_anf ident_info env body in
    check_set_equality assigned
      (Ident.Set.union assigned_low
         (Ident.Set.union assigned_high assigned_body));
    Ufor (id, low, high, direction, body), assigned, Fixed
  | Uassign (id, expr) ->
    let expr, assigned_expr = un_anf ident_info env expr in
    let assigned = Ident.Set.add id assigned_expr in
    Uassign (id, expr), assigned, Fixed
  | Usend (kind, e1, e2, args, dbg) ->
    let assigned = assigned_idents clam in
    let env = remove_assigned env assigned in
    let e1, assigned1 = un_anf ident_info env e1 in
    let e2, assigned2 = un_anf ident_info env e2 in
    let args, assigned_args = un_anf_list ident_info env args in
    let assigned' =
      Ident.Set.union assigned_args
        (Ident.Set.union assigned1 assigned2)
    in
    assert(Ident.Set.equal assigned assigned');
    Usend (kind, e1, e2, args, dbg), assigned, Fixed
  | Uunreachable ->
    Uunreachable, Ident.Set.empty, Fixed

and un_anf ident_info env clam : Clambda.ulambda * Ident.Set.t =
  let clam, assigned, _moveable = un_anf_and_moveable ident_info env clam in
  clam, assigned

and un_anf_list_and_moveable ident_info env clams
      : Clambda.ulambda list * Ident.Set.t * moveable =
  List.fold_right (fun clam (l, acc_assigned, acc_moveable) ->
      let clam, assigned, moveable = un_anf_and_moveable ident_info env clam in
      clam :: l,
      Ident.Set.union acc_assigned assigned,
      both_moveable moveable acc_moveable)
    clams ([], Ident.Set.empty, Moveable)

and un_anf_list ident_info env clams : Clambda.ulambda list * Ident.Set.t =
  let clams, assigned, _moveable = un_anf_list_and_moveable ident_info env clams in
  clams, assigned

and un_anf_list_snd ident_info env clams : ('a * Clambda.ulambda) list * Ident.Set.t =
  List.fold_right (fun (v, clam) (l, acc_assigned) ->
      let clam, assigned = un_anf ident_info env clam in
      (v, clam) :: l, Ident.Set.union assigned acc_assigned)
    clams ([], Ident.Set.empty)

and un_anf_array ident_info env clams : Clambda.ulambda array * Ident.Set.t =
  let l, assigned = un_anf_list ident_info env (Array.to_list clams) in
  Array.of_list l, assigned

let empty_env rewrite_mutable_aliases =
  { rewrite_mutable_aliases;
    definitions = Ident.Map.empty;
    valid_mutable_aliases_map = Ident.Map.empty;
    valid_mutable_aliases_revmap = Ident.Map.empty;
  }

let apply clam =

  let pass rewrite_mutable_aliases clam =
    let ident_info = make_ident_info clam in
    let clam, assigned =
      un_anf ident_info (empty_env rewrite_mutable_aliases) clam
    in
    check_set_equality assigned (assigned_idents clam);
    clam
  in

  (* The first pass simplifies pure expressions as much as possible.
     It does not subsitute mutable variable accesses to keep as many
     expression pure as possible.

     The second one rewrite mutable accesses.

     The third one eliminate dead variables introduced by the second
     one. *)

  let clam = pass false clam in
  let clam = pass true clam in
  let clam = pass false clam in

  if !Clflags.dump_clambda then begin
    Format.eprintf "@.un-anf:@ %a@." Printclambda.clambda clam
  end;
  clam
