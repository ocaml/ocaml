(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-someday vlaviron for mshinwell: I believe that the phantom lets introduced
   in un_anf (when the new debug_full flag is enabled) bind mostly variables
   that were created in the middle-end. Is it relevant to generate debugging
   information for such variables ? I expect later pull requests to refine the
   generation of these phantom constructions anyway, but maybe it would already
   make sense to restrict the phantom let generation to variables with an actual
   provenance.
*)

module V = Backend_var
module VP = Backend_var.With_provenance

(* We say that an [V.t] is "linear" iff:
   (a) it is used exactly once;
   (b) it is never assigned to (using [Uassign]).
*)
type var_info =
  { used_let_bound_vars : V.Set.t;
    linear_let_bound_vars : V.Set.t;
    assigned : V.Set.t;
    closure_environment : V.Set.t;
  }

let ignore_uconstant (_ : Clambda.uconstant) = ()
let ignore_ulambda (_ : Clambda.ulambda) = ()
let ignore_ulambda_list (_ : Clambda.ulambda list) = ()
let ignore_uphantom_defining_expr_option
      (_ : Clambda.uphantom_defining_expr option) = ()
let ignore_function_label (_ : Clambda.function_label) = ()
let ignore_debuginfo (_ : Debuginfo.t) = ()
let ignore_int (_ : int) = ()
let ignore_var (_ : V.t) = ()
let ignore_var_option (_ : V.t option) = ()
let ignore_primitive (_ : Clambda_primitives.primitive) = ()
let ignore_string (_ : string) = ()
let ignore_int_array (_ : int array) = ()
let ignore_var_with_provenance (_ : VP.t) = ()
let ignore_params_with_value_kind (_ : (VP.t * Lambda.value_kind) list) = ()
let ignore_direction_flag (_ : Asttypes.direction_flag) = ()
let ignore_meth_kind (_ : Lambda.meth_kind) = ()
let ignore_value_kind (_ : Lambda.value_kind) = ()

(* CR-soon mshinwell: check we aren't traversing function bodies more than
   once (need to analyse exactly what the calls are from Cmmgen into this
   module). *)

let closure_environment_var (ufunction:Clambda.ufunction) =
  (* The argument after the arity is the environment *)
  if List.length ufunction.params = ufunction.arity + 1 then
    let (env_var, _) = List.nth ufunction.params ufunction.arity in
    assert (VP.name env_var = "env");
    Some env_var
  else
    (* closed function, no environment *)
    None

type var_uses =
  | Zero
  | One
  | More_than_one
  | Assigned

type var =
  { definition_depth : int;
    uses : var_uses; }

let incr_uses { definition_depth; uses } depth =
  assert (definition_depth <= depth);
  let uses =
    match uses with
    | Zero ->
        if definition_depth < depth then More_than_one
        else One
    | One -> More_than_one
    | More_than_one -> More_than_one
    | Assigned -> Assigned
  in
  { definition_depth; uses }

let assign_uses r = { r with uses = Assigned }

let zero definition_depth = { definition_depth; uses = Zero }

let add_definition t var depth =
  V.Tbl.add t var (zero depth)

let add_use t var depth =
  match V.Tbl.find t var with
  | info -> V.Tbl.replace t var (incr_uses info depth)
  | exception Not_found -> () (* Variable is not let-bound *)

let add_assignment t var =
  match V.Tbl.find t var with
  | info -> V.Tbl.replace t var (assign_uses info)
  | exception Not_found ->
    Misc.fatal_errorf
      "make_var_info: Assigned variable %a not let-bound"
      V.print var

let make_var_info (clam : Clambda.ulambda) : var_info =
  let t : var V.Tbl.t = V.Tbl.create 42 in
  let environment_vars = ref V.Set.empty in
  let rec loop ~depth : Clambda.ulambda -> unit = function
    (* No underscores in the pattern match, to reduce the chance of failing
       to traverse some subexpression. *)
    | Uvar var -> add_use t var depth
    | Uconst const ->
      (* The only variables that might occur in [const] are those in constant
         closures---and those are all bound by such closures.  It follows that
         [const] cannot contain any variables that are bound in the current
         scope, so we do not need to count them here.  (The function bodies
         of the closures will be traversed when this function is called from
         [Flambda_to_clambda.to_clambda_closed_set_of_closures].) *)
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      List.iter (loop ~depth) args;
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      loop ~depth func;
      List.iter (loop ~depth) args;
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      List.iter (loop ~depth) captured_variables;
      List.iter (fun (
        { Clambda. label; arity; params; return; body; dbg; env; _ } as clos) ->
          (match closure_environment_var clos with
           | None -> ()
           | Some env_var ->
             environment_vars :=
               V.Set.add (VP.var env_var) !environment_vars);
          ignore_function_label label;
          ignore_int arity;
          ignore_params_with_value_kind params;
          ignore_value_kind return;
          loop ~depth:(depth + 1) body;
          ignore_debuginfo dbg;
          ignore_var_option env)
        functions
    | Uoffset (expr, offset) ->
      loop ~depth expr;
      ignore_int offset
    | Ulet (_let_kind, _value_kind, var, def, body) ->
      add_definition t (VP.var var) depth;
      loop ~depth def;
      loop ~depth body
    | Uphantom_let (var, defining_expr_opt, body) ->
      ignore_var_with_provenance var;
      ignore_uphantom_defining_expr_option defining_expr_opt;
      loop ~depth body
    | Uletrec (defs, body) ->
      List.iter (fun (var, def) ->
          ignore_var_with_provenance var;
          loop ~depth def)
        defs;
      loop ~depth body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      List.iter (loop ~depth) args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }, dbg) ->
      loop ~depth cond;
      ignore_int_array us_index_consts;
      Array.iter (loop ~depth) us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter (loop ~depth) us_actions_blocks;
      ignore_debuginfo dbg
    | Ustringswitch (cond, branches, default) ->
      loop ~depth cond;
      List.iter (fun (str, branch) ->
          ignore_string str;
          loop ~depth branch)
        branches;
      Option.iter (loop ~depth) default
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      List.iter (loop ~depth) args
    | Ucatch (static_exn, vars, body, handler) ->
      ignore_int static_exn;
      ignore_params_with_value_kind vars;
      loop ~depth body;
      loop ~depth handler
    | Utrywith (body, var, handler) ->
      loop ~depth body;
      ignore_var_with_provenance var;
      loop ~depth handler
    | Uifthenelse (cond, ifso, ifnot) ->
      loop ~depth cond;
      loop ~depth ifso;
      loop ~depth ifnot
    | Usequence (e1, e2) ->
      loop ~depth e1;
      loop ~depth e2
    | Uwhile (cond, body) ->
      loop ~depth:(depth + 1) cond;
      loop ~depth:(depth + 1) body
    | Ufor (var, low, high, direction_flag, body) ->
      ignore_var_with_provenance var;
      loop ~depth low;
      loop ~depth high;
      ignore_direction_flag direction_flag;
      loop ~depth:(depth + 1) body
    | Uassign (var, expr) ->
      add_assignment t var;
      loop ~depth expr
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      loop ~depth e1;
      loop ~depth e2;
      List.iter (loop ~depth) args;
      ignore_debuginfo dbg
    | Uunreachable ->
      ()
  in
  loop ~depth:0 clam;
  let linear_let_bound_vars, used_let_bound_vars, assigned =
    V.Tbl.fold (fun var desc ((linear, used, assigned) as acc) ->
      match desc.uses with
      | Zero -> acc
      | One -> (V.Set.add var linear, V.Set.add var used, assigned)
      | More_than_one -> (linear, V.Set.add var used, assigned)
      | Assigned -> (linear, V.Set.add var used, V.Set.add var assigned))
      t (V.Set.empty, V.Set.empty, V.Set.empty)
  in
  { used_let_bound_vars; linear_let_bound_vars; assigned;
    closure_environment = !environment_vars;
  }

(* When sequences of [let]-bindings match the evaluation order in a subsequent
   primitive or function application whose arguments are linearly-used
   non-assigned variables bound by such lets (possibly interspersed with other
   variables that are known to be constant), and it is known that there were no
   intervening side-effects during the evaluation of the [let]-bindings,
   permit substitution of the variables for their defining expressions. *)
let let_bound_vars_that_can_be_moved var_info (clam : Clambda.ulambda) =
  let obviously_constant = ref V.Set.empty in
  let can_move = ref V.Set.empty in
  let let_stack = ref [] in
  let examine_argument_list args =
    let rec loop let_bound_vars (args : Clambda.ulambda list) =
      match let_bound_vars, args with
      | _, [] ->
        (* We've matched all arguments and will not substitute (in the
           current application being considered) any of the remaining
           [let_bound_vars].  As such they may stay on the stack. *)
        let_bound_vars
      | [], _ ->
        (* There are no more [let]-bindings to consider, so the stack
           is left empty. *)
        []
      | let_bound_vars, (Uvar arg)::args
          when V.Set.mem arg !obviously_constant ->
        loop let_bound_vars args
      | let_bound_var::let_bound_vars, (Uvar arg)::args
          when V.same let_bound_var arg
            && not (V.Set.mem arg var_info.assigned) ->
        assert (V.Set.mem arg var_info.used_let_bound_vars);
        assert (V.Set.mem arg var_info.linear_let_bound_vars);
        can_move := V.Set.add arg !can_move;
        loop let_bound_vars args
      | _::_, _::_ ->
        (* The [let] sequence has ceased to match the evaluation order
           or we have encountered some complicated argument.  In this case
           we empty the stack to ensure that we do not end up moving an
           outer [let] across a side effect. *)
        []
    in
    (* Start at the most recent let binding and the leftmost argument
       (the last argument to be evaluated). *)
    let_stack := loop !let_stack args
  in
  let rec loop : Clambda.ulambda -> unit = function
    | Uvar var ->
      if V.Set.mem var var_info.assigned then begin
        let_stack := []
      end
    | Uconst const ->
      ignore_uconstant const
    | Udirect_apply (label, args, dbg) ->
      ignore_function_label label;
      examine_argument_list args;
      (* We don't currently traverse [args]; they should all be variables
         anyway.  If this is added in the future, take care to traverse [args]
         following the evaluation order. *)
      ignore_debuginfo dbg
    | Ugeneric_apply (func, args, dbg) ->
      examine_argument_list (args @ [func]);
      ignore_debuginfo dbg
    | Uclosure (functions, captured_variables) ->
      ignore_ulambda_list captured_variables;
      (* Start a new let stack for speed. *)
      List.iter
        (fun {Clambda. label; arity; params; return; body; dbg; env; _} ->
          ignore_function_label label;
          ignore_int arity;
          ignore_params_with_value_kind params;
          ignore_value_kind return;
          let_stack := [];
          loop body;
          let_stack := [];
          ignore_debuginfo dbg;
          ignore_var_option env)
        functions
    | Uoffset (expr, offset) ->
      (* [expr] should usually be a variable. *)
      examine_argument_list [expr];
      ignore_int offset
    | Ulet (_let_kind, _value_kind, var, def, body) ->
      let var = VP.var var in
      begin match def with
      | Uconst _ ->
        (* The defining expression is obviously constant, so we don't
           have to put this [let] on the stack, and we don't have to
           traverse the defining expression either. *)
        obviously_constant := V.Set.add var !obviously_constant;
        loop body
      | _ ->
        loop def;
        if V.Set.mem var var_info.linear_let_bound_vars then begin
          let_stack := var::!let_stack
        end else begin
          (* If we encounter a non-linear [let]-binding then we must clear
             the let stack, since we cannot now move any previous binding
             across the non-linear one. *)
          let_stack := []
        end;
        loop body
      end
    | Uphantom_let (var, _defining_expr, body) ->
      ignore_var_with_provenance var;
      loop body
    | Uletrec (defs, body) ->
      (* Evaluation order for [defs] is not defined, and this case
         probably isn't important for [Cmmgen] anyway. *)
      let_stack := [];
      List.iter (fun (var, def) ->
          ignore_var_with_provenance var;
          loop def;
          let_stack := [])
        defs;
      loop body
    | Uprim (prim, args, dbg) ->
      ignore_primitive prim;
      examine_argument_list args;
      ignore_debuginfo dbg
    | Uswitch (cond, { us_index_consts; us_actions_consts;
          us_index_blocks; us_actions_blocks }, dbg) ->
      examine_argument_list [cond];
      ignore_int_array us_index_consts;
      Array.iter (fun action ->
          let_stack := [];
          loop action)
        us_actions_consts;
      ignore_int_array us_index_blocks;
      Array.iter (fun action ->
          let_stack := [];
          loop action)
        us_actions_blocks;
      ignore_debuginfo dbg;
      let_stack := []
    | Ustringswitch (cond, branches, default) ->
      examine_argument_list [cond];
      List.iter (fun (str, branch) ->
          ignore_string str;
          let_stack := [];
          loop branch)
        branches;
      let_stack := [];
      Option.iter loop default;
      let_stack := []
    | Ustaticfail (static_exn, args) ->
      ignore_int static_exn;
      examine_argument_list args
    | Ucatch (static_exn, vars, body, handler) ->
      ignore_int static_exn;
      ignore_params_with_value_kind vars;
      let_stack := [];
      loop body;
      let_stack := [];
      loop handler;
      let_stack := []
    | Utrywith (body, var, handler) ->
      let_stack := [];
      loop body;
      let_stack := [];
      ignore_var_with_provenance var;
      loop handler;
      let_stack := []
    | Uifthenelse (cond, ifso, ifnot) ->
      examine_argument_list [cond];
      let_stack := [];
      loop ifso;
      let_stack := [];
      loop ifnot;
      let_stack := []
    | Usequence (e1, e2) ->
      loop e1;
      let_stack := [];
      loop e2;
      let_stack := []
    | Uwhile (cond, body) ->
      let_stack := [];
      loop cond;
      let_stack := [];
      loop body;
      let_stack := []
    | Ufor (var, low, high, direction_flag, body) ->
      ignore_var_with_provenance var;
      (* Cmmgen generates code that evaluates low before high,
         but we don't do anything here at the moment anyway. *)
      ignore_ulambda low;
      ignore_ulambda high;
      ignore_direction_flag direction_flag;
      let_stack := [];
      loop body;
      let_stack := []
    | Uassign (var, expr) ->
      ignore_var var;
      ignore_ulambda expr;
      let_stack := []
    | Usend (meth_kind, e1, e2, args, dbg) ->
      ignore_meth_kind meth_kind;
      ignore_ulambda e1;
      ignore_ulambda e2;
      ignore_ulambda_list args;
      let_stack := [];
      ignore_debuginfo dbg
    | Uunreachable ->
      let_stack := []
  in
  loop clam;
  !can_move

(* Substitution of an expression for a let-moveable variable can cause the
   surrounding expression to become fixed.  To avoid confusion, do the
   let-moveable substitutions first. *)
let rec substitute_let_moveable is_let_moveable env (clam : Clambda.ulambda)
      : Clambda.ulambda =
  match clam with
  | Uvar var ->
    if not (V.Set.mem var is_let_moveable) then
      clam
    else
      begin match V.Map.find var env with
      | clam -> clam
      | exception Not_found ->
        Misc.fatal_errorf "substitute_let_moveable: Unbound variable %a"
          V.print var
      end
  | Uconst _ -> clam
  | Udirect_apply (label, args, dbg) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Udirect_apply (label, args, dbg)
  | Ugeneric_apply (func, args, dbg) ->
    let func = substitute_let_moveable is_let_moveable env func in
    let args = substitute_let_moveable_list is_let_moveable env args in
    Ugeneric_apply (func, args, dbg)
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with
            body = substitute_let_moveable is_let_moveable env ufunction.body;
          })
        functions
    in
    let variables_bound_by_the_closure =
      substitute_let_moveable_list is_let_moveable env
        variables_bound_by_the_closure
    in
    Uclosure (functions, variables_bound_by_the_closure)
  | Uoffset (clam, n) ->
    let clam = substitute_let_moveable is_let_moveable env clam in
    Uoffset (clam, n)
  | Ulet (let_kind, value_kind, var, def, body) ->
    let def = substitute_let_moveable is_let_moveable env def in
    if V.Set.mem (VP.var var) is_let_moveable then
      let env = V.Map.add (VP.var var) def env in
      let body = substitute_let_moveable is_let_moveable env body in
      (* If we are about to delete a [let] in debug mode, keep it for the
         debugger. *)
      (* CR-someday mshinwell: find out why some closure constructions were
         not leaving phantom lets behind after substitution. *)
      if not !Clflags.debug_full then
        body
      else
        match def with
        | Uconst const ->
          Uphantom_let (var, Some (Clambda.Uphantom_const const), body)
        | Uvar alias_of ->
          Uphantom_let (var, Some (Clambda.Uphantom_var alias_of), body)
        | _ ->
          Uphantom_let (var, None, body)
    else
      Ulet (let_kind, value_kind,
            var, def, substitute_let_moveable is_let_moveable env body)
  | Uphantom_let (var, defining_expr, body) ->
    let body = substitute_let_moveable is_let_moveable env body in
    Uphantom_let (var, defining_expr, body)
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (var, def) ->
          var, substitute_let_moveable is_let_moveable env def)
        defs
    in
    let body = substitute_let_moveable is_let_moveable env body in
    Uletrec (defs, body)
  | Uprim (prim, args, dbg) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Uprim (prim, args, dbg)
  | Uswitch (cond, sw, dbg) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let sw =
      { sw with
        us_actions_consts =
          substitute_let_moveable_array is_let_moveable env
            sw.us_actions_consts;
        us_actions_blocks =
          substitute_let_moveable_array is_let_moveable env
            sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw, dbg)
  | Ustringswitch (cond, branches, default) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let branches =
      List.map (fun (s, branch) ->
          s, substitute_let_moveable is_let_moveable env branch)
        branches
    in
    let default =
      Option.map (substitute_let_moveable is_let_moveable env) default
    in
    Ustringswitch (cond, branches, default)
  | Ustaticfail (n, args) ->
    let args = substitute_let_moveable_list is_let_moveable env args in
    Ustaticfail (n, args)
  | Ucatch (n, vars, body, handler) ->
    let body = substitute_let_moveable is_let_moveable env body in
    let handler = substitute_let_moveable is_let_moveable env handler in
    Ucatch (n, vars, body, handler)
  | Utrywith (body, var, handler) ->
    let body = substitute_let_moveable is_let_moveable env body in
    let handler = substitute_let_moveable is_let_moveable env handler in
    Utrywith (body, var, handler)
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let ifso = substitute_let_moveable is_let_moveable env ifso in
    let ifnot = substitute_let_moveable is_let_moveable env ifnot in
    Uifthenelse (cond, ifso, ifnot)
  | Usequence (e1, e2) ->
    let e1 = substitute_let_moveable is_let_moveable env e1 in
    let e2 = substitute_let_moveable is_let_moveable env e2 in
    Usequence (e1, e2)
  | Uwhile (cond, body) ->
    let cond = substitute_let_moveable is_let_moveable env cond in
    let body = substitute_let_moveable is_let_moveable env body in
    Uwhile (cond, body)
  | Ufor (var, low, high, direction, body) ->
    let low = substitute_let_moveable is_let_moveable env low in
    let high = substitute_let_moveable is_let_moveable env high in
    let body = substitute_let_moveable is_let_moveable env body in
    Ufor (var, low, high, direction, body)
  | Uassign (var, expr) ->
    let expr = substitute_let_moveable is_let_moveable env expr in
    Uassign (var, expr)
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = substitute_let_moveable is_let_moveable env e1 in
    let e2 = substitute_let_moveable is_let_moveable env e2 in
    let args = substitute_let_moveable_list is_let_moveable env args in
    Usend (kind, e1, e2, args, dbg)
  | Uunreachable ->
    Uunreachable

and substitute_let_moveable_list is_let_moveable env clams =
  List.map (substitute_let_moveable is_let_moveable env) clams

and substitute_let_moveable_array is_let_moveable env clams =
  Array.map (substitute_let_moveable is_let_moveable env) clams

(* We say that an expression is "moveable" iff it has neither effects nor
   coeffects.  (See semantics_of_primitives.mli.)
*)
type moveable = Fixed | Constant | Moveable

let both_moveable a b =
  match a, b with
  | Constant, Constant -> Constant
  | Constant, Moveable
  | Moveable, Constant
  | Moveable, Moveable -> Moveable
  | Constant, Fixed
  | Moveable, Fixed
  | Fixed, Constant
  | Fixed, Moveable
  | Fixed, Fixed -> Fixed

let primitive_moveable (prim : Clambda_primitives.primitive)
    (args : Clambda.ulambda list)
    (var_info : var_info) =
  match prim, args with
  | Pfield _, [Uconst (Uconst_ref (_, _))] ->
    (* CR-someday mshinwell: Actually, maybe this shouldn't be needed; these
       should have been simplified to [Read_symbol_field], which doesn't yield
       a Clambda let.  This might be fixed when Inline_and_simplify can
       turn Pfield into Read_symbol_field. *)
    (* Allow field access of symbols to be moveable.  (The comment in
       flambda.mli on [Read_symbol_field] may be helpful to the reader.) *)
    Moveable
  | Pfield _, [Uvar var] when V.Set.mem var var_info.closure_environment ->
    (* accesses to the function environment is coeffect free: this block
       is never mutated *)
    Moveable
  | _ ->
    match Semantics_of_primitives.for_primitive prim with
    | No_effects, No_coeffects -> Moveable
    | No_effects, Has_coeffects
    | Only_generative_effects, No_coeffects
    | Only_generative_effects, Has_coeffects
    | Arbitrary_effects, No_coeffects
    | Arbitrary_effects, Has_coeffects -> Fixed

type moveable_for_env = Constant | Moveable

(** Eliminate, through substitution, [let]-bindings of linear variables with
    moveable defining expressions. *)
let rec un_anf_and_moveable var_info env (clam : Clambda.ulambda)
      : Clambda.ulambda * moveable =
  match clam with
  | Uvar var ->
    begin match V.Map.find var env with
    | Constant, def -> def, Constant
    | Moveable, def -> def, Moveable
    | exception Not_found ->
      let moveable : moveable =
        if V.Set.mem var var_info.assigned then
          Fixed
        else
          Moveable
      in
      clam, moveable
    end
  | Uconst _ ->
    (* Constant closures are rewritten separately. *)
    clam, Constant
  | Udirect_apply (label, args, dbg) ->
    let args = un_anf_list var_info env args in
    Udirect_apply (label, args, dbg), Fixed
  | Ugeneric_apply (func, args, dbg) ->
    let func = un_anf var_info env func in
    let args = un_anf_list var_info env args in
    Ugeneric_apply (func, args, dbg), Fixed
  | Uclosure (functions, variables_bound_by_the_closure) ->
    let functions =
      List.map (fun (ufunction : Clambda.ufunction) ->
          { ufunction with
            body = un_anf var_info env ufunction.body;
          })
        functions
    in
    let variables_bound_by_the_closure =
      un_anf_list var_info env variables_bound_by_the_closure
    in
    Uclosure (functions, variables_bound_by_the_closure), Fixed
  | Uoffset (clam, n) ->
    let clam, moveable = un_anf_and_moveable var_info env clam in
    Uoffset (clam, n), both_moveable Moveable moveable
  | Ulet (_let_kind, _value_kind, var, def, Uvar var')
      when V.same (VP.var var) var' ->
    un_anf_and_moveable var_info env def
  | Ulet (let_kind, value_kind, var, def, body) ->
    let def, def_moveable = un_anf_and_moveable var_info env def in
    let is_linear = V.Set.mem (VP.var var) var_info.linear_let_bound_vars in
    let is_used = V.Set.mem (VP.var var) var_info.used_let_bound_vars in
    let is_assigned =
      V.Set.mem (VP.var var) var_info.assigned
    in
    let maybe_for_debugger (body, moveable) : Clambda.ulambda * moveable =
      if not !Clflags.debug_full then
        body, moveable
      else
        match def with
        | Uconst const ->
          Uphantom_let (var, Some (Clambda.Uphantom_const const),
            body), moveable
        | Uvar alias_of ->
          Uphantom_let (var, Some (Clambda.Uphantom_var alias_of), body),
            moveable
        | _ ->
          Uphantom_let (var, None, body), moveable
    in
    begin match def_moveable, is_linear, is_used, is_assigned with
    | (Constant | Moveable), _, false, _ ->
      (* A moveable expression that is never used may be eliminated.
         However, if in debug mode and the defining expression is
         appropriate, keep the let (as a phantom let) for the debugger. *)
      maybe_for_debugger (un_anf_and_moveable var_info env body)
    | Constant, _, true, false
    (* A constant expression bound to an unassigned variable can replace any
       occurrences of the variable.  The same comment as above concerning
       phantom lets applies. *)
    | Moveable, true, true, false  ->
      (* A moveable expression bound to a linear unassigned [V.t]
         may replace the single occurrence of the variable.  The same comment
         as above concerning phantom lets applies. *)
      let def_moveable =
        match def_moveable with
        | Moveable -> Moveable
        | Constant -> Constant
        | Fixed -> assert false
      in
      let env = V.Map.add (VP.var var) (def_moveable, def) env in
      maybe_for_debugger (un_anf_and_moveable var_info env body)
    | (Constant | Moveable), _, _, true
        (* Constant or Moveable but assigned. *)
    | Moveable, false, _, _
        (* Moveable but not used linearly. *)
    | Fixed, _, _, _ ->
      let body, body_moveable = un_anf_and_moveable var_info env body in
      Ulet (let_kind, value_kind, var, def, body),
      both_moveable def_moveable body_moveable
    end
  | Uphantom_let (var, defining_expr, body) ->
    let body, body_moveable = un_anf_and_moveable var_info env body in
    Uphantom_let (var, defining_expr, body), body_moveable
  | Uletrec (defs, body) ->
    let defs =
      List.map (fun (var, def) -> var, un_anf var_info env def) defs
    in
    let body = un_anf var_info env body in
    Uletrec (defs, body), Fixed
  | Uprim (prim, args, dbg) ->
    let args, args_moveable = un_anf_list_and_moveable var_info env args in
    let moveable =
      both_moveable args_moveable (primitive_moveable prim args var_info)
    in
    Uprim (prim, args, dbg), moveable
  | Uswitch (cond, sw, dbg) ->
    let cond = un_anf var_info env cond in
    let sw =
      { sw with
        us_actions_consts = un_anf_array var_info env sw.us_actions_consts;
        us_actions_blocks = un_anf_array var_info env sw.us_actions_blocks;
      }
    in
    Uswitch (cond, sw, dbg), Fixed
  | Ustringswitch (cond, branches, default) ->
    let cond = un_anf var_info env cond in
    let branches =
      List.map (fun (s, branch) -> s, un_anf var_info env branch)
        branches
    in
    let default = Option.map (un_anf var_info env) default in
    Ustringswitch (cond, branches, default), Fixed
  | Ustaticfail (n, args) ->
    let args = un_anf_list var_info env args in
    Ustaticfail (n, args), Fixed
  | Ucatch (n, vars, body, handler) ->
    let body = un_anf var_info env body in
    let handler = un_anf var_info env handler in
    Ucatch (n, vars, body, handler), Fixed
  | Utrywith (body, var, handler) ->
    let body = un_anf var_info env body in
    let handler = un_anf var_info env handler in
    Utrywith (body, var, handler), Fixed
  | Uifthenelse (cond, ifso, ifnot) ->
    let cond, cond_moveable = un_anf_and_moveable var_info env cond in
    let ifso, ifso_moveable = un_anf_and_moveable var_info env ifso in
    let ifnot, ifnot_moveable = un_anf_and_moveable var_info env ifnot in
    let moveable =
      both_moveable cond_moveable
        (both_moveable ifso_moveable ifnot_moveable)
    in
    Uifthenelse (cond, ifso, ifnot), moveable
  | Usequence (e1, e2) ->
    let e1 = un_anf var_info env e1 in
    let e2 = un_anf var_info env e2 in
    Usequence (e1, e2), Fixed
  | Uwhile (cond, body) ->
    let cond = un_anf var_info env cond in
    let body = un_anf var_info env body in
    Uwhile (cond, body), Fixed
  | Ufor (var, low, high, direction, body) ->
    let low = un_anf var_info env low in
    let high = un_anf var_info env high in
    let body = un_anf var_info env body in
    Ufor (var, low, high, direction, body), Fixed
  | Uassign (var, expr) ->
    let expr = un_anf var_info env expr in
    Uassign (var, expr), Fixed
  | Usend (kind, e1, e2, args, dbg) ->
    let e1 = un_anf var_info env e1 in
    let e2 = un_anf var_info env e2 in
    let args = un_anf_list var_info env args in
    Usend (kind, e1, e2, args, dbg), Fixed
  | Uunreachable ->
    Uunreachable, Fixed

and un_anf var_info env clam : Clambda.ulambda =
  let clam, _moveable = un_anf_and_moveable var_info env clam in
  clam

and un_anf_list_and_moveable var_info env clams
      : Clambda.ulambda list * moveable =
  List.fold_right (fun clam (l, acc_moveable) ->
      let clam, moveable = un_anf_and_moveable var_info env clam in
      clam :: l, both_moveable moveable acc_moveable)
    clams ([], (Moveable : moveable))

and un_anf_list var_info env clams : Clambda.ulambda list =
  let clams, _moveable = un_anf_list_and_moveable var_info env clams in
  clams

and un_anf_array var_info env clams : Clambda.ulambda array =
  Array.map (un_anf var_info env) clams

let apply ~what ~ppf_dump clam =
  let var_info = make_var_info clam in
  let let_bound_vars_that_can_be_moved =
    let_bound_vars_that_can_be_moved var_info clam
  in
  let clam =
    substitute_let_moveable let_bound_vars_that_can_be_moved
      V.Map.empty clam
  in
  let var_info = make_var_info clam in
  let clam = un_anf var_info V.Map.empty clam in
  if !Clflags.dump_clambda then begin
    Format.fprintf ppf_dump
      "@.un-anf (%a):@ %a@."
        Symbol.print what
        Printclambda.clambda clam
  end;
  clam
