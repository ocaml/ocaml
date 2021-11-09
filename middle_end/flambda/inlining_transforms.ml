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

[@@@ocaml.warning "+a-4-9-30-40-41-42-66"]
open! Int_replace_polymorphic_compare

module B = Inlining_cost.Benefit
module E = Inline_and_simplify_aux.Env
module R = Inline_and_simplify_aux.Result
module A = Simple_value_approx

let new_var name =
  Variable.create name
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

(** Fold over all variables bound by the given closure, which is bound to the
    variable [lhs_of_application], and corresponds to the given
    [function_decls].  Each variable bound by the closure is passed to the
    user-specified function as an [Flambda.named] value that projects the
    variable from its closure. *)
let fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~bound_variables ~init ~f =
  Variable.Set.fold (fun var acc ->
      let expr : Flambda.named =
        Project_var {
          closure = lhs_of_application;
          closure_id = closure_id_being_applied;
          var = Var_within_closure.wrap var;
        }
      in
      f ~acc ~var ~expr)
    bound_variables
    init

let set_inline_attribute_on_all_apply body inline specialise =
  Flambda_iterators.map_toplevel_expr (function
      | Apply apply -> Apply { apply with inline; specialise }
      | expr -> expr)
    body

(** Assign fresh names for a function's parameters and rewrite the body to
    use these new names. *)
let copy_of_function's_body_with_freshened_params env
      ~(function_decl : A.function_declaration)
      ~(function_body : A.function_body) =
  let params = function_decl.params in
  let param_vars = Parameter.List.vars params in
  (* We cannot avoid the substitution in the case where we are inlining
     inside the function itself.  This can happen in two ways: either
     (a) we are inlining the function itself directly inside its declaration;
     or (b) we are inlining the function into an already-inlined copy.
     For (a) we cannot short-cut the substitution by freshening since the
     original [params] may still be referenced; for (b) we cannot do it
     either since the freshening may already be renaming the parameters for
     the first inlining of the function. *)
  if E.does_not_bind env param_vars
    && E.does_not_freshen env param_vars
  then
    params, function_body.body
  else
    let freshened_params = List.map (fun p -> Parameter.rename p) params in
    let subst =
      Variable.Map.of_list
        (List.combine param_vars (Parameter.List.vars freshened_params))
    in
    let body = Flambda_utils.toplevel_substitution subst function_body.body in
    freshened_params, body

(* CR-soon mshinwell: Add a note somewhere to explain why "bound by the closure"
   does not include the function identifiers for other functions in the same
   set of closures.
   mshinwell: The terminology may be used inconsistently. *)

(** Inline a function by copying its body into a context where it becomes
    closed.  That is to say, we bind the free variables of the body
    (= "variables bound by the closure"), and any function identifiers
    introduced by the corresponding set of closures. *)
let inline_by_copying_function_body ~env ~r
      ~lhs_of_application
      ~(inline_requested : Lambda.inline_attribute)
      ~(specialise_requested : Lambda.specialise_attribute)
      ~closure_id_being_applied
      ~(function_decl : A.function_declaration)
      ~(function_body : A.function_body)
      ~fun_vars
      ~args ~dbg ~simplify =
  assert (E.mem env lhs_of_application);
  assert (List.for_all (E.mem env) args);
  let r =
    if function_body.stub then r
    else R.map_benefit r B.remove_call
  in
  let freshened_params, body =
    copy_of_function's_body_with_freshened_params env
      ~function_decl ~function_body
  in
  let body =
    let default_inline =
      Lambda.equal_inline_attribute inline_requested Default_inline
    in
    let default_specialise =
      Lambda.equal_specialise_attribute specialise_requested Default_specialise
    in
    if function_body.stub
    && ((not default_inline) || (not default_specialise)) then
      (* When the function inlined function is a stub, the annotation
         is reported to the function applications inside the stub.
         This allows reporting the annotation to the application the
         original programmer really intended: the stub is not visible
         in the source. *)
      set_inline_attribute_on_all_apply body
        inline_requested specialise_requested
    else
      body
  in
  let bindings_for_params_to_args =
    (* Bind the function's parameters to the arguments from the call site. *)
    let args = List.map (fun arg -> Flambda.Expr (Var arg)) args in
    Flambda_utils.bind ~body
      ~bindings:(List.combine (Parameter.List.vars freshened_params) args)
  in
  (* Add bindings for the variables bound by the closure. *)
  let bindings_for_vars_bound_by_closure_and_params_to_args =
    let bound_variables =
      let params = Parameter.Set.vars function_decl.params in
      Variable.Set.diff
        (Variable.Set.diff function_body.free_variables params)
        fun_vars
    in
    fold_over_projections_of_vars_bound_by_closure ~closure_id_being_applied
      ~lhs_of_application ~bound_variables ~init:bindings_for_params_to_args
      ~f:(fun ~acc:body ~var ~expr -> Flambda.create_let var expr body)
  in
  (* Add bindings for variables corresponding to the functions introduced by
     the whole set of closures.  Each such variable will be bound to a closure;
     each such closure is in turn produced by moving from the closure being
     applied to another closure in the same set.
  *)
  let expr =
    Variable.Set.fold (fun another_closure_in_the_same_set expr ->
      let used =
        Variable.Set.mem another_closure_in_the_same_set
           function_body.free_variables
      in
      if used then
        Flambda.create_let another_closure_in_the_same_set
          (Move_within_set_of_closures {
            closure = lhs_of_application;
            start_from = closure_id_being_applied;
            move_to = Closure_id.wrap another_closure_in_the_same_set;
          })
          expr
      else expr)
      fun_vars
      bindings_for_vars_bound_by_closure_and_params_to_args
  in
  let env = E.set_never_inline env in
  let env = E.activate_freshening env in
  let env = E.set_inline_debuginfo ~dbg env in
  simplify env r expr

type state = {
  old_inside_to_new_inside : Variable.t Variable.Map.t;
    (* Map from old inner vars to new inner vars *)
  old_outside_to_new_outside : Variable.t Variable.Map.t;
    (* Map from old outer vars to new outer vars *)
  old_params_to_new_outside : Variable.t Variable.Map.t;
    (* Map from old parameters to new outer vars. These are params
       that should be specialised if they are copied to the new set of
       closures. *)
  old_fun_var_to_new_fun_var : Variable.t Variable.Map.t;
    (* Map from old fun vars to new fun vars. These are the functions
       that will be copied into the new set of closures *)
  let_bindings : (Variable.t * Flambda.named) list;
    (* Let bindings that will surround the definition of the new set
       of closures *)
  to_copy : Variable.t list;
    (* List of functions that still need to be copied to the new set
       of closures *)
  new_funs : Flambda.function_declaration Variable.Map.t;
    (* The function declarations for the new set of closures *)
  new_free_vars_with_old_projections : Flambda.specialised_to Variable.Map.t;
    (* The free variables for the new set of closures, but the projection
       fields still point to old free variables. *)
  new_specialised_args_with_old_projections :
    Flambda.specialised_to Variable.Map.t;
    (* The specialised parameters for the new set of closures, but the
       projection fields still point to old specialised parameters. *)
}

let empty_state =
  { to_copy = [];
    old_inside_to_new_inside   = Variable.Map.empty;
    old_outside_to_new_outside = Variable.Map.empty;
    old_params_to_new_outside = Variable.Map.empty;
    old_fun_var_to_new_fun_var = Variable.Map.empty;
    let_bindings = [];
    new_funs = Variable.Map.empty;
    new_free_vars_with_old_projections = Variable.Map.empty;
    new_specialised_args_with_old_projections = Variable.Map.empty; }

(* Add let bindings for the free vars in the set_of_closures and
   add them to [old_outside_to_new_outside] *)
let bind_free_vars ~lhs_of_application ~closure_id_being_applied
      ~state ~free_vars =
  Variable.Map.fold
    (fun free_var (spec : Flambda.specialised_to) state ->
       let var_clos = new_var Internal_variable_names.from_closure in
       let expr : Flambda.named =
         Project_var {
           closure = lhs_of_application;
           closure_id = closure_id_being_applied;
           var = Var_within_closure.wrap free_var;
         }
       in
       let let_bindings = (var_clos, expr) :: state.let_bindings in
       let old_outside_to_new_outside =
         Variable.Map.add spec.var var_clos state.old_outside_to_new_outside
       in
       { state with let_bindings; old_outside_to_new_outside })
    free_vars state

(* For arguments of specialised parameters:
   - Add them to [old_outside_to_new_outside]
   - Add them and their invariant aliases to [old_params_to_new_outside]
   For other arguments that are also worth specialising:
   - Add them and their invariant aliases to [old_params_to_new_outside] *)
let register_arguments ~specialised_args ~invariant_params
      ~state ~params ~args ~args_approxs =
  let rec loop ~state ~params ~args ~args_approxs =
    match params, args, args_approxs with
    | [], [], [] -> state
    | param :: params, arg :: args, arg_approx :: args_approxs -> begin
        let param = Parameter.var param in
        let worth_specialising, old_outside_to_new_outside =
          match Variable.Map.find_opt param specialised_args with
          | Some (spec : Flambda.specialised_to) ->
              let old_outside_to_new_outside =
                Variable.Map.add spec.var arg state.old_outside_to_new_outside
              in
              true, old_outside_to_new_outside
          | None ->
              let worth_specialising =
                A.useful arg_approx
                && Variable.Map.mem param (Lazy.force invariant_params)
              in
              worth_specialising, state.old_outside_to_new_outside
        in
        let old_params_to_new_outside =
          if worth_specialising then begin
            let old_params_to_new_outside =
              Variable.Map.add param arg state.old_params_to_new_outside
            in
            match Variable.Map.find_opt param (Lazy.force invariant_params) with
            | Some set ->
                Variable.Set.fold
                  (fun elem acc -> Variable.Map.add elem arg acc)
                  set old_params_to_new_outside
            | None ->
                old_params_to_new_outside
          end else begin
            state.old_params_to_new_outside
          end
        in
        let state =
          { state with old_outside_to_new_outside; old_params_to_new_outside }
        in
        loop ~state ~params ~args ~args_approxs
      end
    | _, _, _ -> assert false
  in
  loop ~state ~params ~args ~args_approxs

(* Add an old parameter to [old_inside_to_new_inside]. If it appears in
   [old_params_to_new_outside] then also add it to the new specialised args. *)
let add_param ~specialised_args ~state ~param =
  let param = Parameter.var param in
  let new_param = Variable.rename param in
  let old_inside_to_new_inside =
    Variable.Map.add param new_param state.old_inside_to_new_inside
  in
  let new_specialised_args_with_old_projections =
    match Variable.Map.find_opt param specialised_args with
    | Some (spec : Flambda.specialised_to) ->
        let new_outside_var =
          Variable.Map.find spec.var state.old_outside_to_new_outside
        in
        let new_spec : Flambda.specialised_to =
          { spec with var = new_outside_var }
        in
        Variable.Map.add new_param new_spec
          state.new_specialised_args_with_old_projections
    | None -> begin
        match Variable.Map.find_opt param state.old_params_to_new_outside with
        | None -> state.new_specialised_args_with_old_projections
        | Some new_outside_var ->
            let new_spec : Flambda.specialised_to =
              { var = new_outside_var; projection = None }
            in
            Variable.Map.add new_param new_spec
              state.new_specialised_args_with_old_projections
      end
  in
  let state =
    { state with old_inside_to_new_inside;
                 new_specialised_args_with_old_projections }
  in
  state, Parameter.wrap new_param

(* Add a let binding for an old fun_var, add it to the new free variables, and
   add it to [old_inside_to_new_inside] *)
let add_fun_var ~lhs_of_application ~closure_id_being_applied ~state ~fun_var =
  if Variable.Map.mem fun_var state.old_inside_to_new_inside then state
  else begin
    let inside_var = Variable.rename fun_var in
    let outside_var = Variable.create Internal_variable_names.closure in
    let expr =
      Flambda.Move_within_set_of_closures
        { closure    = lhs_of_application;
          start_from = closure_id_being_applied;
          move_to    = Closure_id.wrap fun_var; }
    in
    let let_bindings = (outside_var, expr) :: state.let_bindings in
    let spec : Flambda.specialised_to =
      { var = outside_var; projection = None; }
    in
    let new_free_vars_with_old_projections =
      Variable.Map.add inside_var spec state.new_free_vars_with_old_projections
    in
    let old_inside_to_new_inside =
      Variable.Map.add fun_var inside_var state.old_inside_to_new_inside
    in
    { state with
        old_inside_to_new_inside; let_bindings;
        new_free_vars_with_old_projections }
  end

(* Add an old free_var to the new free variables and add it to
   [old_inside_to_new_inside]. *)
let add_free_var ~free_vars ~state ~free_var =
  if Variable.Map.mem free_var state.old_inside_to_new_inside then state
  else begin
    let spec : Flambda.specialised_to = Variable.Map.find free_var free_vars in
    let outside_var = spec.var in
    let new_outside_var =
      Variable.Map.find outside_var state.old_outside_to_new_outside
    in
    let new_spec : Flambda.specialised_to =
      { spec with var = new_outside_var }
    in
    let new_inside_var = Variable.rename free_var in
    let new_free_vars_with_old_projections =
      Variable.Map.add new_inside_var new_spec
        state.new_free_vars_with_old_projections
    in
    let old_inside_to_new_inside =
      Variable.Map.add free_var new_inside_var state.old_inside_to_new_inside
    in
    { state with old_inside_to_new_inside; new_free_vars_with_old_projections }
  end

(* Add a function to the new set of closures iff:
   1) All it's specialised parameters are available in
      [old_outside_to_new_outside]
   2) At least one more parameter will become specialised *)
let add_function ~specialised_args ~state ~fun_var ~function_decl =
  match function_decl.A.function_body with
  | None -> None
  | Some _ -> begin
    let rec loop worth_specialising = function
      | [] -> worth_specialising
      | param :: params -> begin
          let param = Parameter.var param in
          match Variable.Map.find_opt param specialised_args with
          | Some (spec : Flambda.specialised_to) ->
              Variable.Map.mem spec.var state.old_outside_to_new_outside
              && loop worth_specialising params
          | None ->
              let worth_specialising =
                worth_specialising
                || Variable.Map.mem param state.old_params_to_new_outside
              in
              loop worth_specialising params
        end
    in
    let worth_specialising = loop false function_decl.A.params in
    if not worth_specialising then None
    else begin
      let new_fun_var = Variable.rename fun_var in
      let old_fun_var_to_new_fun_var =
        Variable.Map.add fun_var new_fun_var state.old_fun_var_to_new_fun_var
      in
      let to_copy = fun_var :: state.to_copy in
      let state = { state with old_fun_var_to_new_fun_var; to_copy } in
      Some (state, new_fun_var)
    end
  end

(* Lookup a function in the new set of closures, trying to add it if
   necessary. *)
let lookup_function ~specialised_args ~state ~fun_var ~function_decl =
  match Variable.Map.find_opt fun_var state.old_fun_var_to_new_fun_var with
  | Some new_fun_var -> Some (state, new_fun_var)
  | None -> add_function ~specialised_args ~state ~fun_var ~function_decl

(* A direct call to a function in the new set of closures can be specialised
   if all the function's newly specialised parameters are passed arguments
   that are specialised to the same outside variable *)
let specialisable_call ~specialised_args ~state ~args ~params =
  List.for_all2
    (fun arg param ->
       let param = Parameter.var param in
       if Variable.Map.mem param specialised_args then true
       else begin
         let old_params_to_new_outside = state.old_params_to_new_outside in
         match Variable.Map.find_opt param old_params_to_new_outside with
         | None -> true
         | Some outside_var -> begin
             match Variable.Map.find_opt arg old_params_to_new_outside with
             | Some outside_var' ->
               Variable.equal outside_var outside_var'
             | None -> false
           end
       end)
    args params

(* Rewrite a call iff:
   1) It is to a function in the old set of closures that can be specialised
   2) All the newly specialised parameters of that function are passed values
      known to be equal to their new specialisation. *)
let rec rewrite_direct_call ~specialised_args ~funs ~direct_call_surrogates
      ~state ~closure_id ~(apply : Flambda.apply) =
  match Closure_id.Map.find_opt closure_id direct_call_surrogates with
  | Some closure_id ->
      rewrite_direct_call ~specialised_args ~funs ~direct_call_surrogates
        ~state ~closure_id ~apply
  | None -> begin
      let fun_var = Closure_id.unwrap closure_id in
      match Variable.Map.find_opt fun_var funs with
      | None -> None
      | Some function_decl -> begin
          match
            lookup_function ~specialised_args ~state ~fun_var ~function_decl
          with
          | None -> None
          | Some (state, new_fun_var) -> begin
              let args = apply.args in
              let params = function_decl.A.params in
              let specialisable =
                specialisable_call ~specialised_args ~state ~args ~params
              in
              if not specialisable then None
              else begin
                let kind = Flambda.Direct (Closure_id.wrap new_fun_var) in
                let apply = { apply with func = new_fun_var; kind } in
                Some (state, Flambda.Apply apply)
              end
            end
        end
    end

(* Rewrite the body a function declaration for use in the new set of
   closures. *)
let rewrite_function ~lhs_of_application ~closure_id_being_applied
      ~direct_call_surrogates ~specialised_args ~free_vars ~funs
      ~state fun_var =
  let function_decl : A.function_declaration =
    Variable.Map.find fun_var funs
  in
  let function_body =
    match function_decl.function_body with
    | None -> assert false
    | Some function_body -> function_body
  in
  let new_fun_var =
    Variable.Map.find fun_var state.old_fun_var_to_new_fun_var
  in
  let state, params =
    List.fold_right
      (fun param (state, params) ->
         let state, param = add_param ~specialised_args ~state ~param in
         (state, param :: params))
      function_decl.params (state, [])
  in
  let state =
    Variable.Set.fold
      (fun var state ->
         if Variable.Map.mem var funs then
           add_fun_var ~lhs_of_application ~closure_id_being_applied
             ~state ~fun_var:var
         else if Variable.Map.mem var free_vars then
           add_free_var ~free_vars ~state ~free_var:var
         else
           state)
      function_body.free_variables state
  in
  let state_ref = ref state in
  let body =
    Flambda_iterators.map_toplevel_expr
      (fun (expr : Flambda.t) ->
         match expr with
         | Apply ({ kind = Direct closure_id } as apply) -> begin
             match
               rewrite_direct_call ~specialised_args ~funs
                 ~direct_call_surrogates ~state:!state_ref ~closure_id ~apply
             with
             | None -> expr
             | Some (state, expr) ->
                 state_ref := state;
                 expr
           end
         | _ -> expr)
      function_body.body
  in
  let body =
    Flambda_utils.toplevel_substitution state.old_inside_to_new_inside body
  in
  let new_function_decl =
    Flambda.create_function_declaration
      ~params ~body
      ~stub:function_body.stub
      ~dbg:function_body.dbg
      ~inline:function_body.inline
      ~specialise:function_body.specialise
      ~is_a_functor:function_body.is_a_functor
      ~closure_origin:(Closure_origin.create (Closure_id.wrap new_fun_var))
      ~poll:function_body.poll
  in
  let new_funs =
    Variable.Map.add new_fun_var new_function_decl state.new_funs
  in
  let state = { !state_ref with new_funs } in
  state

let update_projections ~state projections =
  let old_to_new = state.old_inside_to_new_inside in
  Variable.Map.map
    (fun (spec_to : Flambda.specialised_to) ->
       let projection : Projection.t option =
         match spec_to.projection with
         | None -> None
         | Some (Project_var proj) -> begin
             match Variable.Map.find_opt proj.closure old_to_new with
             | None -> None
             | Some closure ->
                 let proj = { proj with closure } in
                 Some (Projection.Project_var proj)
           end
         | Some (Project_closure proj) -> begin
             match Variable.Map.find_opt proj.set_of_closures old_to_new with
             | None -> None
             | Some set_of_closures ->
                 let proj = { proj with set_of_closures } in
                 Some (Projection.Project_closure proj)
           end
         | Some (Move_within_set_of_closures proj) -> begin
             match Variable.Map.find_opt proj.closure old_to_new with
             | None -> None
             | Some closure ->
                 let proj = { proj with closure } in
                 Some (Projection.Move_within_set_of_closures proj)
           end
         | Some (Field (index, var)) -> begin
             match Variable.Map.find_opt var old_to_new with
             | None -> None
             | Some var -> Some (Projection.Field(index, var))
           end
      in
      { spec_to with projection })
    projections

let inline_by_copying_function_declaration
    ~(env : Inline_and_simplify_aux.Env.t)
    ~(r : Inline_and_simplify_aux.Result.t)
    ~(function_decls : A.function_declarations)
    ~(lhs_of_application : Variable.t)
    ~(inline_requested : Lambda.inline_attribute)
    ~(closure_id_being_applied : Closure_id.t)
    ~(function_decl : A.function_declaration)
    ~(args : Variable.t list)
    ~(args_approxs : A.t list)
    ~(invariant_params : Variable.Set.t Variable.Map.t lazy_t)
    ~(specialised_args : Flambda.specialised_to Variable.Map.t)
    ~(free_vars : Flambda.specialised_to Variable.Map.t)
    ~(direct_call_surrogates : Closure_id.t Closure_id.Map.t)
    ~(dbg : Debuginfo.t)
    ~(simplify : Inlining_decision_intf.simplify) =
  let state = empty_state in
  let state =
    bind_free_vars ~lhs_of_application ~closure_id_being_applied
      ~state ~free_vars
  in
  let params = function_decl.params in
  let state =
    register_arguments ~specialised_args ~invariant_params
      ~state ~params ~args ~args_approxs
  in
  let fun_var = Closure_id.unwrap closure_id_being_applied in
  match add_function ~specialised_args ~state ~fun_var ~function_decl with
  | None -> None
  | Some (state, new_fun_var) -> begin
      let funs = function_decls.funs in
      let rec loop state =
        match state.to_copy with
        | [] -> state
        | next :: rest ->
          let state = { state with to_copy = rest } in
          let state =
            rewrite_function ~lhs_of_application ~closure_id_being_applied
              ~direct_call_surrogates ~specialised_args ~free_vars ~funs
              ~state next
          in
          loop state
      in
      let state = loop state in
      let closure_id = Closure_id.wrap new_fun_var in
      let function_decls =
        Flambda.create_function_declarations_with_origin
          ~funs:state.new_funs
          ~set_of_closures_origin:function_decls.set_of_closures_origin
          ~is_classic_mode:function_decls.is_classic_mode
      in
      let free_vars =
        update_projections ~state
          state.new_free_vars_with_old_projections
      in
      let specialised_args =
        update_projections ~state
          state.new_specialised_args_with_old_projections
      in
      let direct_call_surrogates = Variable.Map.empty in
      let set_of_closures =
        Flambda.create_set_of_closures ~function_decls
          ~free_vars ~specialised_args ~direct_call_surrogates
      in
      let closure_var = new_var Internal_variable_names.dup_func in
      let set_of_closures_var =
        new_var Internal_variable_names.dup_set_of_closures
      in
      let project : Flambda.project_closure =
        {set_of_closures = set_of_closures_var; closure_id}
      in
      let apply : Flambda.apply =
        { func = closure_var; args; kind = Direct closure_id; dbg;
          inline = inline_requested; specialise = Default_specialise; }
      in
      let body =
        Flambda.create_let
          set_of_closures_var (Set_of_closures set_of_closures)
          (Flambda.create_let closure_var (Project_closure project)
             (Apply apply))
      in
      let expr = Flambda_utils.bind ~body ~bindings:state.let_bindings in
      let env = E.activate_freshening (E.set_never_inline env) in
      Some (simplify env r expr)
    end
