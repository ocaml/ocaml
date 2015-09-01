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

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module IdentSet = Lambda.IdentSet

let name_expr = Flambda_utils.name_expr

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
}

(** Generate a wrapper ("stub") function that accepts a tuple argument and
    calls another function with (curried) arguments extracted in the obvious
    manner from the tuple. *)
let tupled_function_call_stub original_params tuplified_version
      : Flambda.function_declaration =
  let tuple_param =
    Variable.rename ~append:"tupled_stub_param" tuplified_version
  in
  let params = List.map (fun p -> Variable.rename p) original_params in
  let call : Flambda.t =
    Apply ({
        func = tuplified_version;
        args = params;
        (* CR mshinwell for mshinwell: investigate if there is some
           redundancy here (func is also tuplified_version) *)
        kind = Direct (Closure_id.wrap tuplified_version);
        dbg = Debuginfo.none;
      })
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : Flambda.named =
          Prim (Pfield pos, [tuple_param], Debuginfo.none)
        in
        pos + 1, Flambda.Let (param, lam, body))
      (0, call) params
  in
  Flambda.create_function_declaration ~params:[tuple_param]
    ~body ~stub:true ~dbg:Debuginfo.none

(** Propagate an [Lev_after] debugging event into an adjacent Flambda node. *)
let add_debug_info (ev : Lambda.lambda_event) (flam : Flambda.t)
      : Flambda.t =
  match ev.lev_kind with
  | Lev_after _ ->
    begin match flam with
    | Apply ap ->
      Apply { ap with dbg = Debuginfo.from_call ev}
(* XXX resurrect this
    | Prim (p, args, _dinfo, v) ->
      Prim (p, args, Debuginfo.from_call ev, v)
*)
    | Send { kind; meth; obj; args; dbg = _; } ->
      Send { kind; meth; obj; args; dbg = Debuginfo.from_call ev; }
(*
    | Fsequence (flam1, flam2, v) ->
      Fsequence (flam1, add_debug_info ev flam2, v)
*)
    | _ -> flam
    end
  | _ -> flam

let close_const (const : Lambda.structured_constant) : Flambda.named =
  match const with
  | Const_base (Const_int c) -> Const (Int c)
  | Const_base (Const_char c) -> Const (Char c)
  | Const_base (Const_string (s, _)) -> Allocated_const (String s)
  (* CR mshinwell: unsure about [float_of_string] (ditto below) *)
  | Const_base (Const_float c) -> Allocated_const (Float (float_of_string c))
  | Const_base (Const_int32 c) -> Allocated_const (Int32 c)
  | Const_base (Const_int64 c) -> Allocated_const (Int64 c)
  | Const_base (Const_nativeint c) -> Allocated_const (Nativeint c)
  | Const_pointer c -> Const (Const_pointer c)
  | Const_immstring c -> Allocated_const (Immstring c)
  | Const_float_array c ->
    Allocated_const (Float_array (List.map float_of_string c))
  | Const_block _ ->
    Misc.fatal_error "Const_block should have been eliminated before closure \
        conversion"

(* CR mshinwell: try to remove global state once we've shown this works *)
let imported_symbols = ref Symbol.Set.empty

let rec close t env (lam : Lambda.lambda) : Flambda.t =
  match lam with
  | Lvar id -> Var (Env.find_var env id)
  | Lconst cst -> name_expr (close_const cst)
  | Llet ((Strict | Alias | StrictOpt), id, defining_expr, body) ->
    let var = Variable.of_ident id in
    let defining_expr = close_let_bound_expression t var env defining_expr in
    let body = close t (Env.add_var env id var) body in
    Let (var, defining_expr, body)
  | Llet (Variable, id, defining_expr, body) ->
    let mut_var = Mutable_variable.of_ident id in
    let var = Variable.of_ident id in
    let defining_expr = close_let_bound_expression t var env defining_expr in
    let body = close t (Env.add_mutable_var env id mut_var) body in
    Let (var, defining_expr, Let_mutable (mut_var, var, body))
  | Lfunction { kind; params; body; } ->
    let closure_bound_var =
      let name =
        (* Name anonymous functions by their source location, if known. *)
        match body with
        | Levent (_, { lev_loc }) ->
          Format.asprintf "anon-fn[%a]" Location.print_compact lev_loc
        | _ -> "anon-fn"
      in
      Variable.create name
    in
    (* CR mshinwell: some of this is now very similar to the let rec case
       below *)
    let set_of_closures_var = Variable.create "set_of_closures" in
    let set_of_closures =
      let decl =
        Function_decl.create ~let_rec_ident:None ~closure_bound_var ~kind
          ~params ~body
      in
      close_functions t env (Function_decls.create [decl])
    in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Let (set_of_closures_var, set_of_closures,
      name_expr (Project_closure (project_closure)))
  | Lapply (funct, args, _loc) ->
    (* CR-someday mshinwell: the location should probably not be lost. *)
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:"apply_arg"
      ~create_body:(fun args ->
        let func = close t env funct in
        let func_var = Variable.create "apply_funct" in
        Let (func_var, Expr func,
          Apply ({
              func = func_var;
              args;
              kind = Indirect;
              dbg = Debuginfo.none;
            })))
  | Lletrec (defs, body) ->
    let env =
      List.fold_right (fun (id,  _) env ->
          Env.add_var env id (Variable.of_ident id))
        defs env
    in
    let function_declarations =
      (* Identify any bindings in the [let rec] that are functions.  These
         will be named after the corresponding identifier in the [let rec]. *)
      List.map (function
          | (let_rec_ident, Lambda.Lfunction { kind; params; body; }) ->
            let closure_bound_var = Variable.of_ident let_rec_ident in
            let function_declaration =
              Function_decl.create ~let_rec_ident:(Some let_rec_ident)
                ~closure_bound_var ~kind ~params ~body
            in
            Some function_declaration
          | _ -> None)
        defs
    in
    begin match Misc.some_if_all_elements_are_some function_declarations with
    | Some function_declarations ->
      (* When all the bindings are (syntactically) functions, we can
         eliminate the [let rec] construction, instead producing a normal
         [Let] that binds a set of closures containing all of the functions.
      *)
      let set_of_closures_var = Variable.create "set_of_closures" in
      let set_of_closures =
        close_functions t env (Function_decls.create function_declarations)
      in
      let body =
        List.fold_left (fun body decl ->
            let let_rec_ident = Function_decl.let_rec_ident decl in
            let closure_bound_var = Function_decl.closure_bound_var decl in
            let let_bound_var = Env.find_var env let_rec_ident in
            (* Inside the body of the [let], each function is referred to by
               a [Project_closure] expression, which projects from the set of
               closures. *)
            (Let (let_bound_var,
              Project_closure {
                set_of_closures = set_of_closures_var;
                closure_id = Closure_id.wrap closure_bound_var;
              },
              body) : Flambda.t))
          (close t env body) function_declarations
      in
      Let (set_of_closures_var, set_of_closures, body)
    | None ->
      (* If the condition above is not satisfied, we build a [Let_rec]
         expression; any functions bound by it will have their own
         individual closures. *)
      let defs =
        List.map (fun (id, def) ->
            let var = Env.find_var env id in
            var, close_let_bound_expression t ~let_rec_ident:id var env def)
          defs
      in
      Let_rec (defs, close t env body)
    end
  | Lsend (kind, meth, obj, args, loc) ->
    let meth_var = Variable.create "meth" in
    let obj_var = Variable.create "obj" in
    let dbg = Debuginfo.from_location Dinfo_call loc in
    Let (meth_var, Expr (close t env meth),
      Let (obj_var, Expr (close t env obj),
        Lift_code.lifting_helper (close_list t env args)
          ~evaluation_order:`Right_to_left
          ~name:"send_arg"
          ~create_body:(fun args ->
              Send { kind; meth = meth_var; obj = obj_var; args; dbg; })))
  | Lprim (Psequor, [arg1; arg2]) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_true = Variable.create "const_true" in
    let cond = Variable.create "cond_sequor" in
    Let (const_true, Const (Int 1),
      Let (cond, Expr arg1,
        If_then_else (cond, Var (const_true), arg2)))
  | Lprim (Psequand, [arg1; arg2]) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_false = Variable.create "const_true" in
    let cond = Variable.create "cond_sequand" in
    Let (const_false, Const (Int 0),
      Let (cond, Expr arg1,
        If_then_else (cond, arg2, Var (const_false))))
  | Lprim ((Psequand | Psequor), _) ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Lprim (Pidentity, [arg]) -> close t env arg
  | Lprim (Pdirapply loc, [funct; arg])
  | Lprim (Prevapply loc, [arg; funct]) ->
    close t env (Lambda.Lapply (funct, [arg], Lambda.mk_apply_info loc))
  | Lprim (Praise kind, [Levent (arg, event)]) ->
    let arg_var = Variable.create "raise_arg" in
    Let (arg_var, Expr (close t env arg),
      name_expr
        (Prim (Praise kind, [arg_var], Debuginfo.from_raise event)))
  | Lprim (Pfield pos, [Lprim (Pgetglobal id, [])])
    when Ident.same id t.current_unit_id ->
    let symbol = Env.find_global env pos in
    let sym_v = Variable.create ("access_global_" ^ string_of_int pos) in
    let result_v = Variable.create ("access_global_field_" ^ string_of_int pos) in
    Let (sym_v, Symbol symbol,
        Let (result_v, Prim(Pfield 0, [sym_v], Debuginfo.none),
            Var result_v))
  | Lprim (Psetfield (pos, _), [Lprim (Pgetglobal id, []); _]) ->
    assert (Ident.same id t.current_unit_id);
    Misc.fatal_errorf "[Psetfield %i] (to the current compilation unit) is \
        forbidden upon entry to the middle end" pos
  | Lprim (Pgetglobal id, []) when Ident.is_predef_exn id ->
    let symbol = t.symbol_for_global' id in
    imported_symbols := Symbol.Set.add symbol !imported_symbols;
    name_expr (Symbol symbol)
  | Lprim (Pgetglobal id, []) ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    imported_symbols := Symbol.Set.add symbol !imported_symbols;
    name_expr (Symbol symbol)
  | Lprim (p, args) ->
    (* One of the important consequences of the ANF-like representation
       here is that we obtain names corresponding to the components of
       blocks being made (with [Pmakeblock]).  This information can be used
       by the simplification pass to increase the likelihood of eliminating
       the allocation, since some field accesses can be tracked back to known
       field values. *)
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:"prim_arg"
      ~create_body:(fun args ->
        name_expr (Prim (p, args, Debuginfo.none)))
  | Lswitch (arg, sw) ->
    let scrutinee = Variable.create "switch" in
    let aux (i, lam) = i, close t env lam in
    let zero_to_n = Ext_types.IntSet.zero_to_n in
    Let (scrutinee, Expr (close t env arg),
      Switch (scrutinee,
        { numconsts = zero_to_n (sw.sw_numconsts - 1);
          consts = List.map aux sw.sw_consts;
          numblocks = zero_to_n (sw.sw_numblocks - 1);
          blocks = List.map aux sw.sw_blocks;
          failaction = Misc.may_map (close t env) sw.sw_failaction;
        }))
  | Lstringswitch (arg, sw, def) ->
    let scrutinee = Variable.create "string_switch" in
    Let (scrutinee, Expr (close t env arg),
      String_switch (scrutinee,
        List.map (fun (s, e) -> s, close t env e) sw,
        Misc.may_map (close t env) def))
  | Lstaticraise (i, args) ->
    Static_raise (Env.find_static_exception env i, close_list t env args)
  | Lstaticcatch (body, (i, ids), handler) ->
    let st_exn = Static_exception.create () in
    let env = Env.add_static_exception env i st_exn in
    let vars = List.map (Variable.of_ident) ids in
    Static_catch (st_exn, vars, close t env body,
      close t (Env.add_vars env ids vars) handler)
  | Ltrywith (body, id, handler) ->
    let var = Variable.of_ident id in
    Try_with (close t env body, var, close t (Env.add_var env id var) handler)
  | Lifthenelse (cond, ifso, ifnot) ->
    let cond = close t env cond in
    let cond_var = Variable.create "cond" in
    Let (cond_var, Expr cond,
      If_then_else (cond_var, close t env ifso, close t env ifnot))
  | Lsequence (lam1, lam2) ->
    let var = Variable.create "sequence" in
    let lam1 = Flambda.Expr (close t env lam1) in
    let lam2 = close t env lam2 in
    Let (var, lam1, lam2)
  | Lwhile (cond, body) -> While (close t env cond, close t env body)
  | Lfor (id, lo, hi, direction, body) ->
    let bound_var = Variable.of_ident id in
    let from_value = Variable.create "for_from" in
    let to_value = Variable.create "for_to" in
    let body = close t (Env.add_var env id bound_var) body in
    Let (from_value, Expr (close t env lo),
      Let (to_value, Expr (close t env hi),
        For { bound_var; from_value; to_value; direction; body; }))
  | Lassign (id, new_value) ->
    let being_assigned = Env.find_mutable_var env id in
    let new_value_var = Variable.create "new_value" in
    Let (new_value_var, Expr (close t env new_value),
      Assign { being_assigned; new_value = new_value_var; })
  | Levent (lam, ev) -> add_debug_info ev (close t env lam)
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if
       an identifier is.  Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression,
       or by completely removing it (replacing by unit). *)
    Misc.fatal_error "[Lifused] should have been removed by \
        [Simplif.simplify_lets]"

(** Perform closure conversion on a set of function declarations, returning a
    set of closures.  (The set will often only contain a single function;
    the only case where it cannot is for "let rec".) *)
and close_functions t external_env function_declarations : Flambda.named =
  let closure_env_without_parameters =
    Function_decls.closure_env_without_parameters
      external_env function_declarations
  in
  let all_free_idents = Function_decls.all_free_idents function_declarations in
  let close_one_function map decl =
    let body = Function_decl.body decl in
    let dbg =
      (* Move any debugging event that may exist at the start of the function
         body onto the function declaration itself. *)
      match body with
      | Levent (_, ({ lev_kind = Lev_function } as ev)) ->
        Debuginfo.from_call ev
      | _ -> Debuginfo.none
    in
    let params = Function_decl.params decl in
    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.used_idents]; the results of
       that renaming are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun id env -> Env.add_var env id (Variable.of_ident id))
        params closure_env_without_parameters
    in
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
    let stub, body =
      match Function_decl.primitive_wrapper decl with
      | None -> false, body
      | Some wrapper_body -> true, wrapper_body
    in
    let params = List.map (Env.find_var closure_env) params in
    let closure_bound_var = Function_decl.closure_bound_var decl in
    let body = close t closure_env body in
    let fun_decl =
      Flambda.create_function_declaration ~params ~body ~stub ~dbg
    in
    match Function_decl.kind decl with
    | Curried -> Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
      let tuplified_version = Variable.rename closure_bound_var in
      let generic_function_stub =
        tupled_function_call_stub params tuplified_version
      in
      Variable.Map.add tuplified_version fun_decl
        (Variable.Map.add closure_bound_var generic_function_stub map)
  in
  let function_decls : Flambda.function_declarations =
    { set_of_closures_id =
        Set_of_closures_id.create (Compilation_unit.get_current_exn ());
      funs =
        List.fold_left close_one_function Variable.Map.empty
          (Function_decls.to_list function_declarations);
      compilation_unit = Compilation_unit.get_current_exn ();
    }
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    let free_vars =
      IdentSet.fold (fun var map ->
          let internal_var =
            Env.find_var closure_env_without_parameters var
          in
          let external_var = Env.find_var external_env var in
          Variable.Map.add internal_var external_var map)
        all_free_idents Variable.Map.empty
    in
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args:Variable.Map.empty
  in
  Set_of_closures set_of_closures

and close_list t sb l = List.map (close t sb) l

and close_let_bound_expression t ?let_rec_ident let_bound_var env
      (lam : Lambda.lambda) : Flambda.named =
  match lam with
  | Lfunction { kind; params; body; } ->
    (* Ensure that [let] and [let rec]-bound functions have appropriate
       names. *)
    let closure_bound_var = Variable.rename let_bound_var in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~body
    in
    let set_of_closures_var = Variable.create "set_of_closures_var" in
    let set_of_closures = close_functions t env [decl] in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Expr (Let (set_of_closures_var, set_of_closures,
      name_expr (Project_closure (project_closure))))
  | lam -> Expr (close t env lam)

(* type initialisation = *)
(*   | Field_initialisation of Symbol.t *)
(*   | Effect *)

(* (\* This deconstruction has to handle initialisation of patterns that *)
(*    are not simply compiled. *)

(*    {[ *)
(*      type t = *)
(*        | A of (int * int * int) *)
(*        | B of int * int *)

(*      let (A (a, _, b) | B (b, a)) = A (1, 2, 3) *)
(*    ]} *)

(* (let (match/1012 = (makeblock 0 (makeblock 0 1 2 3))) *)
(*   (catch *)
(*     (switch* match/1012 *)
(*      case tag 0: *)
(*       (let (match/1017 =a (field 0 match/1012)) *)
(*         (exit 2 (field 0 match/1017) (field 2 match/1017))) *)
(*      case tag 1: (exit 2 (field 1 match/1012) (field 0 match/1012))) *)
(*    with (2 a/1005 b/1006) *)
(*     (seq (setfield_imm 0 (global Pattern!) a/1005) *)
(*       (setfield_imm 1 (global Pattern!) b/1006)))) *)

(* *\) *)

(* let _build_initialization *)
(*     compilation_unit t *)
(*     (env, initialisations) *)
(*     (lam, (init:Deconstruct_initialisation.initialisation)) = *)
(*   match init with *)
(*   | Field_initialisations pos -> begin *)
(*       match pos with *)
(*       | [] -> assert false *)
(*       | [pos] -> *)
(*         (\* If there is a single initialised global, we can assign it directly to a symbol *\) *)
(*         let linkage_name = Linkage_name.create ("init_" ^ string_of_int pos) in *)
(*         let symbol = Symbol.create compilation_unit linkage_name in *)
(*         let elt = close t env lam, Field_initialisation symbol in *)
(*         let env = Env.add_global env pos symbol in *)
(*         env, elt :: initialisations *)
(*       | _ -> *)
(*         (\* If there are multiple initialised globals, the value *)
(*            returned by the expression is a block containing the *)
(*            results of the different fields. So the original result is *)
(*            assigned to a symbol [block_symbol], that is accessed to *)
(*            initialize the real globals. *)
(*         *\) *)
(*         let block_linkage_name = *)
(*           Linkage_name.create ("init_intermediate_" ^ *)
(*                                (String.concat "_" (List.map string_of_int pos))) *)
(*         in *)
(*         let block_symbol = Symbol.create compilation_unit block_linkage_name in *)
(*         let block_elt = close t env lam, Field_initialisation block_symbol in *)
(*         let elts = *)
(*           List.mapi *)
(*             (fun field pos -> *)
(*                let linkage_name = Linkage_name.create ("init_" ^ string_of_int pos) in *)
(*                let symbol = Symbol.create compilation_unit linkage_name in *)
(*                let sym_v = Variable.create ("access_global_" ^ string_of_int pos) in *)
(*                let result_v = Variable.create ("tmp_" ^ string_of_int pos) in *)
(*                let value_v = Variable.create ("tmp_" ^ string_of_int pos) in *)
(*                let expr = *)
(*                  Flambda.Let *)
(*                    (Immutable, sym_v, Symbol block_symbol, *)
(*                     Let (result_v, Prim(Pfield 0, [sym_v], Debuginfo.none), *)
(*                         Let (value_v, Prim(Pfield field, [result_v], Debuginfo.none), *)
(*                             Var value_v))) *)
(*                in *)
(*                (pos, symbol), (expr, Field_initialisation symbol)) *)
(*             (\* Deconstruct_initialisation build a block that is sorted *)
(*                following the order of the fields in the final global *)
(*                module block *\) *)
(*             (List.sort compare pos) *)
(*         in *)
(*         let env = *)
(*           List.fold_left (fun env ((pos, symbol), _) -> *)
(*               Env.add_global env pos symbol) *)
(*             env elts *)
(*         in *)
(*         let elts = List.map snd elts in *)
(*         env,  elts @ block_elt :: initialisations *)
(*     end *)
(*   | Effect -> *)
(*     let elt = close t env lam, Effect in *)
(*     env, elt :: initialisations *)

(* let lambda_to_flambda ~backend ~module_ident ~exported_fields module_initializer = *)
(*   imported_symbols := Symbol.Set.empty; *)
(*   let module Backend = (val backend : Backend_intf.S) in *)
(*   let compilation_unit = Compilation_unit.get_current_exn () in *)
(*   let t = *)
(*     { current_unit_id = *)
(*         Compilation_unit.get_persistent_ident compilation_unit; *)
(*       symbol_for_global' = Backend.symbol_for_global'; *)
(*     } *)
(*   in *)
(*   let initialisations = *)
(*     Deconstruct_initialisation.split_module_initialization module_initializer *)
(*   in *)
(*   let module_symbol = Backend.symbol_for_global' module_ident in *)
(*   let env, initialisations = *)
(*     List.fold_left (build_initialization compilation_unit t) (Env.empty, []) initialisations *)
(*   in *)
(*   (\* The global module block is built by accessing the fields of the *)
(*      all the introduced symbols.*\) *)
(*   let fields = *)
(*     Array.init exported_fields (fun i -> *)
(*         match Env.find_global env i with *)
(*         | symbol -> *)
(*           let sym_v = Variable.create "global_symbol" in *)
(*           let field_v = Variable.create "global_field" in *)
(*           Flambda.Let( *)
(*             Immutable,sym_v,Symbol symbol, *)
(*             Let(Immutable,field_v,Prim (Pfield 0,[sym_v],Debuginfo.none), *)
(*                 Var field_v)) *)
(*         | exception Not_found -> *)
(*           let field_v = Variable.create "global_unused" in *)
(*           Flambda.Let(Immutable,field_v,Const (Int 0),Var field_v)) *)
(*   in *)
(*   let module_initializer = *)
(*     Flambda.Initialize_symbol( *)
(*       module_symbol, *)
(*       Tag.create_exn 0, *)
(*       Array.to_list fields, *)
(*       Flambda.End module_symbol) *)
(*   in *)
(*   let module_initializer = *)
(*     List.fold_left (fun program -> function *)
(*         | flam, Field_initialisation symbol -> *)
(*           Flambda.Initialize_symbol (symbol, Tag.create_exn 0, [flam], program) *)
(*         | flam, Effect -> *)
(*           Flambda.Effect (flam, program) *)
(*       ) *)
(*       module_initializer initialisations *)
(*   in *)
(*   Symbol.Set.fold (fun sym expr -> Flambda.Import_symbol (sym, expr)) *)
(*     !imported_symbols *)
(*     module_initializer *)

let lambda_to_flambda ~backend ~module_ident ~size lam =
  imported_symbols := Symbol.Set.empty;
  let module Backend = (val backend : Backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let t =
    { current_unit_id =
        Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
    }
  in
  let module_symbol = Backend.symbol_for_global' module_ident in

  let block_symbol =
    let linkage_name = Linkage_name.create ("module_as_block") in
    Symbol.create compilation_unit linkage_name
  in


  (* The global module block is built by accessing the fields of the
     all the introduced symbols.*)
  let fields =
    Array.init size (fun pos ->
        let sym_v = Variable.create ("block_symbol_" ^ string_of_int pos) in
        let result_v = Variable.create ("block_symbol_get_" ^ string_of_int pos) in
        let value_v = Variable.create ("block_symbol_get_field_" ^ string_of_int pos) in
        Flambda.Let
          (sym_v, Symbol block_symbol,
           Let (result_v, Prim(Pfield 0, [sym_v], Debuginfo.none),
               Let (value_v, Prim(Pfield pos, [result_v], Debuginfo.none),
                   Var value_v))))
  in
  let module_initializer =
    Flambda.Initialize_symbol(
      block_symbol,
      Tag.create_exn 0,
      [close t Env.empty lam],
      Flambda.Initialize_symbol(
        module_symbol,
        Tag.create_exn 0,
        Array.to_list fields,
        Flambda.End module_symbol))
  in

  Symbol.Set.fold (fun sym expr -> Flambda.Import_symbol (sym, expr))
    !imported_symbols
    module_initializer
