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
        (* CR-someday mshinwell for mshinwell: investigate if there is some
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
        pos + 1, Flambda.create_let param lam body)
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
      Apply { ap with dbg = Debuginfo.from_call ev; }
    | Let let_expr ->
      Flambda.map_defining_expr_of_let let_expr ~f:(function
        | Prim (p, args, _dinfo) ->
          Prim (p, args, Debuginfo.from_call ev)
        | defining_expr -> defining_expr)
    | Send { kind; meth; obj; args; dbg = _; } ->
      Send { kind; meth; obj; args; dbg = Debuginfo.from_call ev; }
    | _ -> flam
    end
  | _ -> flam

let close_const (const : Lambda.structured_constant) : Flambda.named * string =
  match const with
  | Const_base (Const_int c) -> Const (Int c), "int"
  | Const_base (Const_char c) -> Const (Char c), "char"
  | Const_base (Const_string (s, _)) -> Allocated_const (String s), "string"
  (* CR mshinwell: unsure about [float_of_string] (ditto below) *)
  | Const_base (Const_float c) -> Allocated_const (Float (float_of_string c)), "float"
  | Const_base (Const_int32 c) -> Allocated_const (Int32 c), "int32"
  | Const_base (Const_int64 c) -> Allocated_const (Int64 c), "int64"
  | Const_base (Const_nativeint c) -> Allocated_const (Nativeint c), "nativeint"
  | Const_pointer c -> Const (Const_pointer c), "pointer"
  | Const_immstring c -> Allocated_const (Immstring c), "immstring"
  | Const_float_array c ->
    Allocated_const (Float_array (List.map float_of_string c)), "float_array"
  | Const_block _ ->
    Misc.fatal_error "Const_block should have been eliminated before closure \
        conversion"

(* CR-someday mshinwell: remove global state *)
let imported_symbols = ref Symbol.Set.empty

let rec close t env (lam : Lambda.lambda) : Flambda.t =
  match lam with
  | Lvar id ->
    begin match Env.find_var_exn env id with
    | var -> Var var
    | exception Not_found ->
      match Env.find_mutable_var_exn env id with
      | mut_var -> name_expr (Read_mutable mut_var)
      | exception Not_found ->
        Misc.fatal_errorf "Closure_conversion.close: unbound identifier %a"
          Ident.print id
    end
  | Lconst cst ->
    let cst, name = close_const cst in
    name_expr cst ~name:("const_" ^ name)
  | Llet ((Strict | Alias | StrictOpt), id, defining_expr, body) ->
    let var = Variable.of_ident id in
    let defining_expr = close_let_bound_expression t var env defining_expr in
    let body = close t (Env.add_var env id var) body in
    Flambda.create_let var defining_expr body
  | Llet (Variable, id, defining_expr, body) ->
    let mut_var = Mutable_variable.of_ident id in
    let var = Variable.of_ident id in
    let defining_expr = close_let_bound_expression t var env defining_expr in
    let body = close t (Env.add_mutable_var env id mut_var) body in
    Flambda.create_let var defining_expr (Let_mutable (mut_var, var, body))
  | Lfunction { kind; params; body; } ->
    let name =
      (* Name anonymous functions by their source location, if known. *)
      match body with
      | Levent (_, { lev_loc }) ->
        Format.asprintf "anon-fn[%a]" Location.print_compact lev_loc
      | _ -> "anon-fn"
    in
    let closure_bound_var =
      Variable.create name
    in
    (* CR-soon mshinwell: some of this is now very similar to the let rec case
       below *)
    let set_of_closures_var = Variable.create ("set_of_closures_" ^ name) in
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
    Flambda.create_let set_of_closures_var set_of_closures
      (name_expr (Project_closure (project_closure))
        ~name:("project_closure_" ^ name))
  | Lapply (funct, args, loc) ->
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:"apply_arg"
      ~create_body:(fun args ->
        let func = close t env funct in
        let func_var = Variable.create "apply_funct" in
        Flambda.create_let func_var (Expr func)
          (Apply ({
              func = func_var;
              args;
              kind = Indirect;
              dbg = Debuginfo.from_location Dinfo_call loc.apply_loc;
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
      let name =
        String.concat "_and_"
          (List.map (fun (id, _) -> Ident.unique_name id) defs)
      in
      let set_of_closures_var = Variable.create name in
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
            (Flambda.create_let let_bound_var
              (Project_closure {
                set_of_closures = set_of_closures_var;
                closure_id = Closure_id.wrap closure_bound_var;
              })
              body))
          (close t env body) function_declarations
      in
      Flambda.create_let set_of_closures_var set_of_closures body
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
    Flambda.create_let meth_var (Expr (close t env meth))
      (Flambda.create_let obj_var (Expr (close t env obj))
        (Lift_code.lifting_helper (close_list t env args)
          ~evaluation_order:`Right_to_left
          ~name:"send_arg"
          ~create_body:(fun args ->
              Send { kind; meth = meth_var; obj = obj_var; args; dbg; })))
  | Lprim (Psequor, [arg1; arg2]) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_true = Variable.create "const_true" in
    let cond = Variable.create "cond_sequor" in
    Flambda.create_let const_true (Const (Int 1))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, Var (const_true), arg2)))
  | Lprim (Psequand, [arg1; arg2]) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_false = Variable.create "const_true" in
    let cond = Variable.create "cond_sequand" in
    Flambda.create_let const_false (Const (Int 0))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, arg2, Var (const_false))))
  | Lprim ((Psequand | Psequor), _) ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Lprim (Pidentity, [arg]) -> close t env arg
  | Lprim (Pdirapply loc, [funct; arg])
  | Lprim (Prevapply loc, [arg; funct]) ->
    close t env (Lambda.Lapply (funct, [arg], Lambda.mk_apply_info loc))
  | Lprim (Praise kind, [Levent (arg, event)]) ->
    let arg_var = Variable.create "raise_arg" in
    Flambda.create_let arg_var (Expr (close t env arg))
      (name_expr
        (Prim (Praise kind, [arg_var], Debuginfo.from_raise event))
        ~name:"raise")
  | Lprim (Pfield pos, [Lprim (Pgetglobal id, [])])
    when Ident.same id t.current_unit_id ->
    (* CR mshinwell: is this case now redundant? *)
    let symbol = Env.find_global env pos in
    let sym_v = Variable.create ("access_global_" ^ string_of_int pos) in
    let result_v = Variable.create ("access_global_field_" ^ string_of_int pos) in
    Flambda.create_let sym_v (Symbol symbol)
      (Flambda.create_let result_v (Prim(Pfield 0, [sym_v], Debuginfo.none))
        (Var result_v))
  | Lprim (Psetfield (pos, _), [Lprim (Pgetglobal id, []); _]) ->
    assert (Ident.same id t.current_unit_id);
    Misc.fatal_errorf "[Psetfield %i] (to the current compilation unit) is \
        forbidden upon entry to the middle end" pos
  | Lprim (Pgetglobal id, []) when Ident.is_predef_exn id ->
    let symbol = t.symbol_for_global' id in
    imported_symbols := Symbol.Set.add symbol !imported_symbols;
    name_expr (Symbol symbol) ~name:"predef_exn"
  | Lprim (Pgetglobal id, []) ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    imported_symbols := Symbol.Set.add symbol !imported_symbols;
    name_expr (Symbol symbol) ~name:"Pgetglobal"
  | Lprim (p, args) ->
    (* One of the important consequences of the ANF-like representation
       here is that we obtain names corresponding to the components of
       blocks being made (with [Pmakeblock]).  This information can be used
       by the simplification pass to increase the likelihood of eliminating
       the allocation, since some field accesses can be tracked back to known
       field values. *)
    let name = Printlambda.string_of_primitive p in
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:(name ^ "_arg")
      ~create_body:(fun args ->
        name_expr (Prim (p, args, Debuginfo.none)) ~name)
  | Lswitch (arg, sw) ->
    let scrutinee = Variable.create "switch" in
    let aux (i, lam) = i, close t env lam in
    let zero_to_n = Ext_types.IntSet.zero_to_n in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (Switch (scrutinee,
        { numconsts = zero_to_n (sw.sw_numconsts - 1);
          consts = List.map aux sw.sw_consts;
          numblocks = zero_to_n (sw.sw_numblocks - 1);
          blocks = List.map aux sw.sw_blocks;
          failaction = Misc.may_map (close t env) sw.sw_failaction;
        }))
  | Lstringswitch (arg, sw, def) ->
    let scrutinee = Variable.create "string_switch" in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (String_switch (scrutinee,
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
    Flambda.create_let cond_var (Expr cond)
      (If_then_else (cond_var, close t env ifso, close t env ifnot))
  | Lsequence (lam1, lam2) ->
    let var = Variable.create "sequence" in
    let lam1 = Flambda.Expr (close t env lam1) in
    let lam2 = close t env lam2 in
    Flambda.create_let var lam1 lam2
  | Lwhile (cond, body) -> While (close t env cond, close t env body)
  | Lfor (id, lo, hi, direction, body) ->
    let bound_var = Variable.of_ident id in
    let from_value = Variable.create "for_from" in
    let to_value = Variable.create "for_to" in
    let body = close t (Env.add_var env id bound_var) body in
    Flambda.create_let from_value (Expr (close t env lo))
      (Flambda.create_let to_value (Expr (close t env hi))
        (For { bound_var; from_value; to_value; direction; body; }))
  | Lassign (id, new_value) ->
    let being_assigned =
      match Env.find_mutable_var_exn env id with
      | being_assigned -> being_assigned
      | exception Not_found ->
        Misc.fatal_errorf "Closure_conversion.close: unbound mutable \
            variable %s in assignment"
          (Ident.unique_name id)
    in
    let new_value_var = Variable.create "new_value" in
    Flambda.create_let new_value_var (Expr (close t env new_value))
      (Assign { being_assigned; new_value = new_value_var; })
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
  let function_decls =
    Flambda.create_function_declarations
      ~set_of_closures_id:
        (Set_of_closures_id.create (Compilation_unit.get_current_exn ()))
      ~funs:
        (List.fold_left close_one_function Variable.Map.empty
          (Function_decls.to_list function_declarations))
      ~compilation_unit:(Compilation_unit.get_current_exn ())
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
    let closure_bound_var =
      Variable.rename let_bound_var ~append:"_closure"
    in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~body
    in
    let set_of_closures_var =
      Variable.rename let_bound_var ~append:"_set_of_closures"
    in
    let set_of_closures = close_functions t env [decl] in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Expr (Flambda.create_let set_of_closures_var set_of_closures
      (name_expr (Project_closure (project_closure))
        ~name:(Variable.unique_name let_bound_var)))
  | lam -> Expr (close t env lam)

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
    let linkage_name = Linkage_name.create "module_as_block" in
    Symbol.create compilation_unit linkage_name
  in
  (* The global module block is built by accessing the fields of all the
     introduced symbols. *)
  (* CR mshinwell for mshinwell: Add a comment describing how modules are
     compiled. *)
  let fields =
    Array.init size (fun pos ->
      let pos_str = string_of_int pos in
      let sym_v = Variable.create ("block_symbol_" ^ pos_str) in
      let result_v = Variable.create ("block_symbol_get_" ^ pos_str) in
      let value_v = Variable.create ("block_symbol_get_field_" ^ pos_str) in
      Flambda.create_let
        sym_v (Symbol block_symbol)
         (Flambda.create_let result_v
            (Prim (Pfield 0, [sym_v], Debuginfo.none))
            (Flambda.create_let value_v
              (Prim (Pfield pos, [result_v], Debuginfo.none))
              (Var value_v))))
  in
  let module_initializer : Flambda.program =
    Initialize_symbol (
      block_symbol,
      Tag.create_exn 0,
      [close t Env.empty lam],
      Initialize_symbol (
        module_symbol,
        Tag.create_exn 0,
        Array.to_list fields,
        End module_symbol))
  in
  Symbol.Set.fold (fun sym expr -> Flambda.Import_symbol (sym, expr))
    !imported_symbols
    module_initializer
