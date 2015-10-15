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

type for_one_or_more_units = {
  fun_offset_table : int Closure_id.Map.t;
  fv_offset_table : int Var_within_closure.Map.t;
  closures : Flambda.function_declarations Closure_id.Map.t;
  constant_sets_of_closures : Set_of_closures_id.Set.t;
}

type t = {
  current_unit : for_one_or_more_units;
  imported_units : for_one_or_more_units;
}

type ('a, 'b) declaration_position =
  | Current_unit of 'a
  | Imported_unit of 'b
  | Not_declared

let get_fun_offset t closure_id =
  let fun_offset_table =
    if Closure_id.in_compilation_unit (Compilenv.current_unit ()) closure_id
    then t.current_unit.fun_offset_table
    else t.imported_units.fun_offset_table
  in
  try Closure_id.Map.find closure_id fun_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for closure %a"
      Closure_id.print closure_id

let get_fv_offset t var_within_closure =
  let fv_offset_table =
    if Var_within_closure.in_compilation_unit (Compilenv.current_unit ())
        var_within_closure
    then t.current_unit.fv_offset_table
    else t.imported_units.fv_offset_table
  in
  try Var_within_closure.Map.find var_within_closure fv_offset_table
  with Not_found ->
    Misc.fatal_errorf "Flambda_to_clambda: missing offset for variable %a"
      Var_within_closure.print var_within_closure

let function_declaration_position t closure_id =
  try
    Current_unit (Closure_id.Map.find closure_id t.current_unit.closures)
  with Not_found ->
    try
      Imported_unit (Closure_id.Map.find closure_id t.imported_units.closures)
    with Not_found -> Not_declared

let is_function_constant t closure_id =
  match function_declaration_position t closure_id with
  | Current_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id
      t.current_unit.constant_sets_of_closures
  | Imported_unit { set_of_closures_id } ->
    Set_of_closures_id.Set.mem set_of_closures_id
      t.imported_units.constant_sets_of_closures
  | Not_declared ->
    Misc.fatal_errorf "Flambda_to_clambda: missing closure %a"
      Closure_id.print closure_id

module Env : sig
  type t

  val empty : t

  val add_subst : t -> Variable.t -> Clambda.ulambda -> t
  val find_subst_exn : t -> Variable.t -> Clambda.ulambda

  val add_fresh_ident : t -> Variable.t -> Ident.t * t
  val ident_for_var_exn : t -> Variable.t -> Ident.t

  val add_fresh_mutable_ident : t -> Mutable_variable.t -> Ident.t * t
  val ident_for_mutable_var_exn : t -> Mutable_variable.t -> Ident.t

  val add_allocated_const : t -> Symbol.t -> Allocated_const.t -> t
  val allocated_const_for_symbol : t -> Symbol.t -> Allocated_const.t option

  val keep_only_symbols : t -> t
end = struct
  type t =
    { subst : Clambda.ulambda Variable.Map.t;
      var : Ident.t Variable.Map.t;
      mutable_var : Ident.t Mutable_variable.Map.t;
      toplevel : bool;
      allocated_constant_for_symbol : Allocated_const.t Symbol.Map.t;
    }

  let empty =
    { subst = Variable.Map.empty;
      var = Variable.Map.empty;
      mutable_var = Mutable_variable.Map.empty;
      toplevel = false;
      allocated_constant_for_symbol = Symbol.Map.empty;
    }

  let add_subst t id subst =
    { t with subst = Variable.Map.add id subst t.subst }

  let find_subst_exn t id = Variable.Map.find id t.subst

  let ident_for_var_exn t id = Variable.Map.find id t.var

  let add_fresh_ident t var =
    let id = Variable.unique_ident var in
    id, { t with var = Variable.Map.add var id t.var }

  let ident_for_mutable_var_exn t mut_var =
    Mutable_variable.Map.find mut_var t.mutable_var

  let add_fresh_mutable_ident t mut_var =
    let id = Mutable_variable.unique_ident mut_var in
    let mutable_var = Mutable_variable.Map.add mut_var id t.mutable_var in
    id, { t with mutable_var; }

  let add_allocated_const t sym cons =
    { t with
      allocated_constant_for_symbol =
        Symbol.Map.add sym cons t.allocated_constant_for_symbol;
    }

  let allocated_const_for_symbol t sym =
    try
      Some (Symbol.Map.find sym t.allocated_constant_for_symbol)
    with Not_found -> None

  let keep_only_symbols t =
    { empty with
      allocated_constant_for_symbol = t.allocated_constant_for_symbol; }

end

let subst_var env var : Clambda.ulambda =
  try Env.find_subst_exn env var
  with Not_found ->
    try Uvar (Env.ident_for_var_exn env var)
    with Not_found ->
      Misc.fatal_errorf "Flambda_to_clambda: unbound variable %a@.%s@."
        Variable.print var
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 400000))

let subst_vars env vars = List.map (subst_var env) vars

let build_uoffset ulam offset : Clambda.ulambda =
  if offset = 0 then ulam
  else Uoffset (ulam, offset)

let to_uconst_symbol env symbol : Clambda.ustructured_constant option =
  match Env.allocated_const_for_symbol env symbol with
  | Some (Float f) ->
    Some (Clambda.Uconst_float f)
  | Some (Int32 i) ->
    Some (Clambda.Uconst_int32 i)
  | Some (Int64 i) ->
    Some (Clambda.Uconst_int64 i)
  | Some (Nativeint i) ->
    Some (Clambda.Uconst_nativeint i)
  | None | Some _ ->
    None

let to_clambda_symbol env sym : Clambda.ulambda =
  let lbl = Linkage_name.to_string (Symbol.label sym) in
  (* XCR pchambart: The constant should contains details about the variable to
     allow cmmgen to unbox.
     mshinwell: What are we going to do here?
     pchambart: done *)
  Uconst (Uconst_ref (lbl, to_uconst_symbol env sym))

let to_clambda_const (const : Flambda.constant_defining_value_block_field)
      : Clambda.uconstant =
  match const with
  | Symbol s ->
    let lbl = Linkage_name.to_string (Symbol.label s) in
    (* XCR pchambart: The constant should contain details about the variable to
       allow cmmgen to unbox
       pchambart: In that case, it doesn't matter. Those are only fields of
       constant blocks. They are not amenable to unboxing. *)
    Uconst_ref (lbl, None)
  | Const (Int i) -> Uconst_int i
  | Const (Char c) -> Uconst_int (Char.code c)
  | Const (Const_pointer i) -> Uconst_ptr i

let rec to_clambda t env (flam : Flambda.t) : Clambda.ulambda =
  match flam with
  | Var var -> subst_var env var
  | Let { var; defining_expr; body; _ } ->
    let id, env_body = Env.add_fresh_ident env var in
    Ulet (id, to_clambda_named t env var defining_expr,
      to_clambda t env_body body)
  | Let_mutable (mut_var, var, body) ->
    let id, env_body = Env.add_fresh_mutable_ident env mut_var in
    let def = subst_var env var in
    Ulet (id, def, to_clambda t env_body body)
  | Let_rec (defs, body) ->
    let env, defs =
      List.fold_right (fun (var, def) (env, defs) ->
          let id, env = Env.add_fresh_ident env var in
          env, (id, var, def) :: defs)
        defs (env, [])
    in
    let defs =
      List.map (fun (id, var, def) -> id, to_clambda_named t env var def) defs
    in
    Uletrec (defs, to_clambda t env body)
  | Apply { func; args; kind = Direct direct_func; dbg = dbg } ->
    to_clambda_direct_apply t func args direct_func dbg env
  | Apply { func; args; kind = Indirect; dbg = dbg } ->
    (* CR mshinwell for mshinwell: improve this comment *)
    (* The closure parameter of the function is added by cmmgen, but
       it already appears in the list of parameters of the clambda
       function for generic calls. Notice that for direct calls it is
       added here. *)
    Ugeneric_apply (subst_var env func, subst_vars env args, dbg)
  | Switch (arg, sw) ->
    let aux () : Clambda.ulambda =
      let const_index, const_actions =
        to_clambda_switch t env sw.consts sw.numconsts sw.failaction
      in
      let block_index, block_actions =
        to_clambda_switch t env sw.blocks sw.numblocks sw.failaction
      in
      Uswitch (subst_var env arg,
        { us_index_consts = const_index;
          us_actions_consts = const_actions;
          us_index_blocks = block_index;
          us_actions_blocks = block_actions;
        })
    in
    let simple_expr (flam : Flambda.t) =
      match flam with
      | Var _ -> true
      | _ -> false
    in
    (* Check that the [failaction] may be duplicated.  If this is not the
       case, share it through a static raise / static catch. *)
    (* CR pchambart for pchambart: This is overly simplified. We should verify
       that this does not generates too bad code. If it the case, handle some
       let cases.
    *)
    begin match sw.failaction with
    | None -> aux ()
    | Some (Static_raise (_, args)) when List.for_all simple_expr args ->
      aux ()
    | Some failaction ->
      let exn = Static_exception.create () in
      let sw =
        { sw with
          failaction = Some (Flambda.Static_raise (exn, []));
        }
      in
      let expr : Flambda.t =
        Static_catch (exn, [], Switch (arg, sw), failaction)
      in
      to_clambda t env expr
    end
  | String_switch (arg, sw, def) ->
    let arg = subst_var env arg in
    let sw = List.map (fun (s, e) -> s, to_clambda t env e) sw in
    let def = Misc.may_map (to_clambda t env) def in
    Ustringswitch (arg, sw, def)
  | Static_raise (static_exn, args) ->
    Ustaticfail (Static_exception.to_int static_exn,
      List.map (to_clambda t env) args)
  | Static_catch (static_exn, vars, body, handler) ->
    let env_handler, ids =
      List.fold_right (fun var (env, ids) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: ids)
        vars (env, [])
    in
    Ucatch (Static_exception.to_int static_exn, ids,
      to_clambda t env body, to_clambda t env_handler handler)
  | Try_with (body, var, handler) ->
    let id, env_handler = Env.add_fresh_ident env var in
    Utrywith (to_clambda t env body, id, to_clambda t env_handler handler)
  | If_then_else (arg, ifso, ifnot) ->
    Uifthenelse (subst_var env arg, to_clambda t env ifso,
      to_clambda t env ifnot)
  | While (cond, body) ->
    Uwhile (to_clambda t env cond, to_clambda t env body)
  | For { bound_var; from_value; to_value; direction; body } ->
    let id, env_body = Env.add_fresh_ident env bound_var in
    Ufor (id, subst_var env from_value, subst_var env to_value,
      direction, to_clambda t env_body body)
  | Assign { being_assigned; new_value } ->
    let id =
      try Env.ident_for_mutable_var_exn env being_assigned
      with Not_found ->
        Misc.fatal_errorf "Unbound mutable variable %a in [Assign]: %a"
          Mutable_variable.print being_assigned
          Flambda.print flam
    in
    Uassign (id, subst_var env new_value)
  | Send { kind; meth; obj; args; dbg } ->
    Usend (kind, subst_var env meth, subst_var env obj,
      subst_vars env args, dbg)
  | Proved_unreachable ->
    (* CR pchambart: shouldn't be executable, maybe build something else e.g.
       Uprim(Praise, [Uconst (Uconst_pointer 0, None)], Debuginfo.none) *)
    Uunreachable

and to_clambda_named t env var (named : Flambda.named) : Clambda.ulambda =
  match named with
  | Symbol sym -> to_clambda_symbol env sym
  | Const (Const_pointer n) -> Uconst (Uconst_ptr n)
  | Const (Int n) -> Uconst (Uconst_int n)
  | Const (Char c) -> Uconst (Uconst_int (Char.code c))
  | Allocated_const _ ->
    Misc.fatal_errorf "[Allocated_const] should have been lifted to a \
        [Let_symbol] construction before [Clambdagen]: %a = %a"
      Variable.print var
      Flambda.print_named named
  | Read_mutable mut_var ->
    begin try Uvar (Env.ident_for_mutable_var_exn env mut_var)
    with Not_found ->
      Misc.fatal_errorf "Unbound mutable variable %a in [Read_mutable]: %a"
        Mutable_variable.print mut_var
        Flambda.print_named named
    end
  | Read_symbol_field (symbol, field) ->
    Uprim (Pfield field, [to_clambda_symbol env symbol], Debuginfo.none)
  | Set_of_closures set_of_closures ->
    to_clambda_set_of_closures t env set_of_closures
  | Project_closure { set_of_closures; closure_id } ->
    (* CR mshinwell for pchambart: I don't understand how this comment
       relates to this code.  Can you explain? *)
    (* compilation of let rec in cmmgen assumes
       that a closure is not offseted (Cmmgen.expr_size) *)
    build_uoffset (subst_var env set_of_closures) (get_fun_offset t closure_id)
  | Move_within_set_of_closures { closure; start_from; move_to } ->
    build_uoffset (subst_var env closure)
      ((get_fun_offset t move_to) - (get_fun_offset t start_from))
  | Project_var { closure; var; closure_id } ->
    let ulam = subst_var env closure in
    let fun_offset = get_fun_offset t closure_id in
    let var_offset = get_fv_offset t var in
    let pos = var_offset - fun_offset in
    Uprim (Pfield pos, [ulam], Debuginfo.none)
  (* CR mshinwell: these next two cases are probably now redundant.  Delete the
     primitives too *)
  | Prim (Pgetglobalfield (id, index), _, dbg) ->
    let ident = Ident.create_persistent (Compilenv.symbol_for_global id) in
    Uprim (Pfield index, [Clambda.Uprim (Pgetglobal ident, [], dbg)], dbg)
  | Prim (Psetglobalfield index, [arg], dbg) ->
    let ident = Ident.create_persistent (Compilenv.make_symbol None) in
    Uprim (Psetfield (index, false), [
        Clambda.Uprim (Pgetglobal ident, [], dbg);
        subst_var env arg;
      ], dbg)
  | Prim (p, args, dbg) -> Uprim (p, subst_vars env args, dbg)
  | Expr expr -> to_clambda t env expr

and to_clambda_switch t env cases num_keys default =
  let num_keys =
    if Ext_types.Int.Set.cardinal num_keys = 0 then 0
    else Ext_types.Int.Set.max_elt num_keys + 1
  in
  let index = Array.make num_keys 0 in
  let store = Flambda_utils.Switch_storer.mk_store () in
  begin match default with
  | Some def when List.length cases < num_keys -> ignore (store.act_store def)
  | _ -> ()
  end;
  List.iter (fun (key, lam) -> index.(key) <- store.act_store lam) cases;
  let actions = Array.map (to_clambda t env) (store.act_get ()) in
  match actions with
  | [| |] -> [| |], [| |]  (* May happen when [default] is [None]. *)
  | _ -> index, actions

and to_clambda_direct_apply t func args direct_func dbg env : Clambda.ulambda =
  let closed = is_function_constant t direct_func in
  let label = Compilenv.function_label direct_func in
  let uargs =
    let uargs = subst_vars env args in
    (* CR mshinwell: improve comment.  Should we check [func] too? *)
    (* If the function is closed, the function expression is always a
       variable, so it is ok to drop it. Note that it means that
       some Let can be dead. The un-anf pass should get rid of it *)
    if closed then uargs else uargs @ [subst_var env func]
  in
  Udirect_apply (label, uargs, dbg)

(* CR mshinwell for mshinwell: improve comment *)
(* Make the substitutions for variables bound by the closure:
   the variables bound are the functions inside the closure and
   the free variables of the functions.

   For instance the closure for a code like

     let rec fun_a x =
       if x <= 0 then 0 else fun_b (x-1) v1
     and fun_b x y =
       if x <= 0 then 0 else v1 + v2 + y + fun_a (x-1)

   will be represented in memory as:

     [ closure header; fun_a;
       1; infix header; fun caml_curry_2;
       2; fun_b; v1; v2 ]

   fun_a and fun_b will take an additional parameter 'env' to
   access their closure.  It will be shifted such that in the body
   of a function the env parameter points to its code
   pointer. i.e. in fun_b it will be shifted by 3 words.

   Hence accessing to v1 in the body of fun_a is accessing to the
   6th field of 'env' and in the body of fun_b it is the 1st
   field.
*)
and to_clambda_set_of_closures t env
      (({ function_decls; free_vars } : Flambda.set_of_closures)
        as set_of_closures) : Clambda.ulambda =
  let all_functions = Variable.Map.bindings function_decls.funs in
  let env_var = Ident.create "env" in
  let to_clambda_function
        (closure_id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    let closure_id = Closure_id.wrap closure_id in
    let fun_offset =
      Closure_id.Map.find closure_id t.current_unit.fun_offset_table
    in
    let env =
      (* Inside the body of the function, we cannot access variables
         declared outside, so start with a clean environment. *)
      let env = Env.empty in
      (* Add the Clambda expressions for the free variables of the function
         to the environment. *)
      let add_env_free_variable id _ env =
        let var_offset =
          try
            Var_within_closure.Map.find
              (Var_within_closure.wrap id) t.current_unit.fv_offset_table
          with Not_found ->
            Misc.fatal_errorf "Clambda.to_clambda_set_of_closures: offset for \
                free variable %a is unknown.  Set of closures: %a"
              Variable.print id
              Flambda.print_set_of_closures set_of_closures
        in
        let pos = var_offset - fun_offset in
        Env.add_subst env id
          (Uprim (Pfield pos, [Clambda.Uvar env_var], Debuginfo.none))
      in
      let env = Variable.Map.fold add_env_free_variable free_vars env in
      (* Add the Clambda expressions for all functions defined in the current
         set of closures to the environment.  The various functions may be
         retrieved by moving within the runtime closure, starting from the
         current function's closure. *)
      let add_env_function pos env (id, _) =
        let offset =
          Closure_id.Map.find (Closure_id.wrap id)
            t.current_unit.fun_offset_table
        in
        let exp : Clambda.ulambda = Uoffset (Uvar env_var, offset - pos) in
        Env.add_subst env id exp
      in
      List.fold_left (add_env_function fun_offset) env all_functions
    in
    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label closure_id;
      arity = Flambda_utils.function_arity function_decl;
      params = params @ [env_var];
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
    }
  in
  let funs = List.map to_clambda_function all_functions in
  let free_vars =
    Variable.Map.bindings (Variable.Map.map (subst_var env) free_vars)
  in
  Uclosure (funs, List.map snd free_vars)

and to_clambda_closed_set_of_closures t env symbol
      ({ function_decls; } : Flambda.set_of_closures)
      : Clambda.ustructured_constant =
  let functions = Variable.Map.bindings function_decls.funs in
  let to_clambda_function (id, (function_decl : Flambda.function_declaration))
        : Clambda.ufunction =
    (* All that we need in the environment, for translating one closure from
       a closed set of closures, is the substitutions for variables bound to
       the various closures in the set.  Such closures will always be
       referenced via symbols. *)
    let env =
      List.fold_left (fun env (var, _) ->
          let closure_id = Closure_id.wrap var in
          let symbol = Compilenv.closure_symbol closure_id in
          Env.add_subst env var (to_clambda_symbol env symbol))
        (Env.keep_only_symbols env)
        functions
    in
    let env_body, params =
      List.fold_right (fun var (env, params) ->
          let id, env = Env.add_fresh_ident env var in
          env, id :: params)
        function_decl.params (env, [])
    in
    { label = Compilenv.function_label (Closure_id.wrap id);
      arity = Flambda_utils.function_arity function_decl;
      params;
      body = to_clambda t env_body function_decl.body;
      dbg = function_decl.dbg;
    }
  in
  let ufunct = List.map to_clambda_function functions in
  let closure_lbl = Linkage_name.to_string (Symbol.label symbol) in
  Uconst_closure (ufunct, closure_lbl, [])

let to_clambda_allocated_constant (const : Allocated_const.t)
      : Clambda.ustructured_constant =
  match const with
  | Float f -> Uconst_float f
  | Int32 i -> Uconst_int32 i
  | Int64 i -> Uconst_int64 i
  | Nativeint i -> Uconst_nativeint i
  | Immstring s | String s -> Uconst_string s
  | Float_array a -> Uconst_float_array a

let to_clambda_initialize_symbol t env symbol fields : Clambda.ulambda =
  let fields =
    List.mapi (fun index expr -> index, to_clambda t env expr) fields
  in
  let build_setfield (index, field) : Clambda.ulambda =
    (* This [Psetfield] can affect a pointer, but since we are initializing
       a toplevel symbol, it is safe not to use [caml_modify].  (Moreover, we
       must not use [caml_modify], since the location being modified is
       outside the heap.) *)
   Uprim (Psetfield (index, false), [to_clambda_symbol env symbol; field],
      Debuginfo.none)
  in
  match fields with
  | [] -> Uconst (Uconst_ptr 0)
  | h :: t ->
    List.fold_left (fun acc (p, field) ->
        Clambda.Usequence (build_setfield (p, field), acc))
      (build_setfield h) t

let accumulate_structured_constants t env symbol
      (c : Flambda.constant_defining_value) acc =
  match c with
  | Allocated_const c ->
    Symbol.Map.add symbol (to_clambda_allocated_constant c) acc
  | Block (tag, fields) ->
    let fields = List.map to_clambda_const fields in
    Symbol.Map.add symbol (Clambda.Uconst_block (Tag.to_int tag, fields)) acc
  | Set_of_closures set_of_closures ->
    let to_clambda_set_of_closures =
      to_clambda_closed_set_of_closures t env symbol set_of_closures
    in
    Symbol.Map.add symbol to_clambda_set_of_closures acc
  | Project_closure _ -> acc

let rec to_clambda_program t env constants (program : Flambda.program) :
  Clambda.ulambda * Clambda.ustructured_constant Symbol.Map.t =
  match program with
  | Let_symbol (symbol, alloc, program) ->
    (* Useful only for unboxing. Since floats and boxed integers will
       never be part of a Let_rec_symbol, handling only the Let_symbol
       is sufficient. *)
    let env = match alloc with
      | Allocated_const const ->
        Env.add_allocated_const env symbol const
      | _ ->
        env
    in
    let constants =
      accumulate_structured_constants t env symbol alloc constants
    in
    to_clambda_program t env constants program
  | Let_rec_symbol (defs, program) ->
    let constants =
      List.fold_left (fun constants (symbol, alloc) ->
          accumulate_structured_constants t env symbol alloc constants)
        constants defs
    in
    to_clambda_program t env constants program
  | Import_symbol (_, program) ->
    to_clambda_program t env constants program
  | Initialize_symbol (symbol, _tag, fields, program) ->
    (* The tag is ignored here: It is used separately to generate the
       preallocated block. Only the initialisation code is generated
       here. *)
    let e1 = to_clambda_initialize_symbol t env symbol fields in
    let e2, constants = to_clambda_program t env constants program in
    Usequence (e1, e2), constants
  | Effect (expr, program) ->
    let e1 = to_clambda t env expr in
    let e2, constants = to_clambda_program t env constants program in
    Usequence (e1, e2), constants
  | End _ ->
    Uconst (Uconst_ptr 0), constants

type result = {
  expr : Clambda.ulambda;
  preallocated_blocks : Clambda.preallocated_block list;
  structured_constants : Clambda.ustructured_constant Symbol.Map.t;
  exported : Export_info.t;
}

let convert (program, exported) : result =
  let current_unit =
    let offsets = Closure_offsets.compute program in
    { fun_offset_table = offsets.function_offsets;
      fv_offset_table = offsets.free_variable_offsets;
      closures = Flambda_utils.make_closure_map program;
      constant_sets_of_closures =
        Flambda_utils.all_lifted_constant_sets_of_closures program;
    }
  in
  let imported_units =
    let imported = Compilenv.approx_env () in
    { fun_offset_table = imported.offset_fun;
      fv_offset_table = imported.offset_fv;
      closures = imported.closures;
      constant_sets_of_closures = imported.constant_sets_of_closures;
    }
  in
  let t = { current_unit; imported_units; } in
  let preallocated_blocks =
    List.map (fun (symbol, tag, fields) ->
        { Clambda.
          symbol = Linkage_name.to_string (Symbol.label symbol);
          tag = Tag.to_int tag;
          size = List.length fields;
        })
      (Flambda_utils.initialize_symbols program)
  in
  let expr, structured_constants =
    to_clambda_program t Env.empty Symbol.Map.empty program
  in
  (* XCR mshinwell for pchambart: add offsets to export info
     pchambart: done, but reexported are still missing *)
  let exported =
    Export_info.add_offsets exported
      ~offset_fun:current_unit.fun_offset_table
      ~offset_fv:current_unit.fv_offset_table
  in
  { expr; preallocated_blocks; structured_constants; exported; }
