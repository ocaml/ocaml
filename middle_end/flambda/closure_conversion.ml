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

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module Names = Internal_variable_names

let name_expr = Flambda_utils.name_expr
let name_expr_from_var = Flambda_utils.name_expr_from_var

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  backend : (module Backend_intf.S);
  mutable imported_symbols : Symbol.Set.t;
  mutable declared_symbols : (Symbol.t * Flambda.constant_defining_value) list;
}

let add_default_argument_wrappers lam =
  let f (lam : Lambda.lambda) : Lambda.lambda =
    match lam with
    | Llet (( Strict | Alias | StrictOpt), _k, id,
        Lfunction {kind; params; body = fbody; attr; loc}, body) ->
      begin match
        Simplif.split_default_wrapper ~id ~kind ~params
          ~body:fbody ~return:Pgenval ~attr ~loc
      with
      | [{ id = fun_id; def }] ->
        Llet (Alias, Pgenval, fun_id, Lfunction def, body)
      | [{ id = fun_id; def };
         { id = inner_fun_id; def = def_inner }] ->
        Llet (Alias, Pgenval, inner_fun_id, Lfunction def_inner,
              Llet (Alias, Pgenval, fun_id, Lfunction def, body))
      | _ -> assert false
      end
    | Lletrec (defs, body) ->
        let defs =
          List.flatten
            (List.map
               (function Lambda.{ id; def = {kind; params; body; attr; loc} } ->
                   Simplif.split_default_wrapper ~id ~kind ~params ~body
                     ~return:Pgenval ~attr ~loc)
               defs)
        in
        Lletrec (defs, body)
    | lam -> lam
  in
  Lambda.map f lam

(** Generate a wrapper ("stub") function that accepts a tuple argument and
    calls another function with arguments extracted in the obvious
    manner from the tuple. *)
let tupled_function_call_stub original_params unboxed_version ~closure_bound_var
      : Flambda.function_declaration =
  let tuple_param_var = Variable.rename unboxed_version in
  let params = List.map (fun p -> Variable.rename p) original_params in
  let call : Flambda.t =
    Apply ({
        func = unboxed_version;
        args = params;
        (* CR-someday mshinwell for mshinwell: investigate if there is some
           redundancy here (func is also unboxed_version) *)
        kind = Direct (Closure_id.wrap unboxed_version);
        dbg = Debuginfo.none;
        inline = Default_inline;
        specialise = Default_specialise;
      })
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : Flambda.named =
          Prim (Pfield (pos, Pointer, Mutable),
                [tuple_param_var], Debuginfo.none)
        in
        pos + 1, Flambda.create_let param lam body)
      (0, call) params
  in
  let tuple_param = Parameter.wrap tuple_param_var in
  Flambda.create_function_declaration ~params:[tuple_param]
    ~body ~stub:true ~dbg:Debuginfo.none ~inline:Default_inline
    ~specialise:Default_specialise ~is_a_functor:false
    ~closure_origin:(Closure_origin.create (Closure_id.wrap closure_bound_var))
    ~poll:Default_poll (* don't propagate attribute to wrappers *)

let register_const t (constant:Flambda.constant_defining_value) name
    : Flambda.constant_defining_value_block_field * Internal_variable_names.t =
  let var = Variable.create name in
  let symbol = Symbol.of_variable var in
  t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
  Symbol symbol, name

let rec declare_const t (const : Lambda.structured_constant)
    : Flambda.constant_defining_value_block_field * Internal_variable_names.t =
  match const with
  | Const_base (Const_int c) -> (Const (Int c), Names.const_int)
  | Const_base (Const_char c) -> (Const (Char c), Names.const_char)
  | Const_base (Const_string (s, _, _)) ->
    let const, name =
      (Flambda.Allocated_const (Immutable_string s),
       Names.const_immstring)
    in
    register_const t const name
  | Const_base (Const_float c) ->
    register_const t
      (Allocated_const (Float (float_of_string c)))
      Names.const_float
  | Const_base (Const_int32 c) ->
    register_const t (Allocated_const (Int32 c))
      Names.const_int32
  | Const_base (Const_int64 c) ->
    register_const t (Allocated_const (Int64 c))
      Names.const_int64
  | Const_base (Const_nativeint c) ->
    register_const t (Allocated_const (Nativeint c)) Names.const_nativeint
  | Const_immstring c ->
    register_const t (Allocated_const (Immutable_string c))
      Names.const_immstring
  | Const_float_array c ->
    register_const t
      (Allocated_const (Immutable_float_array (List.map float_of_string c)))
      Names.const_float_array
  | Const_block (tag, consts) ->
    let const : Flambda.constant_defining_value =
      Block (Tag.create_exn tag,
             List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const Names.const_block

let close_const t (const : Lambda.structured_constant)
      : Flambda.named * Internal_variable_names.t =
  match declare_const t const with
  | Const c, name ->
    Const c, name
  | Symbol s, name ->
    Symbol s, name

let lambda_const_bool b : Lambda.structured_constant =
  if b then
    Lambda.const_int 1
  else
    Lambda.const_int 0

let lambda_const_int i : Lambda.structured_constant =
  Const_base (Const_int i)

let rec close t env (lam : Lambda.lambda) : Flambda.t =
  match lam with
  | Lvar id ->
     begin match Env.find_var_exn env id with
     | var -> Var var
     | exception Not_found ->
        Misc.fatal_errorf "Closure_conversion.close: unbound identifier %a"
          Ident.print id
     end
  | Lmutvar id ->
    begin match Env.find_mutable_var_exn env id with
    | mut_var ->
       name_expr (Read_mutable mut_var) ~name:Names.read_mutable
    | exception Not_found ->
       Misc.fatal_errorf
         "Closure_conversion.close: unbound mutable identifier %a"
         Ident.print id
    end
  | Lconst cst ->
    let cst, name = close_const t cst in
    name_expr cst ~name
  | Llet ((Strict | Alias | StrictOpt), _value_kind, id, defining_expr, body) ->
    (* TODO: keep value_kind in flambda *)
    let var = Variable.create_with_same_name_as_ident id in
    let defining_expr =
      close_let_bound_expression t var env defining_expr
    in
    let body = close t (Env.add_var env id var) body in
    Flambda.create_let var defining_expr body
  | Lmutlet (block_kind, id, defining_expr, body) ->
    let mut_var = Mutable_variable.create_with_same_name_as_ident id in
    let var = Variable.create_with_same_name_as_ident id in
    let defining_expr =
      close_let_bound_expression t var env defining_expr
    in
    let body = close t (Env.add_mutable_var env id mut_var) body in
    Flambda.create_let var defining_expr
      (Let_mutable
         { var = mut_var;
           initial_value = var;
           body;
           contents_kind = block_kind })
  | Lfunction { kind; params; body; attr; loc; } ->
    let name = Names.anon_fn_with_loc loc in
    let closure_bound_var = Variable.create name in
    (* CR-soon mshinwell: some of this is now very similar to the let rec case
       below *)
    let set_of_closures_var = Variable.create Names.set_of_closures in
    let set_of_closures =
      let decl =
        Function_decl.create ~let_rec_ident:None ~closure_bound_var ~kind
          ~params:(List.map fst params) ~body ~attr ~loc
      in
      close_functions t env (Function_decls.create [decl])
    in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Flambda.create_let set_of_closures_var set_of_closures
      (name_expr (Project_closure (project_closure)) ~name)
  | Lapply { ap_func; ap_args; ap_loc;
             ap_tailcall = _; ap_inlined; ap_specialised; } ->
    Lift_code.lifting_helper (close_list t env ap_args)
      ~evaluation_order:`Right_to_left
      ~name:Names.apply_arg
      ~create_body:(fun args ->
        let func = close t env ap_func in
        let func_var = Variable.create Names.apply_funct in
        Flambda.create_let func_var (Expr func)
          (Apply ({
              func = func_var;
              args;
              kind = Indirect;
              dbg = Debuginfo.from_location ap_loc;
              inline = ap_inlined;
              specialise = ap_specialised;
            })))
  | Lletrec (defs, body) ->
    let env =
      List.fold_right (fun { Lambda.id } env ->
          Env.add_var env id (Variable.create_with_same_name_as_ident id))
        defs env
    in
    let function_declarations =
      (* Name functions *)
      List.map (function
          | Lambda.{ id = let_rec_ident;
              def = { kind; params; body; attr; loc }} ->
            let closure_bound_var =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            Function_decl.create ~let_rec_ident:(Some let_rec_ident)
              ~closure_bound_var ~kind ~params:(List.map fst params) ~body
              ~attr ~loc)
        defs
    in
    let set_of_closures_var = Variable.create (Names.set_of_closures) in
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
  | Lsend (kind, meth, obj, args, loc) ->
    let meth_var = Variable.create Names.meth in
    let obj_var = Variable.create Names.obj in
    let dbg = Debuginfo.from_location loc in
    Flambda.create_let meth_var (Expr (close t env meth))
      (Flambda.create_let obj_var (Expr (close t env obj))
        (Lift_code.lifting_helper (close_list t env args)
          ~evaluation_order:`Right_to_left
          ~name:Names.send_arg
          ~create_body:(fun args ->
              Send { kind; meth = meth_var; obj = obj_var; args; dbg; })))
  | Lprim ((Pdivint Safe | Pmodint Safe
           | Pdivbint { is_safe = Safe } | Pmodbint { is_safe = Safe }) as prim,
           [arg1; arg2], loc)
      when not !Clflags.unsafe ->
    let arg2 = close t env arg2 in
    let arg1 = close t env arg1 in
    let numerator = Variable.create Names.numerator in
    let denominator = Variable.create Names.denominator in
    let zero = Variable.create Names.zero in
    let is_zero = Variable.create Names.is_zero in
    let exn = Variable.create Names.division_by_zero in
    let exn_symbol =
      t.symbol_for_global' Predef.ident_division_by_zero
    in
    let dbg = Debuginfo.from_location loc in
    let zero_const : Flambda.named =
      match prim with
      | Pdivint _ | Pmodint _ ->
        Const (Int 0)
      | Pdivbint { size = Pint32 } | Pmodbint { size = Pint32 } ->
        Allocated_const (Int32 0l)
      | Pdivbint { size = Pint64 } | Pmodbint { size = Pint64 } ->
        Allocated_const (Int64 0L)
      | Pdivbint { size = Pnativeint } | Pmodbint { size = Pnativeint } ->
        Allocated_const (Nativeint 0n)
      | _ -> assert false
    in
    let prim : Clambda_primitives.primitive =
      match prim with
      | Pdivint _ -> Pdivint Unsafe
      | Pmodint _ -> Pmodint Unsafe
      | Pdivbint { size } -> Pdivbint { size; is_safe = Unsafe }
      | Pmodbint { size } -> Pmodbint { size; is_safe = Unsafe }
      | _ -> assert false
    in
    let comparison : Clambda_primitives.primitive =
      match prim with
      | Pdivint _ | Pmodint _ -> Pintcomp Ceq
      | Pdivbint { size } | Pmodbint { size } -> Pbintcomp (size,Ceq)
      | _ -> assert false
    in
    t.imported_symbols <- Symbol.Set.add exn_symbol t.imported_symbols;
    Flambda.create_let zero zero_const
      (Flambda.create_let exn (Symbol exn_symbol)
        (Flambda.create_let denominator (Expr arg2)
          (Flambda.create_let numerator (Expr arg1)
            (Flambda.create_let is_zero
              (Prim (comparison, [zero; denominator], dbg))
                (If_then_else (is_zero,
                  name_expr (Prim (Praise Raise_regular, [exn], dbg))
                    ~name:Names.dummy,
                  (* CR-someday pchambart: find the right event.
                     mshinwell: I briefly looked at this, and couldn't
                     figure it out.
                     lwhite: I don't think any of the existing events
                     are suitable. I had to add a new one for a similar
                     case in the array data types work.
                     mshinwell: deferred CR *)
                  name_expr ~name:Names.result
                    (Prim (prim, [numerator; denominator], dbg))))))))
  | Lprim ((Pdivint Safe | Pmodint Safe
           | Pdivbint { is_safe = Safe } | Pmodbint { is_safe = Safe }), _, _)
      when not !Clflags.unsafe ->
    Misc.fatal_error "Pdivint / Pmodint must have exactly two arguments"
  | Lprim (Psequor, [arg1; arg2], _) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_true = Variable.create Names.const_true in
    let cond = Variable.create Names.cond_sequor in
    Flambda.create_let const_true (Const (Int 1))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, Var const_true, arg2)))
  | Lprim (Psequand, [arg1; arg2], _) ->
    let arg1 = close t env arg1 in
    let arg2 = close t env arg2 in
    let const_false = Variable.create Names.const_false in
    let cond = Variable.create Names.const_sequand in
    Flambda.create_let const_false (Const (Int 0))
      (Flambda.create_let cond (Expr arg1)
        (If_then_else (cond, arg2, Var const_false)))
  | Lprim ((Psequand | Psequor), _, _) ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Lprim ((Pbytes_to_string | Pbytes_of_string), [arg], _) ->
    close t env arg
  | Lprim (Pignore, [arg], _) ->
    let var = Variable.create Names.ignore in
    let defining_expr =
      close_let_bound_expression t var env arg
    in
    Flambda.create_let var defining_expr
      (name_expr (Const (Int 0)) ~name:Names.unit)
  | Lprim (Praise kind, [arg], loc) ->
    let arg_var = Variable.create Names.raise_arg in
    let dbg = Debuginfo.from_location loc in
    Flambda.create_let arg_var (Expr (close t env arg))
      (name_expr
        (Prim (Praise kind, [arg_var], dbg))
        ~name:Names.raise)
  | Lprim (Pctconst c, [arg], _loc) ->
      let module Backend = (val t.backend) in
      let const =
        begin match c with
        | Big_endian -> lambda_const_bool Backend.big_endian
        | Word_size -> lambda_const_int (8*Backend.size_int)
        | Int_size -> lambda_const_int (8*Backend.size_int - 1)
        | Max_wosize ->
            lambda_const_int ((1 lsl ((8*Backend.size_int) - 10)) - 1)
        | Ostype_unix ->
            lambda_const_bool (String.equal Config.target_os_type "Unix")
        | Ostype_win32 ->
            lambda_const_bool (String.equal Config.target_os_type "Win32")
        | Ostype_cygwin ->
            lambda_const_bool (String.equal Config.target_os_type "Cygwin")
        | Backend_type ->
            Lambda.const_int 0 (* tag 0 is the same as Native *)
        end
      in
      close t env
        (Lambda.Llet(Strict, Pgenval, Ident.create_local "dummy",
                     arg, Lconst const))
  | Lprim (Pfield _, [Lprim (Pgetglobal id, [],_)], _)
      when Ident.same id t.current_unit_id ->
    Misc.fatal_errorf "[Pfield (Pgetglobal ...)] for the current compilation \
        unit is forbidden upon entry to the middle end"
  | Lprim (Psetfield (_, _, _), [Lprim (Pgetglobal _, [], _); _], _) ->
    Misc.fatal_errorf "[Psetfield (Pgetglobal ...)] is \
        forbidden upon entry to the middle end"
  | Lprim (Pgetglobal id, [], _) when Ident.is_predef id ->
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    name_expr (Symbol symbol) ~name:Names.predef_exn
  | Lprim (Pgetglobal id, [], _) ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    name_expr (Symbol symbol) ~name:Names.pgetglobal
  | Lprim (lambda_p, args, loc) ->
    (* One of the important consequences of the ANF-like representation
       here is that we obtain names corresponding to the components of
       blocks being made (with [Pmakeblock]).  This information can be used
       by the simplification pass to increase the likelihood of eliminating
       the allocation, since some field accesses can be tracked back to known
       field values. *)
    let dbg = Debuginfo.from_location loc in
    let p = Convert_primitives.convert lambda_p in
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:(Names.of_primitive_arg lambda_p)
      ~create_body:(fun args ->
        name_expr (Prim (p, args, dbg))
          ~name:(Names.of_primitive lambda_p))
  | Lswitch (arg, sw, _loc) ->
    let scrutinee = Variable.create Names.switch in
    let aux (i, lam) = i, close t env lam in
    let nums sw_num cases default =
      let module I = Numbers.Int in
      match default with
      | Some _ ->
          I.zero_to_n (sw_num - 1)
      | None ->
          List.fold_left (fun set (i, _) -> I.Set.add i set) I.Set.empty cases
    in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (Switch (scrutinee,
        { numconsts = nums sw.sw_numconsts sw.sw_consts sw.sw_failaction;
          consts = List.map aux sw.sw_consts;
          numblocks = nums sw.sw_numblocks sw.sw_blocks sw.sw_failaction;
          blocks = List.map aux sw.sw_blocks;
          failaction = Option.map (close t env) sw.sw_failaction;
        }))
  | Lstringswitch (arg, sw, def, _) ->
    let scrutinee = Variable.create Names.string_switch in
    Flambda.create_let scrutinee (Expr (close t env arg))
      (String_switch (scrutinee,
        List.map (fun (s, e) -> s, close t env e) sw,
        Option.map (close t env) def))
  | Lstaticraise (i, args) ->
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:Names.staticraise_arg
      ~create_body:(fun args ->
        let static_exn = Env.find_static_exception env i in
        Static_raise (static_exn, args))
  | Lstaticcatch (body, (i, ids), handler) ->
    let st_exn = Static_exception.create () in
    let env = Env.add_static_exception env i st_exn in
    let vars =
      List.map (fun (id, kind) ->
          Variable.create_with_same_name_as_ident id, kind)
        ids
    in
    let env_handler =
      Env.add_vars env (List.map fst ids) (List.map fst vars)
    in
    Static_catch (st_exn, vars, close t env body,
      close t env_handler handler)
  | Ltrywith (body, id, handler) ->
    let var = Variable.create_with_same_name_as_ident id in
    Try_with (close t env body, var, close t (Env.add_var env id var) handler)
  | Lifthenelse (cond, ifso, ifnot) ->
    let cond = close t env cond in
    let cond_var = Variable.create Names.cond in
    Flambda.create_let cond_var (Expr cond)
      (If_then_else (cond_var, close t env ifso, close t env ifnot))
  | Lsequence (lam1, lam2) ->
    let var = Variable.create Names.sequence in
    let lam1 = Flambda.Expr (close t env lam1) in
    let lam2 = close t env lam2 in
    Flambda.create_let var lam1 lam2
  | Lwhile (cond, body) -> While (close t env cond, close t env body)
  | Lfor (id, lo, hi, direction, body) ->
    let bound_var = Variable.create_with_same_name_as_ident id in
    let from_value = Variable.create Names.for_from in
    let to_value = Variable.create Names.for_to in
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
    let new_value_var = Variable.create Names.new_value in
    Flambda.create_let new_value_var (Expr (close t env new_value))
      (Assign { being_assigned; new_value = new_value_var; })
  | Levent (lam, _) -> close t env lam
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
    let loc = Function_decl.loc decl in
    let dbg = Debuginfo.from_location loc in
    let params = Function_decl.params decl in
    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.free_idents]; the results of
       that renaming are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun id env ->
          Env.add_var env id (Variable.create_with_same_name_as_ident id))
        params closure_env_without_parameters
    in
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
    let stub = Function_decl.stub decl in
    let param_vars = List.map (Env.find_var closure_env) params in
    let params = List.map Parameter.wrap param_vars in
    let closure_bound_var = Function_decl.closure_bound_var decl in
    let unboxed_version = Variable.rename closure_bound_var in
    let body = close t closure_env body in
    let closure_origin =
      Closure_origin.create (Closure_id.wrap unboxed_version)
    in
    let fun_decl =
      Flambda.create_function_declaration ~params ~body ~stub ~dbg
        ~inline:(Function_decl.inline decl)
        ~specialise:(Function_decl.specialise decl)
        ~is_a_functor:(Function_decl.is_a_functor decl)
        ~closure_origin
        ~poll:(Function_decl.poll_attribute decl)
    in
    match Function_decl.kind decl with
    | Curried -> Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
      let unboxed_version = Variable.rename closure_bound_var in
      let generic_function_stub =
        tupled_function_call_stub param_vars unboxed_version ~closure_bound_var
      in
      Variable.Map.add unboxed_version fun_decl
        (Variable.Map.add closure_bound_var generic_function_stub map)
  in
  let function_decls =
    let is_classic_mode = !Clflags.classic_inlining in
    let funs =
      List.fold_left close_one_function Variable.Map.empty
        (Function_decls.to_list function_declarations)
    in
    Flambda.create_function_declarations ~is_classic_mode ~funs
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    let free_vars =
      Ident.Set.fold (fun var map ->
          let internal_var =
            Env.find_var closure_env_without_parameters var
          in
          let external_var : Flambda.specialised_to =
            { var = Env.find_var external_env var;
              projection = None;
            }
          in
          Variable.Map.add internal_var external_var map)
        all_free_idents Variable.Map.empty
    in
    Flambda.create_set_of_closures ~function_decls ~free_vars
      ~specialised_args:Variable.Map.empty
      ~direct_call_surrogates:Variable.Map.empty
  in
  Set_of_closures set_of_closures

and close_list t sb l = List.map (close t sb) l

and close_let_bound_expression t ?let_rec_ident let_bound_var env
      (lam : Lambda.lambda) : Flambda.named =
  match lam with
  | Lfunction { kind; params; body; attr; loc; } ->
    (* Ensure that [let] and [let rec]-bound functions have appropriate
       names. *)
    let closure_bound_var = Variable.rename let_bound_var in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind
        ~params:(List.map fst params) ~body ~attr ~loc
    in
    let set_of_closures_var = Variable.rename let_bound_var in
    let set_of_closures =
      close_functions t env (Function_decls.create [decl])
    in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Expr (Flambda.create_let set_of_closures_var set_of_closures
      (name_expr_from_var (Project_closure (project_closure))
        ~var:let_bound_var))
  | lam -> Expr (close t env lam)

let lambda_to_flambda ~backend ~module_ident ~size lam
      : Flambda.program =
  let lam = add_default_argument_wrappers lam in
  let module Backend = (val backend : Backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let t =
    { current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
      backend;
      imported_symbols = Symbol.Set.empty;
      declared_symbols = [];
    }
  in
  let module_symbol = Backend.symbol_for_global' module_ident in
  let block_symbol =
    let var = Variable.create Internal_variable_names.module_as_block in
    Symbol.of_variable var
  in
  (* The global module block is built by accessing the fields of all the
     introduced symbols. *)
  (* CR-soon mshinwell for mshinwell: Add a comment describing how modules are
     compiled. *)
  let fields =
    Array.init size (fun pos ->
      let sym_v = Variable.create Names.block_symbol in
      let result_v = Variable.create Names.block_symbol_get in
      let value_v = Variable.create Names.block_symbol_get_field in
      Flambda.create_let
        sym_v (Symbol block_symbol)
         (Flambda.create_let result_v
            (Prim (Pfield (0, Pointer, Mutable), [sym_v], Debuginfo.none))
            (Flambda.create_let value_v
              (Prim (Pfield (pos, Pointer, Mutable),
                     [result_v], Debuginfo.none))
              (Var value_v))))
  in
  let module_initializer : Flambda.program_body =
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
  let program_body =
    List.fold_left
      (fun program_body (symbol, constant) : Flambda.program_body ->
         Let_symbol (symbol, constant, program_body))
      module_initializer
      t.declared_symbols
  in
  { imported_symbols = t.imported_symbols;
    program_body;
  }
