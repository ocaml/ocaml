(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                      Pierre Chambart (OCamlPro)                        *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

open Misc
open Lambda
open Abstract_identifiers
open Flambda

let rec add_debug_info ev f =
  match ev.lev_kind with
  | Lev_after _ ->
    begin match f with
      | Fapply(ap,v) ->
        Fapply({ ap with ap_dbg = Debuginfo.from_call ev}, v)
      | Fprim(p, args, dinfo, v) ->
        Fprim(p, args, Debuginfo.from_call ev, v)
      | Fsend(kind, f1, f2, args, dinfo, v) ->
        Fsend(kind, f1, f2, args, Debuginfo.from_call ev, v)
      | Fsequence(f1, f2, v) ->
        Fsequence(f1, add_debug_info ev f2, v)
      | _ -> f
    end
  | _ -> f

let nid = ExprId.create

type function_declaration =
  { rec_ident : Ident.t;
    closure_bound_var : Variable.t;
    kind : function_kind;
    params : Ident.t list;
    body : lambda }

let rec close_const = function
  | Const_base c -> Fconst(Fconst_base c, nid ~name:"cst" ())
  | Const_pointer c -> Fconst(Fconst_pointer c, nid ~name:"cstptr" ())
  | Const_immstring c -> Fconst(Fconst_immstring c, nid ~name:"immstring" ())
  | Const_float_array c -> Fconst(Fconst_float_array c, nid ~name:"float" ())
  | Const_block (tag, l) ->
      Fprim(Pmakeblock(tag, Asttypes.Immutable),
            List.map close_const l, Debuginfo.none, nid ~name:"cstblock" ())

let to_flambda
    ~for_bytecode
    ~current_compilation_unit ~current_unit_id
    ~symbol_for_global'
    lam =

  let debugger_map = ref VarMap.empty in

  let create_var id =
    let var = Variable.create ~current_compilation_unit (Ident.name id) in
    debugger_map := VarMap.add var id !debugger_map;
    var
  in

  let rename_var var =
    let new_var = Variable.rename ~current_compilation_unit var in
    begin
      try
        let id = VarMap.find var !debugger_map in
        debugger_map := VarMap.add new_var id !debugger_map;
      with Not_found -> ()
    end;
    new_var in

  let add_var id var (env:Variable.t Ident.tbl) = Ident.add id var env in

  let add_vars ids vars env = List.fold_right2 add_var ids vars env in

  let find_var env id =
    try Ident.find_same id env
    with Not_found ->
      Format.eprintf "%s@." (Ident.unique_name id);
      fatal_error ("Flambdagen.close: var " ^ Ident.unique_name id) in

  let rec close env = function
    | Lvar id ->
        let var = find_var env id in
        Fvar (var, nid ~name:(Format.asprintf "var_%a" Variable.print var) ())
    | Lconst cst -> close_const cst
    | Llet(str, id, lam, body) ->
        let str =
          match str with
          | Variable -> Assigned
          | _ -> Not_assigned in
        let var = create_var id in
        Flet(str, var, close_named var env lam,
             close (add_var id var env) body, nid ~name:"let" ())
    | Lfunction(kind, params, body) ->
        let closure_bound_var =
          Variable.create ~current_compilation_unit "fun" in
        let decl =
          { rec_ident = Ident.create "dummy"; closure_bound_var;
            kind; params; body } in
        Ffunction(
          { fu_closure = close_functions env [decl];
            fu_fun = Closure_function.wrap closure_bound_var;
            fu_relative_to = None },
          nid ~name:"function" ())
    | Lapply(funct, args, loc) ->
        Fapply(
          { ap_function = close env funct;
            ap_arg = close_list env args;
            ap_kind = Indirect;
            ap_dbg = Debuginfo.none },
          nid ~name:"apply" ())
    | Lletrec(defs, body) ->
        let env =
          List.fold_right
            (fun (id,  _) env -> add_var id (create_var id) env)
            defs env in
        let function_declarations =
          List.map
            (function
              | (rec_ident, Lfunction(kind, params, body)) ->
                  let closure_bound_var = create_var rec_ident in
                  Some ({ rec_ident; closure_bound_var;
                          kind; params; body })
              | _ -> None)
            defs
        in
        begin match Misc.some_if_all_elements_are_some function_declarations with
        | None ->
            let fdefs =
              List.map
                (fun (id, def) ->
                   let var = find_var env id in
                   (var, close_named var env def))
                defs in
            Fletrec(fdefs, close env body, nid ~name:"letrec" ())
        | Some function_declarations ->
            (* When all the binding are functions, we build a single closure
               for all the functions *)
            let clos = close_functions env function_declarations in
            let clos_var = Variable.create ~current_compilation_unit "clos" in
            let body =
              List.fold_left
                (fun body decl ->
                   let let_bound_var = find_var env decl.rec_ident in
                   Flet(Not_assigned, let_bound_var,
                        Ffunction(
                          { fu_closure = Fvar (clos_var, nid ());
                            fu_fun = Closure_function.wrap decl.closure_bound_var;
                            fu_relative_to = None },
                          nid ()),
                        body, nid ()))
                (close env body) function_declarations in
            Flet(Not_assigned, clos_var, clos,
                 body, nid ~name:"closure_letrec" ())
        end
    | Lsend(kind, met, obj, args, _) ->
        Fsend(kind, close env met, close env obj,
              close_list env args, Debuginfo.none, nid ())
    | Lprim(Pdirapply loc,[funct;arg])
    | Lprim(Prevapply loc,[arg;funct]) ->
        close env (Lapply(funct, [arg], loc))
    | Lprim(Praise, [Levent(arg, ev)]) ->
        let arg = close env arg in
        let arg = if for_bytecode then Fevent (arg, ev, nid ()) else arg in
        Fprim(Praise, [arg], Debuginfo.from_raise ev, nid ())
    | Lprim(Pfield i, [Lprim(Pgetglobal id, [])])
      when Ident.same id current_unit_id ->
        Fprim(Pgetglobalfield(id,i), [], Debuginfo.none,
              nid ~name:"getglobalfield" ())
    | Lprim(Psetfield(i,_), [Lprim(Pgetglobal id, []); lam]) ->
        assert(Ident.same id current_unit_id);
        Fprim(Psetglobalfield i, [close env lam], Debuginfo.none,
              nid ~name:"setglobalfield" ())
    | Lprim(Pgetglobal id, [])
      when not (Ident.is_predef_exn id) &&
           not for_bytecode ->
        assert(not (Ident.same id current_unit_id));
        let symbol = symbol_for_global' id in
        Fsymbol (symbol,nid ~name:"external_global" ())
    | Lprim(Pmakeblock _ as p, args) ->
        (* Lift the contents of the block to variables. This allows to
           eliminate the allocation if the block does not escape.
           A more general solution would be to convert completely to ANF *)
        let (block,lets) = List.fold_right (fun lam (block,lets) ->
            match close env lam with
            | Fvar(v,_) as e -> (e::block,lets)
            | expr ->
                let v = Variable.create ~current_compilation_unit "block_field" in
                Fvar(v,nid ()) :: block, (v,expr)::lets)
            args ([],[]) in
        let block = Fprim(p, block, Debuginfo.none, nid ~name:"block" ()) in
        List.fold_left (fun body (v,expr) ->
            Flet(Not_assigned, v, expr, body, nid ()))
          block lets
    | Lprim(p, args) ->
        Fprim(p, close_list env args, Debuginfo.none,
              nid ~name:"prim" ())
    | Lswitch(arg, sw) ->
        let aux (i,lam) = i, close env lam in
        let rec set n = (* set of integers {0, 1, ... n} *)
          if n < 0 then Ext_types.IntSet.empty
          else Ext_types.IntSet.add n (set (n-1)) in
        Fswitch(close env arg,
                { fs_numconsts = set (sw.sw_numconsts - 1);
                  fs_consts = List.map aux sw.sw_consts;
                  fs_numblocks = set (sw.sw_numblocks - 1);
                  fs_blocks = List.map aux sw.sw_blocks;
                  fs_failaction = Misc.may_map (close env) sw.sw_failaction },
                nid ~name:"switch" ())
    | Lstaticraise (i, args) ->
        Fstaticraise (Static_exception.of_int i, close_list env args, nid ())
    | Lstaticcatch(body, (i, ids), handler) ->
        let vars = List.map create_var ids in
        Fstaticcatch (Static_exception.of_int i, vars,
                      close env body, close (add_vars ids vars env) handler,
                      nid ())
    | Ltrywith(body, id, handler) ->
        let var = create_var id in
        Ftrywith(close env body, var, close (add_var id var env) handler, nid ())
    | Lifthenelse(arg, ifso, ifnot) ->
        Fifthenelse(close env arg, close env ifso, close env ifnot,
                    nid ~name:"if" ())
    | Lsequence(lam1, lam2) ->
        Fsequence(close env lam1, close env lam2,
                  nid ~name:"seq" ())
    | Lwhile(cond, body) ->
        Fwhile(close env cond, close env body, nid ())
    | Lfor(id, lo, hi, dir, body) ->
        let var = create_var id in
        Ffor(var, close env lo, close env hi, dir,
             close (add_var id var env) body,
             nid ())
    | Lassign(id, lam) ->
        Fassign(find_var env id, close env lam, nid ())
    | Levent(lam, ev) ->
        let lam = add_debug_info ev (close env lam) in
        if for_bytecode then Fevent(lam, ev, nid ()) else lam
    | Lifused _ ->
        assert false

  and close_functions external_env function_declarations =

    let used_idents_per_function =
      List.fold_right
        (fun {closure_bound_var; body; params} map ->
           VarMap.add closure_bound_var (Lambda.free_variables body) map)
        function_declarations VarMap.empty in

    let all_free_idents =
      let rec_idents = List.map (fun d -> d.rec_ident) function_declarations in
      let all_params =
        List.concat (List.map (fun d -> d.params) function_declarations) in
      (* all used idents *)
      VarMap.fold
        (fun _ -> IdentSet.union)
        used_idents_per_function IdentSet.empty
      (* remove function parameters *)
      |> List.fold_right IdentSet.remove all_params
      (* remove recursives functions *)
      |> List.fold_right IdentSet.remove rec_idents in

    let closure_env_without_parameters =
      Ident.empty
      (* add recursive functions *)
      |> List.fold_right
        (fun d env -> add_var d.rec_ident d.closure_bound_var env)
        function_declarations
      (* add free variables *)
      |> IdentSet.fold
        (fun id env -> add_var id (create_var id) env)
        all_free_idents in

    let close_one_function map { closure_bound_var; kind; params; body } =
      let dbg = match body with
        | Levent (_,({lev_kind=Lev_function} as ev)) ->
            Debuginfo.from_call ev
        | _ -> Debuginfo.none in
      let closure_env =
        List.fold_right
          (fun id env -> add_var id (create_var id) env)
          params closure_env_without_parameters in
      let params = List.map (find_var closure_env) params in
      let fun_decl =
        { stub = false; params; dbg;
          free_variables =
            IdentSet.fold
              (fun id set -> VarSet.add (find_var closure_env id) set)
              (VarMap.find closure_bound_var used_idents_per_function)
              VarSet.empty;
          body = close closure_env body } in
      match kind with
      | Curried ->
          VarMap.add closure_bound_var fun_decl map
      | Tupled ->
          let tuplified_version = rename_var closure_bound_var in
          let generic_function_stub =
            tupled_function_call_stub
              closure_bound_var params tuplified_version in
          let map = VarMap.add closure_bound_var generic_function_stub map in
          VarMap.add tuplified_version fun_decl map
    in
    let fun_decls =
      { ident = FunId.create current_compilation_unit;
        funs =
          List.fold_left close_one_function VarMap.empty function_declarations;
        compilation_unit = current_compilation_unit } in
    let closure =
      { cl_fun = fun_decls;
        cl_free_var =
          IdentSet.fold
            (fun id map ->
               let internal_var = find_var closure_env_without_parameters id in
               let external_var = find_var external_env id in
               VarMap.add internal_var (Fvar(external_var, nid ())) map)
            all_free_idents VarMap.empty;
        cl_specialised_arg = VarMap.empty } in

    Fclosure (closure, nid ())

  and tupled_function_call_stub id original_params tuplified_version =
    let tuple_param =
      Variable.create ~current_compilation_unit "tupled_stub_param" in
    let params = List.map (fun p -> rename_var p) original_params in
    let call = Fapply(
        { ap_function = Fvar(tuplified_version,nid ());
          ap_arg = List.map (fun p' -> Fvar(p',nid ())) params;
          ap_kind = Direct (Closure_function.wrap tuplified_version);
          ap_dbg = Debuginfo.none },
        nid ()) in
    let _, body =
      List.fold_left (fun (pos,body) param ->
          let lam = Fprim(Pfield pos, [Fvar(tuple_param, nid ())],
                          Debuginfo.none, nid ()) in
          pos+1,
          Flet(Not_assigned,param,lam,body,nid ()))
        (0,call) params in
    { stub = true;
      params = [tuple_param];
      free_variables = VarSet.of_list [tuple_param;tuplified_version];
      body;
      dbg = Debuginfo.none }

  and close_list sb l = List.map (close sb) l

  and close_named let_bound_var env = function
    | Lfunction(kind, params, body) ->
        let closure_bound_var = rename_var let_bound_var in
        let decl =
          { rec_ident = Ident.create "dummy"; closure_bound_var;
            kind; params; body } in
        Ffunction(
          { fu_closure = close_functions env [decl];
            fu_fun = Closure_function.wrap closure_bound_var;
            fu_relative_to = None },
          nid ~name:"function" ())
    | lam ->
        close env lam

  in

  let flam = close Ident.empty lam in

  !debugger_map, flam

(** String lifting to toplevel of expressions *)

let rec lift_strings acc = function
    | Lvar _ as lam ->
        acc, lam
    | Lconst (Const_base (Asttypes.Const_string s)) ->
        let id = Ident.create "constant_string" in
        (id, s) :: acc, Lvar id
    | Lconst (Const_base (Asttypes.Const_nativeint _ | Asttypes.Const_char _ |
                          Asttypes.Const_float _ | Asttypes.Const_int32 _ |
                          Asttypes.Const_int64 _ | Asttypes.Const_int _) |
              Const_pointer _ | Const_block _ | Const_float_array _ |
              Const_immstring _) as lam ->
        acc, lam
    | Llet(str, id, lam, body) ->
        let acc, lam = lift_strings acc lam in
        let acc, body = lift_strings acc body in
        acc, Llet(str, id, lam, body)
    | Lfunction(kind, params, body) ->
        let acc, body = lift_strings acc body in
        acc, Lfunction(kind, params, body)
    | Lapply(funct, args, loc) ->
        let acc, funct = lift_strings acc funct in
        let acc, args = lift_strings_list acc args in
        acc, Lapply(funct, args, loc)
    | Lletrec(defs, body) ->
        let acc, defs = lift_strings_couple_list acc defs in
        acc, Lletrec(defs, body)
    | Lsend(kind, met, obj, args, loc) ->
        let acc, met = lift_strings acc met in
        let acc, obj = lift_strings acc obj in
        let acc, args = lift_strings_list acc args in
        acc, Lsend(kind, met, obj, args, loc)
    | Lprim(p, args) ->
        let acc, args = lift_strings_list acc args in
        acc, Lprim(p, args)
    | Lswitch(arg, sw) ->
        let acc, arg = lift_strings acc arg in
        let acc, sw_consts = lift_strings_couple_list acc sw.sw_consts in
        let acc, sw_blocks = lift_strings_couple_list acc sw.sw_blocks in
        let acc, sw_failaction =
          match sw.sw_failaction with
          | None -> acc, None
          | Some failaction ->
              let acc, failaction = lift_strings acc failaction in
              acc, Some failaction in
        acc, Lswitch(arg, { sw with sw_consts; sw_blocks; sw_failaction })
    | Lstaticraise (i, args) ->
        let acc, args = lift_strings_list acc args in
        acc, Lstaticraise (i, args)
    | Lstaticcatch(body, (i, vars), handler) ->
        let acc, body = lift_strings acc body in
        let acc, handler = lift_strings acc handler in
        acc, Lstaticcatch(body, (i, vars), handler)
    | Ltrywith(body, id, handler) ->
        let acc, body = lift_strings acc body in
        let acc, handler = lift_strings acc handler in
        acc, Ltrywith(body, id, handler)
    | Lifthenelse(arg, ifso, ifnot) ->
        let acc, arg = lift_strings acc arg in
        let acc, ifso = lift_strings acc ifso in
        let acc, ifnot = lift_strings acc ifnot in
        acc, Lifthenelse(arg, ifso, ifnot)
    | Lsequence(lam1, lam2) ->
        let acc, lam1 = lift_strings acc lam1 in
        let acc, lam2 = lift_strings acc lam2 in
        acc, Lsequence(lam1, lam2)
    | Lwhile(cond, body) ->
        let acc, cond = lift_strings acc cond in
        let acc, body = lift_strings acc body in
        acc, Lwhile(cond, body)
    | Lfor(id, lo, hi, dir, body) ->
        let acc, lo = lift_strings acc lo in
        let acc, hi = lift_strings acc hi in
        let acc, body = lift_strings acc body in
        acc, Lfor(id, lo, hi, dir, body)
    | Lassign(id, lam) ->
        let acc, lam = lift_strings acc lam in
        acc, Lassign(id, lam)
    | Levent(lam, ev) ->
        let acc, lam = lift_strings acc lam in
        acc, Levent(lam, ev)
    | Lifused _ ->
        assert false

and lift_strings_list acc lams =
  List.fold_right (fun lam (acc,lams) ->
      let acc, lam = lift_strings acc lam in
      acc, lam :: lams)
    lams (acc, [])

and lift_strings_couple_list :
  'a. 'acc -> ('a * Lambda.lambda) list -> 'acc * ('a * Lambda.lambda) list =
  fun acc lams ->
    List.fold_right (fun (v,lam) (acc,lams) ->
        let acc, lam = lift_strings acc lam in
        acc, (v,lam) :: lams)
      lams (acc, [])

let lift_strings_to_toplevel lam =
  let bindings, lam = lift_strings [] lam in
  List.fold_left (fun lam (id, string) ->
      Llet(Strict,id,
           Lconst (Const_base (Asttypes.Const_string string)),
           lam))
    lam bindings

let intro ?(for_bytecode = false) ~current_compilation_unit ~current_unit_id
    ~symbol_for_global' lam =
  (* Strings are the only expressions that can't be duplicated without
     changing the semantics. So we lift them to toplevel to avoid
     having to handle special cases later.
     There is no runtime cost to this transformation: strings are
     constants, they will not appear in the closures *)
  let lam = if for_bytecode then lam else lift_strings_to_toplevel lam in
  to_flambda ~for_bytecode ~current_compilation_unit ~current_unit_id
    ~symbol_for_global' lam
