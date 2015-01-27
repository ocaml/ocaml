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

type t = {
  current_compilation_unit : Symbol.compilation_unit;
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  mutable debugger_map : unit;
}

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

let nid = Expr_id.create

let create_var t id =
  let current_compilation_unit = t.current_compilation_unit in
  let var = Variable.create ~current_compilation_unit (Ident.name id) in
  t.debugger_map <- Variable.Map.add var id t.debugger_map;
  var

let rename_var t var =
  let current_compilation_unit = t.current_compilation_unit in
  let new_var = Variable.rename ~current_compilation_unit var in
  begin
    try
      let id = Variable.Map.find var t.debugger_map in
      t.debugger_map := Variable.Map.add new_var id debugger_map;
    with Not_found -> ()
  end;
  new_var

let add_var id var (env : Variable.t Ident.tbl) = Ident.add id var env
let add_vars ids vars env = List.fold_right2 add_var ids vars env

let find_var env id =
  try Ident.find_same id env
  with Not_found ->
    Format.eprintf "%s@." (Ident.unique_name id);
    fatal_error ("Flambdagen.close: var " ^ Ident.unique_name id)

(* CR mshinwell for pchambart: We should establish conventions for various
   kinds of identifier and make sure that we stick to them everywhere.  Some
   that come to mind: "function identifier" or something (the [f] in
   [let f x = ...], "closure bound var", etc *)

module Function_declaration : sig
  (* A value of type [t] is used to represent a declaration of a *single*
     function during closure conversion. *)
  type t

  val create
     : rec_ident:Ident.t option
    -> closure_bound_var:Variable.t
    -> kind:function_kind
    -> params:Ident.t list
    -> body:lambda
    -> t

  val rec_ident : t -> Ident.t

  (* CR mshinwell for pchambart: Please check these comments *)
  (* CR mshinwell for pchambart: Should improve the name of this function. *)
  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  val used_idents_by_function : t list -> IdentSet.t Variable.Map.t

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t list -> IdentSet.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are neither bound as a parameter nor function identifier.

     This function creates new [Variable.t] values for everything except the
     function identifiers. *)
  val closure_env_without_parameters : t list -> Variable.t Ident.tbl
end = struct
  type t = {
    (* CR mshinwell for pchambart: maybe the name [rec_ident] is misleading.
       What about if it is a simultaneous binding but not recursive?
       Maybe it should be called "let_rec_ident". *)
    rec_ident : Ident.t;
    closure_bound_var : Variable.t;
    kind : function_kind;
    params : Ident.t list;
    body : lambda;
  }

  let create ~rec_ident ~closure_bound_var ~kind ~params ~body =
    let rec_ident =
      match rec_ident with
      | None ->
        (* CR mshinwell for pchambart: Can this be called something other than
           "dummy"? *)
        Ident.create "dummy"
      | Some rec_ident -> rec_ident
    in
    { rec_ident;
      closure_bound_var;
      kind;
      params;
      body;
    }

  let rec_ident t = t.rec_ident

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let rec_idents ts =
    List.map (fun t -> t.rec_ident) ts

  (* All parameters of functions in [ts]. *)
  let all_params ts =
    List.concat (List.map (fun t -> t.params) ts)

  let used_idents_by_function ts =
    List.fold_right
      (fun {closure_bound_var; body; params} map ->
         Variable.Map.add closure_bound_var (Lambda.free_variables body) map)
      ts Variable.Map.empty

  let all_used_idents ts =
    Variable.Map.fold (fun _ -> IdentSet.union)
      (used_idents_by_function ts) IdentSet.empty

  let set_diff (from : IdentSet.t) (idents : Ident.t list) =
    List.fold_right IdentSet.remove idents from

  let all_free_idents ts =
    set_diff (set_diff (all_used_idents ts) (all_params ts)) (rec_idents ts)

  let closure_env_without_parameters ts =
    Ident.empty
    (* add recursive functions *)
    |> List.fold_right
      (fun d env -> add_var d.rec_ident d.closure_bound_var env)
      (function_declarations ts)
    (* add free variables *)
    |> IdentSet.fold
      (fun id env -> add_var id (create_var id) env)
      (all_free_idents ts)
end

let rec close_const = function
  | Const_base c -> Fconst(Fconst_base c, nid ~name:"cst" ())
  | Const_pointer c -> Fconst(Fconst_pointer c, nid ~name:"cstptr" ())
  | Const_immstring c -> Fconst(Fconst_immstring c, nid ~name:"immstring" ())
  | Const_float_array c -> Fconst(Fconst_float_array c, nid ~name:"float" ())
  | Const_block (tag, l) ->
      Fprim(Pmakeblock(tag, Asttypes.Immutable),
            List.map close_const l, Debuginfo.none, nid ~name:"cstblock" ())

let rec close t env = function
  | Lvar id ->
      let var = find_var env id in
      Fvar (var, nid ~name:(Format.asprintf "var_%a" Variable.print var) ())
  | Lconst cst -> close_const cst
  | Llet(str, id, lam, body) ->
      let str =
        match str with
        | Variable -> Assigned
        | _ -> Not_assigned in
      let var = create_var t id in
      Flet(str, var, close_named var env lam,
           close (add_var id var env) body, nid ~name:"let" ())
  | Lfunction(kind, params, body) ->
      let closure_bound_var =
        Variable.create ~current_compilation_unit "fun" in
      let decl =
        Function_declaration.create ~rec_ident:None ~closure_bound_var ~kind
          ~params ~body
      in
      Fclosure(
        { fu_closure = close_functions env [decl];
          fu_fun = Closure_id.wrap closure_bound_var;
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
          (fun (id,  _) env -> add_var id (create_var t id) env)
          defs env in
      let function_declarations =
        List.map
          (function
            | (rec_ident, Lfunction(kind, params, body)) ->
                let closure_bound_var = create_var t rec_ident in
                let function_declaration =
                  Function_declaration.create ~rec_ident:(Some rec_ident)
                    ~closure_bound_var ~kind ~params ~body
                in
                Some function_declaration
            | _ -> None)
          defs
      in
      begin match Misc.some_if_all_elements_are_some function_declarations with
      | None ->
          let fdefs =
            List.map
              (fun (id, def) ->
                 let var = find_var env id in
                 (var, close_named ~rec_ident:id var env def))
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
                 let rec_ident = Function_declaration.rec_ident decl in
                 let let_bound_var = find_var env rec_ident in
                 Flet(Not_assigned, let_bound_var,
                      Fclosure(
                        { fu_closure = Fvar (clos_var, nid ());
                          fu_fun = Closure_id.wrap decl.closure_bound_var;
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
  | Lprim(Praise kind, [Levent(arg, ev)]) ->
      let arg = close env arg in
      Fprim(Praise kind, [arg], Debuginfo.from_raise ev, nid ())
  | Lprim(Pfield i, [Lprim(Pgetglobal id, [])])
    when Ident.same id current_unit_id ->
      Fprim(Pgetglobalfield(id,i), [], Debuginfo.none,
            nid ~name:"getglobalfield" ())
  | Lprim(Psetfield(i,_), [Lprim(Pgetglobal id, []); lam]) ->
      assert(Ident.same id current_unit_id);
      Fprim(Psetglobalfield i, [close env lam], Debuginfo.none,
            nid ~name:"setglobalfield" ())
  | Lprim(Pgetglobal id, [])
    when not (Ident.is_predef_exn id) ->
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
  | Lstringswitch(arg, sw, def) ->
      Fstringswitch(
        close env arg,
        List.map (fun (s, e) -> s, close env e) sw,
        Misc.may_map (close env) def,
        nid ~name:"stringswitch" ())
  | Lstaticraise (i, args) ->
      Fstaticraise (Static_exception.of_int i, close_list env args, nid ())
  | Lstaticcatch(body, (i, ids), handler) ->
      let vars = List.map create_var t ids in
      Fstaticcatch (Static_exception.of_int i, vars,
                    close env body, close (add_vars ids vars env) handler,
                    nid ())
  | Ltrywith(body, id, handler) ->
      let var = create_var t id in
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
      let var = create_var t id in
      Ffor(var, close env lo, close env hi, dir,
           close (add_var id var env) body,
           nid ())
  | Lassign(id, lam) ->
      Fassign(find_var env id, close env lam, nid ())
  | Levent(lam, ev) ->
      add_debug_info ev (close env lam)
  | Lifused _ ->
      assert false

and close_functions external_env function_declarations =
  let close_one_function map { closure_bound_var; kind; params; body } =
    let dbg = match body with
      | Levent (_,({lev_kind=Lev_function} as ev)) ->
          Debuginfo.from_call ev
      | _ -> Debuginfo.none in
    let closure_env =
      List.fold_right
        (fun id env -> add_var id (create_var t id) env)
        params closure_env_without_parameters in
    (* If the function is a wrapper function: force inline *)
    let stub, body =
      match body with
      | Lprim(Pccall { Primitive.prim_name = "*stub*" }, [body]) ->
          true, body
      | _ -> false, body
    in
    let params = List.map (find_var closure_env) params in
    let fun_decl =
      { stub; params; dbg;
        free_variables =
          IdentSet.fold
            (fun id set -> Variable.Set.add (find_var closure_env id) set)
            (Variable.Map.find closure_bound_var used_idents_per_function)
            Variable.Set.empty;
        body = close closure_env body } in
    match kind with
    | Curried ->
        Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
        let tuplified_version = rename_var t closure_bound_var in
        let generic_function_stub =
          tupled_function_call_stub
            closure_bound_var params tuplified_version in
        let map = Variable.Map.add closure_bound_var generic_function_stub map in
        Variable.Map.add tuplified_version fun_decl map
  in
  let fun_decls =
    { ident = Set_of_closures_id.create current_compilation_unit;
      funs =
        List.fold_left close_one_function Variable.Map.empty function_declarations;
      compilation_unit = current_compilation_unit } in
  let closure =
    { cl_fun = fun_decls;
      cl_free_var =
        IdentSet.fold
          (fun id map ->
             let internal_var = find_var closure_env_without_parameters id in
             let external_var = find_var external_env id in
             Variable.Map.add internal_var (Fvar(external_var, nid ())) map)
          all_free_idents Variable.Map.empty;
      cl_specialised_arg = Variable.Map.empty } in

  Fset_of_closures (closure, nid ())

and tupled_function_call_stub id original_params tuplified_version =
  let tuple_param =
    Variable.create ~current_compilation_unit "tupled_stub_param" in
  let params = List.map (fun p -> rename_var t p) original_params in
  let call = Fapply(
      { ap_function = Fvar(tuplified_version,nid ());
        ap_arg = List.map (fun p' -> Fvar(p',nid ())) params;
        ap_kind = Direct (Closure_id.wrap tuplified_version);
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
    free_variables = Variable.Set.of_list [tuple_param;tuplified_version];
    body;
    dbg = Debuginfo.none }

and close_list sb l = List.map (close sb) l

and close_named ?rec_ident let_bound_var env = function
  | Lfunction(kind, params, body) ->
      let closure_bound_var = rename_var t let_bound_var in
      let decl =
        Function_declaration.create ~rec_ident
          ~closure_bound_var:(rename_var t let_bound_var)
          ~kind ~params ~body
      in
      Fclosure(
        { fu_closure = close_functions env [decl];
          fu_fun = Closure_id.wrap closure_bound_var;
          fu_relative_to = None },
        nid ~name:"function" ())
  | lam ->
      close env lam

let lambda_to_flambda ~current_compilation_unit ~current_unit_id
    ~symbol_for_global' lam =
  let t =
    { current_compilation_unit;
      current_unit_id;
      symbol_for_global';
      debugger_map = ref Variable.Map.empty;
    }
  in
  (* Strings are the only expressions that can't be duplicated without
     changing the semantics. So we lift them to toplevel to avoid
     having to handle special cases later.
     There is no runtime cost to this transformation: strings are
     constants, they will not appear in the closures *)
  let lam = Lift_strings.lift_strings_to_toplevel lam in
  let flam = close t Ident.empty lam in
  t.debugger_map, flam
