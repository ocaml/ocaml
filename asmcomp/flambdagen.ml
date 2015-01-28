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
  mutable debugger_map : Ident.t Variable.Map.t;
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

let fresh_variable t name =
  Variable.create name ~current_compilation_unit:t.current_compilation_unit

let create_var t id =
  let var = fresh_variable t (Ident.name id) in
  t.debugger_map <- Variable.Map.add var id t.debugger_map;
  var

let rename_var t var =
  let current_compilation_unit = t.current_compilation_unit in
  let new_var = Variable.rename ~current_compilation_unit var in
  begin
    try
      let id = Variable.Map.find var t.debugger_map in
      t.debugger_map <- Variable.Map.add new_var id t.debugger_map
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

module Function_decl : sig
  (* A value of type [t] is used to represent a declaration of a *single*
     function during closure conversion. *)
  (* CR mshinwell: Maybe this isn't quite the right abstraction.  The "t list"
     business seems slightly dubious. *)
  type t

  val create
     : rec_ident:Ident.t option
    -> closure_bound_var:Variable.t
    -> kind:function_kind
    -> params:Ident.t list
    -> body:lambda
    -> t

  val rec_ident : t -> Ident.t
  val closure_bound_var : t -> Variable.t
  val kind : t -> function_kind
  val params : t -> Ident.t list
  val body : t -> lambda

  (* [primitive_wrapper t] is [None] iff [t] is not an eta-expansion wrapper
     for a primitive.  Otherwise it is [Some body], where [body] is the body of
     the wrapper. *)
  val primitive_wrapper : t -> lambda option

  (* CR mshinwell for pchambart: Please check these comments *)
  (* CR mshinwell for pchambart: Should improve the name of this function.
     How about "free_variables_in_body"?
  *)
  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  val used_idents_by_function : t list -> IdentSet.t Variable.Map.t

  (* Like [used_idents_by_function], but for just one function. *)
  val used_idents : t -> IdentSet.t

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t list -> IdentSet.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are not bound as parameters.

     This function creates new [Variable.t] values for everything except the
     function identifiers by using the supplied [create_var] function. *)
  val closure_env_without_parameters
     : t list
    -> create_var:(Ident.t -> Variable.t)
    -> Variable.t Ident.tbl
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
  let closure_bound_var t = t.closure_bound_var
  let kind t = t.kind
  let params t = t.params
  let body t = t.body

  (* CR-someday mshinwell: eliminate "*stub*" *)
  let primitive_wrapper t =
    match t.body with
    | Lprim (Pccall { Primitive.prim_name = "*stub*" }, [body]) -> Some body
    | _ -> None

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let rec_idents ts =
    List.map (fun t -> t.rec_ident) ts

  (* All parameters of functions in [ts]. *)
  let all_params ts =
    List.concat (List.map (fun t -> t.params) ts)

  let used_idents_by_function ts =
    List.fold_right
      (fun { closure_bound_var; body; } map ->
         Variable.Map.add closure_bound_var (Lambda.free_variables body) map)
      ts Variable.Map.empty

  let all_used_idents ts =
    Variable.Map.fold (fun _ -> IdentSet.union)
      (used_idents_by_function ts) IdentSet.empty

  let used_idents t =
    Lambda.free_variables t.body

  let set_diff (from : IdentSet.t) (idents : Ident.t list) =
    List.fold_right IdentSet.remove idents from

  let all_free_idents ts =
    set_diff (set_diff (all_used_idents ts) (all_params ts)) (rec_idents ts)

  let closure_env_without_parameters ts ~create_var =
    Ident.empty
    (* add recursive functions *)
    |> List.fold_right
      (fun t env -> add_var t.rec_ident t.closure_bound_var env)
      ts
    (* add free variables *)
    |> IdentSet.fold
      (fun id env -> add_var id (create_var id) env)
      (all_free_idents ts)
end

let tupled_function_call_stub t id original_params tuplified_version =
  (* CR mshinwell for pchambart: This should carry the name of the original
     variable (for debugging information output). *)
  let tuple_param = fresh_variable t "tupled_stub_param" in
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
  | Llet(let_kind, id, lam, body) ->
      let let_kind =
        match let_kind with
        | Variable -> Assigned
        | Strict | Alias | StrictOpt -> Not_assigned
      in
      let var = create_var t id in
      Flet(let_kind, var, close_named t var env lam,
           close t (add_var id var env) body, nid ~name:"let" ())
  | Lfunction(kind, params, body) ->
      (* CR-someday mshinwell: Identifiers should have proper names.  If
         the function is anonymous, the location can be used. *)
      let closure_bound_var = fresh_variable t "fun" in
      let decl =
        Function_decl.create ~rec_ident:None ~closure_bound_var ~kind
          ~params ~body
      in
      Fclosure(
        { fu_closure = close_functions t env [decl];
          fu_fun = Closure_id.wrap closure_bound_var;
          fu_relative_to = None },
        nid ~name:"function" ())
  | Lapply(funct, args, loc) ->
      Fapply(
        { ap_function = close t env funct;
          ap_arg = close_list t env args;
          ap_kind = Indirect;
          ap_dbg = Debuginfo.none },
        nid ~name:"apply" ())
  | Lletrec(defs, body) ->
      let env =
        List.fold_right
          (fun (id,  _) env -> add_var id (create_var t id) env)
          defs env in
      let function_declarations =
        (* Identify any bindings in the [let rec] that are functions. *)
        List.map (function
            | (rec_ident, Lfunction(kind, params, body)) ->
                let closure_bound_var = create_var t rec_ident in
                let function_declaration =
                  Function_decl.create ~rec_ident:(Some rec_ident)
                    ~closure_bound_var ~kind ~params ~body
                in
                Some function_declaration
            | _ -> None)
          defs
      in
      begin match Misc.some_if_all_elements_are_some function_declarations with
      | None ->
          (* CR mshinwell for pchambart: add comment mirroring the one
             below *)
          let fdefs =
            List.map
              (fun (id, def) ->
                 let var = find_var env id in
                 (var, close_named t ~rec_ident:id var env def))
              defs in
          Fletrec(fdefs, close t env body, nid ~name:"letrec" ())
      | Some function_declarations ->
          (* When all the bindings are functions, we build a single set of
             closures for all the functions. *)
          let set_of_closures = close_functions t env function_declarations in
          let set_of_closures_var = fresh_variable t "set_of_closures" in
          let body =
            List.fold_left (fun body decl ->
                let rec_ident = Function_decl.rec_ident decl in
                let closure_bound_var = Function_decl.closure_bound_var decl in
                let let_bound_var = find_var env rec_ident in
                Flet(Not_assigned, let_bound_var,
                     Fclosure(
                       { fu_closure = Fvar (set_of_closures_var, nid ());
                         fu_fun = Closure_id.wrap closure_bound_var;
                         fu_relative_to = None },
                       nid ()),
                     body, nid ()))
              (close t env body) function_declarations in
          Flet(Not_assigned, set_of_closures_var, set_of_closures,
               body, nid ~name:"closure_letrec" ())
      end
  | Lsend(kind, met, obj, args, _) ->
      Fsend(kind, close t env met, close t env obj,
            close_list t env args, Debuginfo.none, nid ())
  | Lprim(Pidentity, [arg]) ->
      close t env arg
  | Lprim(Pdirapply loc, [funct; arg]) | Lprim(Prevapply loc, [arg; funct]) ->
      close t env (Lapply(funct, [arg], loc))
  | Lprim(Praise kind, [Levent(arg, ev)]) ->
      Fprim(Praise kind, [close t env arg], Debuginfo.from_raise ev, nid ())
  | Lprim(Pfield i, [Lprim(Pgetglobal id, [])])
    when Ident.same id t.current_unit_id ->
      Fprim(Pgetglobalfield(id,i), [], Debuginfo.none,
            nid ~name:"getglobalfield" ())
  | Lprim(Psetfield(i,_), [Lprim(Pgetglobal id, []); lam]) ->
      assert(Ident.same id t.current_unit_id);
      Fprim(Psetglobalfield i, [close t env lam], Debuginfo.none,
            nid ~name:"setglobalfield" ())
  | Lprim(Pgetglobal id, [])
    when not (Ident.is_predef_exn id) ->
      assert(not (Ident.same id t.current_unit_id));
      let symbol = t.symbol_for_global' id in
      Fsymbol (symbol,nid ~name:"external_global" ())
  | Lprim(Pmakeblock _ as primitive, args) ->
      lift_block_construction_to_variables t ~env ~primitive ~args
  | Lprim(p, args) ->
      Fprim(p, close_list t env args, Debuginfo.none, nid ~name:"prim" ())
  | Lswitch(arg, sw) ->
      let aux (i, lam) = i, close t env lam in
      let zero_to_n = Ext_types.IntSet.zero_to_n in
      Fswitch(close t env arg,
              { fs_numconsts = zero_to_n (sw.sw_numconsts - 1);
                fs_consts = List.map aux sw.sw_consts;
                fs_numblocks = zero_to_n (sw.sw_numblocks - 1);
                fs_blocks = List.map aux sw.sw_blocks;
                fs_failaction = Misc.may_map (close t env) sw.sw_failaction },
              nid ~name:"switch" ())
  | Lstringswitch(arg, sw, def) ->
      Fstringswitch(
        close t env arg,
        List.map (fun (s, e) -> s, close t env e) sw,
        Misc.may_map (close t env) def,
        nid ~name:"stringswitch" ())
  | Lstaticraise (i, args) ->
      Fstaticraise (Static_exception.of_int i, close_list t env args, nid ())
  | Lstaticcatch(body, (i, ids), handler) ->
      let vars = List.map (create_var t) ids in
      Fstaticcatch (Static_exception.of_int i, vars,
                    close t env body, close t (add_vars ids vars env) handler,
                    nid ())
  | Ltrywith(body, id, handler) ->
      let var = create_var t id in
      Ftrywith(close t env body, var, close t (add_var id var env) handler,
        nid ())
  | Lifthenelse(arg, ifso, ifnot) ->
      Fifthenelse(close t env arg, close t env ifso, close t env ifnot,
                  nid ~name:"if" ())
  | Lsequence(lam1, lam2) ->
      Fsequence(close t env lam1, close t env lam2, nid ~name:"seq" ())
  | Lwhile(cond, body) ->
      Fwhile(close t env cond, close t env body, nid ())
  | Lfor(id, lo, hi, dir, body) ->
      let var = create_var t id in
      Ffor(var, close t env lo, close t env hi, dir,
           close t (add_var id var env) body, nid ())
  | Lassign(id, lam) ->
      Fassign(find_var env id, close t env lam, nid ())
  | Levent(lam, ev) ->
      add_debug_info ev (close t env lam)
  | Lifused _ ->
      assert false

and close_functions t external_env function_declarations =
  let closure_env_without_parameters =
    Function_decl.closure_env_without_parameters function_declarations
      ~create_var:(create_var t)
  in
  let all_free_idents =
    Function_decl.all_free_idents function_declarations
  in
  let close_one_function map decl =
    let body = Function_decl.body decl in
    let dbg = match body with
      | Levent (_,({lev_kind=Lev_function} as ev)) -> Debuginfo.from_call ev
      | _ -> Debuginfo.none in
    let params = Function_decl.params decl in
    (* Create fresh variables for the elements of the closure (i.e. the
       free variables of the body, minus the parameters).  This induces a
       renaming on [Function_decl.used_idents]; the results of that renaming
       are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun id env -> add_var id (create_var t id) env)
        params closure_env_without_parameters
    in
    let free_variables =
      IdentSet.fold
        (fun id set -> Variable.Set.add (find_var closure_env id) set)
        (Function_decl.used_idents decl)
        Variable.Set.empty
    in
    (* If the function is an eta-expansion wrapper for a primitive, make
       sure it always gets inlined. *)
    let stub, body =
      match Function_decl.primitive_wrapper decl with
      | None -> false, body
      | Some wrapper_body -> true, wrapper_body
    in
    let params = List.map (find_var closure_env) params in
    let closure_bound_var = Function_decl.closure_bound_var decl in
    let body = close t closure_env body in
    let fun_decl = { stub; params; dbg; free_variables; body; } in
    match Function_decl.kind decl with
    | Curried ->
        Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
        let tuplified_version = rename_var t closure_bound_var in
        let generic_function_stub =
          tupled_function_call_stub t closure_bound_var params tuplified_version
        in
        Variable.Map.add tuplified_version fun_decl
          (Variable.Map.add closure_bound_var generic_function_stub map)
  in
  let fun_decls =
    { ident = Set_of_closures_id.create t.current_compilation_unit;
      funs =
        List.fold_left close_one_function Variable.Map.empty
          function_declarations;
      compilation_unit = t.current_compilation_unit } in
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

and close_list t sb l = List.map (close t sb) l

(* CR mshinwell for pchambart: I know this name was taken from the existing
   code, but I think we should rename it.  It doesn't adequately express
   what's going on. *)
and close_named t ?rec_ident let_bound_var env = function
  | Lfunction(kind, params, body) ->
      let closure_bound_var = rename_var t let_bound_var in
      let decl =
        Function_decl.create ~rec_ident ~closure_bound_var ~kind ~params ~body
      in
      Fclosure(
        { fu_closure = close_functions t env [decl];
          fu_fun = Closure_id.wrap closure_bound_var;
          fu_relative_to = None },
        nid ~name:"function" ())
  | lam ->
      close t env lam

(* Transform a [Pmakeblock] operation, that allocates and fills a new block,
   to a sequence of [let]s.  The aim is to then eliminate the allocation of
   the block, so long as it does not escape.  For example,

     Pmakeblock [expr_0; ...; expr_n]

   is transformed to:

     let x_0 = expr_0 in
     ...
     let x_n = expr_n in
     Pmakeblock [x_0; ...; x_n]

   A more general solution would be to convert completely to ANF.
*)
and lift_block_construction_to_variables t ~env ~primitive ~args =
  let block_fields, lets =
    List.fold_right (fun lam (block, lets) ->
        match close t env lam with
        | Fvar (v, _) as e -> e::block, lets
        | expr ->
          let v = fresh_variable t "block_field" in
          Fvar (v, nid ())::block, (v, expr)::lets)
      args ([],[])
  in
  let block =
    Fprim (primitive, block_fields, Debuginfo.none, nid ~name:"block" ())
  in
  List.fold_left (fun body (v, expr) ->
      Flet(Not_assigned, v, expr, body, nid ()))
    block lets

let lambda_to_flambda ~current_compilation_unit ~current_unit_id
    ~symbol_for_global' lam =
  let t =
    { current_compilation_unit;
      current_unit_id;
      symbol_for_global';
      debugger_map = Variable.Map.empty;
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
