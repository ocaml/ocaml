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

(* Naming conventions used in this module:
   - All variable names containing "id" or "ident" are of type [Ident.t] or
     are collections of [Ident.t].  These refer to identifiers for the type
     [Lambda.lambda], i.e. before closure conversion.
   - All variable names containing "var" are of type [Variable.t] or are
     collections of [Variable.t].  These refer to identifiers for the
     type [Flambda.t], i.e. after closure conversion.

  There is one exception: some [Ident.t] values refer to variables of
  [Lambda.lambda].  These are the module identifiers appearing in the
  constructions [Pgetglobal], [Pgetglobalfield] and [Psetglobalfield].
  These constructions also appear in [Flambda.t].
*)

type t = {
  current_compilation_unit : Symbol.Compilation_unit.t;
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  exported_fields : int;
}

type env = {
  variables : Variable.t Ident.tbl;
  static_exceptions : Static_exception.t Ext_types.Int.Map.t;
}

let empty_env = {
  variables = Ident.empty;
  static_exceptions = Ext_types.Int.Map.empty;
}

let nid = Expr_id.create

let fresh_variable t name =
  Variable.create name ~current_compilation_unit:t.current_compilation_unit

let create_var t id =
  let var = fresh_variable t (Ident.name id) in
  var

let rename_var t ?append var =
  let current_compilation_unit = t.current_compilation_unit in
  let new_var = Variable.rename ~current_compilation_unit ?append var in
  new_var

let add_var id var env = { env with variables = Ident.add id var env.variables }
let add_vars ids vars env = List.fold_right2 add_var ids vars env

let find_var env id =
  try Ident.find_same id env.variables
  with Not_found ->
    fatal_error ("Closure_conversion.find_var: var " ^ Ident.unique_name id)

let add_static_exception st_exn fresh_st_exn env =
  { env with
    static_exceptions =
      Ext_types.Int.Map.add st_exn fresh_st_exn env.static_exceptions }

let find_static_exception env st_exn =
  try Ext_types.Int.Map.find st_exn env.static_exceptions
  with Not_found ->
    fatal_error ("Closure_conversion.find_static_exception: exn "
      ^ string_of_int st_exn)

module Function_decls : sig
  (* Used to represent information about a set of function declarations
     during closure conversion.  (The only case in which such a set may
     contain more than one declaration is when processing "let rec".) *)

  module Function_decl : sig
    type t

    val create
       : let_rec_ident:Ident.t option
      -> closure_bound_var:Variable.t
      -> kind:function_kind
      -> params:Ident.t list
      -> body:lambda
      -> t

    val let_rec_ident : t -> Ident.t
    val closure_bound_var : t -> Variable.t
    val kind : t -> function_kind
    val params : t -> Ident.t list
    val body : t -> lambda

    (* [primitive_wrapper t] is [None] iff [t] is not a wrapper for a function
       with default optionnal arguments. Otherwise it is [Some body], where
       [body] is the body of the wrapper. *)
    val primitive_wrapper : t -> lambda option

    (* Like [used_idents_by_function], but for just one function. *)
    val used_idents : t -> IdentSet.t
  end

  type t = Function_decl.t list

  val create : Function_decl.t list -> t
  val to_list : t -> Function_decl.t list

  (* CR mshinwell for pchambart: Should improve the name of this function.
     How about "free_variables_in_body"?
     pchambart: I wanted to avoid mixing 'ident' and 'var' names. This one
       returns sets of idents. I'm not sure but maybe "free_idents_in_body"
       isn't too strange.
       Also "free_variables_in_body" would suggest that we look at a
       single function. maybe a plural ?
  *)
  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  val used_idents_by_function : t -> IdentSet.t Variable.Map.t

  (* All identifiers free in the given function declarations after the binding
     of parameters and function identifiers has been performed. *)
  val all_free_idents : t -> IdentSet.t

  (* A map from identifiers to their corresponding [Variable.t]s whose domain
     is the set of all identifiers free in the bodies of the declarations that
     are not bound as parameters.

     This function creates new [Variable.t] values for everything except the
     function identifiers by using the supplied [create_var] function. *)
  val closure_env_without_parameters
     : t
    -> create_var:(Ident.t -> Variable.t)
    -> env
end = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_bound_var : Variable.t;
      kind : function_kind;
      params : Ident.t list;
      body : lambda;
    }

    let create ~let_rec_ident ~closure_bound_var ~kind ~params ~body =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        closure_bound_var;
        kind;
        params;
        body;
      }

    let let_rec_ident t = t.let_rec_ident
    let closure_bound_var t = t.closure_bound_var
    let kind t = t.kind
    let params t = t.params
    let body t = t.body
    let used_idents t = Lambda.free_variables t.body

    (* CR-someday mshinwell: eliminate "*stub*" *)
    let primitive_wrapper t =
      match t.body with
      | Lprim (Pccall { Primitive.prim_name = "*stub*" }, [body]) -> Some body
      | _ -> None
  end

  type t = Function_decl.t list

  let create t = t
  let to_list t = t

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents t = List.map Function_decl.let_rec_ident t

  (* All parameters of functions in [ts]. *)
  let all_params t = List.concat (List.map Function_decl.params t)

  let used_idents_by_function t =
    List.fold_right (fun decl map ->
        Variable.Map.add (Function_decl.closure_bound_var decl)
          (Function_decl.used_idents decl) map)
      t Variable.Map.empty

  let all_used_idents t =
    Variable.Map.fold (fun _ -> IdentSet.union)
      (used_idents_by_function t) IdentSet.empty

  let set_diff (from : IdentSet.t) (idents : Ident.t list) =
    List.fold_right IdentSet.remove idents from

  let all_free_idents t =
    set_diff (set_diff (all_used_idents t) (all_params t)) (let_rec_idents t)

  let closure_env_without_parameters t ~create_var =
    let closure_env =
      (* For "let rec"-bound functions. *)
      List.fold_right (fun t env ->
          add_var (Function_decl.let_rec_ident t)
            (Function_decl.closure_bound_var t) env)
        t empty_env
    in
    (* For free variables. *)
    IdentSet.fold (fun id env -> add_var id (create_var id) env)
      (all_free_idents t) closure_env
end
module Function_decl = Function_decls.Function_decl

(* Generate a wrapper ("stub") function that accepts a tuple argument and
   calls another function with (curried) arguments extracted in the obvious
   manner from the tuple. *)
let tupled_function_call_stub t id original_params tuplified_version =
  let tuple_param =
    rename_var t ~append:"tupled_stub_param" tuplified_version
  in
  let params = List.map (fun p -> rename_var t p) original_params in
  let call =
    Fapply ({
        ap_function = Fvar (tuplified_version, nid ());
        ap_arg = List.map (fun p' -> Fvar (p', nid ())) params;
        ap_kind = Direct (Closure_id.wrap tuplified_version);
        ap_dbg = Debuginfo.none;
      },
      nid ())
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam =
          Fprim (Pfield pos, [Fvar (tuple_param, nid ())],
            Debuginfo.none, nid ())
        in
        pos + 1, Flet (Not_assigned, param, lam, body, nid ()))
      (0, call) params
  in
  { stub = true;
    params = [tuple_param];
    free_variables = Variable.Set.of_list [tuple_param; tuplified_version];
    body;
    dbg = Debuginfo.none;
  }

(* Propagate an [Lev_after] debugging event into an adjacent Flambda node. *)
let rec add_debug_info ev flam =
  match ev.lev_kind with
  | Lev_after _ ->
    begin match flam with
    | Fapply (ap, v) ->
      Fapply ({ ap with ap_dbg = Debuginfo.from_call ev}, v)
    | Fprim (p, args, _dinfo, v) ->
      Fprim (p, args, Debuginfo.from_call ev, v)
    | Fsend (kind, flam1, flam2, args, _dinfo, v) ->
      Fsend (kind, flam1, flam2, args, Debuginfo.from_call ev, v)
    | Fsequence (flam1, flam2, v) ->
      Fsequence (flam1, add_debug_info ev flam2, v)
    | _ -> flam
    end
  | _ -> flam

let rec close_const = function
  | Const_base c -> Fconst (Fconst_base c, nid ~name:"cst" ())
  | Const_pointer c -> Fconst (Fconst_pointer c, nid ~name:"cstptr" ())
  | Const_immstring c -> Fconst (Fconst_immstring c, nid ~name:"immstring" ())
  | Const_float_array c -> Fconst (Fconst_float_array c, nid ~name:"float" ())
  | Const_block (tag, l) ->
    Fprim (Pmakeblock (tag, Asttypes.Immutable),
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
      Flet(let_kind, var, close_let_bound_expression t var env lam,
           close t (add_var id var env) body, nid ~name:"let" ())
  | Lfunction(kind, params, body) ->
      let closure_bound_var =
        let name = match body with
          | Levent(_,{ lev_loc }) ->
            Format.asprintf "anon-fn[%a]" Location.print_compact lev_loc
          | _ -> "anon-fn" in
        fresh_variable t name
      in
      let decl =
        Function_decl.create ~let_rec_ident:None ~closure_bound_var ~kind
          ~params ~body
      in
      let decls = Function_decls.create [decl] in
      Fclosure(
        { fu_closure = close_functions t env decls;
          fu_fun = Closure_id.wrap closure_bound_var;
          fu_relative_to = None },
        nid ~name:"function" ())
  | Lapply(funct, args, loc) ->
      lift_apply_construction_to_variables t ~env ~funct ~args
  | Lletrec(defs, body) ->
      let env =
        List.fold_right
          (fun (id,  _) env -> add_var id (create_var t id) env)
          defs env in
      let function_declarations =
        (* Identify any bindings in the [let rec] that are functions. *)
        List.map (function
            | (let_rec_ident, Lfunction(kind, params, body)) ->
                let closure_bound_var = create_var t let_rec_ident in
                let function_declaration =
                  Function_decl.create ~let_rec_ident:(Some let_rec_ident)
                    ~closure_bound_var ~kind ~params ~body
                in
                Some function_declaration
            | _ -> None)
          defs
      in
      begin match Misc.some_if_all_elements_are_some function_declarations with
      | None ->
          (* If a branch is not a function we build a letrec expression
             where every function has its own closure. *)
          let flambda_defs =
            List.map
              (fun (id, def) ->
                 let var = find_var env id in
                 let expr =
                   close_let_bound_expression t ~let_rec_ident:id var env def
                 in
                 (var, expr))
              defs in
          Fletrec(flambda_defs, close t env body, nid ~name:"letrec" ())
      | Some function_declarations ->
          (* When all the bindings are functions, we build a single set of
             closures for all the functions. *)
          let set_of_closures =
            close_functions t env
              (Function_decls.create function_declarations)
          in
          let set_of_closures_var = fresh_variable t "set_of_closures" in
          let body =
            List.fold_left (fun body decl ->
                let let_rec_ident = Function_decl.let_rec_ident decl in
                let closure_bound_var = Function_decl.closure_bound_var decl in
                let let_bound_var = find_var env let_rec_ident in
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
      let exported =
        if i < t.exported_fields then
          Exported
        else
          Not_exported
      in
      Fprim(Psetglobalfield (exported, i), [close t env lam], Debuginfo.none,
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
      Fstaticraise (find_static_exception env i, close_list t env args, nid ())
  | Lstaticcatch(body, (i, ids), handler) ->
      let st_exn = Static_exception.create () in
      let env = add_static_exception i st_exn env in
      let vars = List.map (create_var t) ids in
      Fstaticcatch (st_exn, vars,
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

(* Perform closure conversion on a set of function declarations.  (The set
   will often only contain a single function; the only case where it cannot
   is for "let rec".) *)
and close_functions t external_env function_declarations =
  let closure_env_without_parameters =
    Function_decls.closure_env_without_parameters function_declarations
      ~create_var:(create_var t)
  in
  let all_free_idents =
    Function_decls.all_free_idents function_declarations
  in
  let close_one_function map decl =
    let body = Function_decl.body decl in
    let dbg =
      (* Move any debugging event that may exist at the start of the function
         body onto the function declaration itself. *)
      match body with
      | Levent (_, ({lev_kind = Lev_function} as ev)) -> Debuginfo.from_call ev
      | _ -> Debuginfo.none
    in
    let params = Function_decl.params decl in
    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.used_idents]; the results of
       that renaming are stored in [free_variables]. *)
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
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
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
    | Curried -> Variable.Map.add closure_bound_var fun_decl map
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
          (Function_decls.to_list function_declarations);
      compilation_unit = t.current_compilation_unit;
    }
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    { cl_fun = fun_decls;
      cl_free_var =
        IdentSet.fold
          (fun id map ->
             let internal_var = find_var closure_env_without_parameters id in
             let external_var = find_var external_env id in
             Variable.Map.add internal_var (Fvar (external_var, nid ())) map)
          all_free_idents Variable.Map.empty;
      cl_specialised_arg = Variable.Map.empty;
    }
  in
  Fset_of_closures (set_of_closures, nid ())

and close_list t sb l = List.map (close t sb) l

(* Ensure that [let] and [let rec]-bound functions have appropriate names. *)
and close_let_bound_expression t ?let_rec_ident let_bound_var env = function
  | Lfunction (kind, params, body) ->
    let closure_bound_var = rename_var t let_bound_var in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~body
    in
    Fclosure ({
        fu_closure = close_functions t env [decl];
        fu_fun = Closure_id.wrap closure_bound_var;
        fu_relative_to = None;
      },
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
  let block_fields, lets = lifting_helper t ~env ~args ~name:"block_field" in
  let block =
    Fprim (primitive, block_fields, Debuginfo.none, nid ~name:"block" ())
  in
  List.fold_left (fun body (v, expr) ->
      Flet(Not_assigned, v, expr, body, nid ()))
    block lets

(* Enforce right-to-left evaluation of function arguments by lifting the
   expressions computing the arguments into [let]s. *)
and lift_apply_construction_to_variables t ~env ~funct ~args =
  let apply_args, lets = lifting_helper t ~env ~args ~name:"apply_arg" in
  let apply =
    Fapply ({
        ap_function = close t env funct;
        ap_arg = apply_args;
        ap_kind = Indirect;
        ap_dbg = Debuginfo.none;
      },
      nid ~name:"apply" ())
  in
  List.fold_left (fun body (v, expr) ->
      Flet(Not_assigned, v, expr, body, nid ()))
    apply lets

and lifting_helper t ~env ~args ~name =
  List.fold_right (fun lam (args, lets) ->
      match close t env lam with
      | Fvar (v, _) as e ->
        (* Assumes that [v] is an immutable variable, otherwise this may
           change the evaluation order. *)
        (* CR mshinwell for pchambart: Please justify why [v] is always
           immutable. *)
        e::args, lets
      | expr ->
        let v = fresh_variable t name in
        Fvar (v, nid ())::args, (v, expr)::lets)
    args ([], [])

let lambda_to_flambda ~current_compilation_unit
    ~symbol_for_global'
    ~(exported_fields:int)
    lam =
  let t =
    { current_compilation_unit;
      current_unit_id =
        Symbol.Compilation_unit.get_persistent_ident current_compilation_unit;
      symbol_for_global';
      exported_fields;
    }
  in
  (* Strings are the only expressions that can't be duplicated without
     changing the semantics.  So we lift them to the toplevel to avoid
     having to handle special cases later.
     There is no runtime cost to this transformation: strings are
     constants and will not appear in closures. *)
  let lam = Lift_strings.lift_strings_to_toplevel lam in
  let flam = close t empty_env lam in
  flam
