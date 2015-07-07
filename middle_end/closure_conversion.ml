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

module IdentSet = Lambda.IdentSet

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  exported_fields : int;
}

let fresh_variable ~name =
  Variable.create name
    ~current_compilation_unit:(Compilation_unit.get_current_exn ())

let create_var id =
  let var = fresh_variable ~name:(Ident.name id) in
  var

let rename_var _t ?append var =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let new_var = Variable.rename ~current_compilation_unit ?append var in
  new_var

let nid = Expr_id.create

module Env : sig
  (* Used to remember which [Variable.t] values correspond to which
     [Ident.t] values during closure conversion, and similarly for
     static exception identifiers. *)

  type t

  val empty : t

  val add_var : t -> Ident.t -> Variable.t -> t
  val add_vars : t -> Ident.t list -> Variable.t list -> t

  val find_var : t -> Ident.t -> Variable.t

  val add_static_exception : t -> int -> Static_exception.t -> t
  val find_static_exception : t -> int -> Static_exception.t
end = struct
  type t = {
    variables : Variable.t Ident.tbl;
    static_exceptions : Static_exception.t Ext_types.Int.Map.t;
  }

  let empty = {
    variables = Ident.empty;
    static_exceptions = Ext_types.Int.Map.empty;
  }

  let add_var t id var = { t with variables = Ident.add id var t.variables }
  let add_vars t ids vars = List.fold_left2 add_var t ids vars

  let find_var t id =
    try Ident.find_same id t.variables
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_var: var "
        ^ Ident.unique_name id)

  let add_static_exception t st_exn fresh_st_exn =
    { t with
      static_exceptions =
        Ext_types.Int.Map.add st_exn fresh_st_exn t.static_exceptions }

  let find_static_exception t st_exn =
    try Ext_types.Int.Map.find st_exn t.static_exceptions
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_static_exception: exn "
        ^ string_of_int st_exn)
end

module Function_decls : sig
  (* Used to represent information about a set of function declarations
     during closure conversion.  (The only case in which such a set may
     contain more than one declaration is when processing "let rec".) *)

  module Function_decl : sig
    type t

    val create
       : let_rec_ident:Ident.t option
      -> closure_bound_var:Variable.t
      -> kind:Lambda.function_kind
      -> params:Ident.t list
      -> body:Lambda.lambda
      -> t

    val let_rec_ident : t -> Ident.t
    val closure_bound_var : t -> Variable.t
    val kind : t -> Lambda.function_kind
    val params : t -> Ident.t list
    val body : t -> Lambda.lambda

    (* [primitive_wrapper t] is [None] iff [t] is not a wrapper for a function
       with default optionnal arguments. Otherwise it is [Some body], where
       [body] is the body of the wrapper. *)
    val primitive_wrapper : t -> Lambda.lambda option

    (* Like [used_idents_by_function], but for just one function. *)
    val used_idents : t -> IdentSet.t
  end

  type t = Function_decl.t list

  val create : Function_decl.t list -> t
  val to_list : t -> Function_decl.t list

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
    -> Env.t
end = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_bound_var : Variable.t;
      kind : Lambda.function_kind;
      params : Ident.t list;
      body : Lambda.lambda;
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
          Env.add_var env (Function_decl.let_rec_ident t)
            (Function_decl.closure_bound_var t))
        t Env.empty
    in
    (* For free variables. *)
    IdentSet.fold (fun id env -> Env.add_var env id (create_var id))
      (all_free_idents t) closure_env
end
module Function_decl = Function_decls.Function_decl

(* Generate a wrapper ("stub") function that accepts a tuple argument and
   calls another function with (curried) arguments extracted in the obvious
   manner from the tuple. *)
let tupled_function_call_stub t original_params tuplified_version
      : _ Flambda.function_declaration =
  let tuple_param =
    rename_var t ~append:"tupled_stub_param" tuplified_version
  in
  let params = List.map (fun p -> rename_var t p) original_params in
  let call : _ Flambda.t =
    Fapply ({
        func = Fvar (tuplified_version, nid ());
        args = params;
        kind = Direct (Closure_id.wrap tuplified_version);
        dbg = Debuginfo.none;
      },
      nid ())
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : _ Flambda.named =
          Fprim (Pfield pos, [tuple_param], Debuginfo.none, nid ())
        in
        pos + 1, Flambda.Flet (Immutable, param, lam, body, nid ()))
      (0, call) params
  in
  { stub = true;  (* force the function to be inlined *)
    params = [tuple_param];
    free_variables = Variable.Set.of_list [tuple_param; tuplified_version];
    body;
    dbg = Debuginfo.none;
  }

(* Propagate an [Lev_after] debugging event into an adjacent Flambda node. *)
let add_debug_info (ev : Lambda.lambda_event) (flam : _ Flambda.t)
      : _ Flambda.t =
  match ev.lev_kind with
  | Lev_after _ ->
    begin match flam with
    | Fapply (ap, v) ->
      Fapply ({ ap with dbg = Debuginfo.from_call ev}, v)
(* XXX resurrect this
    | Fprim (p, args, _dinfo, v) ->
      Fprim (p, args, Debuginfo.from_call ev, v)
*)
    | Fsend (kind, flam1, flam2, args, _dinfo, v) ->
      Fsend (kind, flam1, flam2, args, Debuginfo.from_call ev, v)
(*
    | Fsequence (flam1, flam2, v) ->
      Fsequence (flam1, add_debug_info ev flam2, v)
*)
    | _ -> flam
    end
  | _ -> flam

let close_const (const : Lambda.structured_constant) : _ Flambda.named =
   (* CR-soon mshinwell: consider changing [name] arguments to be uniform
     with the constructor names *)
   match const with
  | Const_base c -> Fconst (Fconst_base c, nid ~name:"cst" ())
  | Const_pointer c -> Fconst (Fconst_pointer c, nid ~name:"cstptr" ())
  | Const_immstring c -> Fconst (Fconst_immstring c, nid ~name:"immstring" ())
  | Const_float_array c -> Fconst (Fconst_float_array c, nid ~name:"float" ())
  | Const_block _ ->
    Misc.fatal_error "Const_block should have been eliminated \
        before closure conversion"

let name_expr (named : _ Flambda.named) : _ Flambda.t =
  let var = fresh_variable ~name:"named" in
  let nid1 = nid ~name:"named" () in
  let nid2 = nid ~name:"named" () in
  Flet (Immutable, var, named, Fvar (var, nid1), nid2)

let rec close t env (lam : Lambda.lambda) : _ Flambda.t =
  match lam with
  | Lvar id ->
    let var = Env.find_var env id in
    Fvar (var, nid ~name:(Format.asprintf "var_%a" Variable.print var) ())
  | Lconst cst -> name_expr (close_const cst)
  | Llet (let_kind, id, lam, body) ->
    let let_kind : Flambda.let_kind =
      match let_kind with
      | Variable -> Mutable
      | Strict | Alias | StrictOpt -> Immutable
    in
    let var = create_var id in
    let lam = close_let_bound_expression t var env lam in
    let body = close t (Env.add_var env id var) body in
    Flet (let_kind, var, lam, body, nid ~name:"let" ())
  | Lfunction (kind, params, body) ->
    let closure_bound_var =
      let name =
        (* Name anonymous functions by their source location, if known. *)
        match body with
        | Levent (_, { lev_loc }) ->
          Format.asprintf "anon-fn[%a]" Location.print_compact lev_loc
        | _ -> "anon-fn"
      in
      fresh_variable ~name
    in
    (* CR mshinwell: some of this is now very similar to the let rec case
       below *)
    let set_of_closures_var = fresh_variable ~name:"set_of_closures" in
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
    Flet (Immutable, set_of_closures_var, set_of_closures,
      name_expr (Fproject_closure (project_closure,
        nid ~name:"project_closure" ())),
      nid ~name:"function" ())
  | Lapply (funct, args, _loc) ->
    (* CR-someday mshinwell: the location should probably not be lost. *)
    Lift_code.lifting_helper (close_list t env args)
      ~evaluation_order:`Right_to_left
      ~name:"apply_arg"
      ~create_body:(fun args ->
        Fapply ({
            func = close t env funct;
            args;
            kind = Indirect;
            dbg = Debuginfo.none;
          },
          nid ~name:"apply" ()))
  | Lletrec (defs, body) ->
    let env =
      List.fold_right (fun (id,  _) env ->
          Env.add_var env id (create_var id))
        defs env
    in
    let function_declarations =
      (* Identify any bindings in the [let rec] that are functions.  These
         will be named after the corresponding identifier in the [let rec]. *)
      List.map (function
          | (let_rec_ident, Lambda.Lfunction (kind, params, body)) ->
            let closure_bound_var = create_var let_rec_ident in
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
         [Flet] that binds a set of closures containing all of the functions.
      *)
      let set_of_closures_var = fresh_variable ~name:"set_of_closures" in
      let set_of_closures =
        close_functions t env (Function_decls.create function_declarations)
      in
      let body =
        List.fold_left (fun body decl ->
            let let_rec_ident = Function_decl.let_rec_ident decl in
            let closure_bound_var = Function_decl.closure_bound_var decl in
            let let_bound_var = Env.find_var env let_rec_ident in
            (* Inside the body of the [let], each function is referred to by
               an [Fproject_closure] expression, which projects from the set of
               closures. *)
            ((Flet (Immutable, let_bound_var,
              Fproject_closure ({
                  set_of_closures = set_of_closures_var;
                  closure_id = Closure_id.wrap closure_bound_var;
                },
                nid ()),
              body, nid ())) : _ Flambda.t))
          (close t env body) function_declarations
      in
      Flet (Immutable, set_of_closures_var, set_of_closures,
        body, nid ~name:"closure_letrec" ())
    | None ->
      (* If the condition above is not satisfied, we build an [Fletrec]
         expression; any functions bound by it will have their own
         individual closures. *)
      let defs =
        List.map (fun (id, def) ->
            let var = Env.find_var env id in
            let expr =
              close_let_bound_expression t ~let_rec_ident:id var env def
            in
            var, expr)
          defs
      in
      Fletrec (defs, close t env body, nid ~name:"letrec" ())
    end
  | Lsend (kind, met, obj, args, _) ->
    Fsend (kind, close t env met, close t env obj,
      close_list t env args, Debuginfo.none, nid ())
  | Lprim (Psequor, [arg1; arg2]) ->
    let const_true = fresh_variable ~name:"const_true" in
    let name = "Psequand" in
    Flet (Immutable, const_true, Fconst (Fconst_base (Const_int 1)),
      Fifthenelse (arg1, const_true, arg2, nid ~name ()), nid ~name ())
  | Lprim (Psequand, [arg1; arg2]) ->
    let const_false = fresh_variable ~name:"const_true" in
    let name = "Psequor" in
    Flet (Immutable, const_false, Fconst (Fconst_base (Const_int 0)),
      Fifthenelse (arg1, arg2, const_false, nid ~name ()), nid ~name ())
  | Lprim (Psequand | Psequor, _) ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Lprim (Pidentity, [arg]) -> close t env arg
  | Lprim (Pdirapply loc, [funct; arg])
  | Lprim (Prevapply loc, [arg; funct]) ->
    close t env (Lambda.Lapply (funct, [arg], loc))
  | Lprim (Praise kind, [Levent (arg, event)]) ->
    let arg_var = fresh_variable ~name:"raise_arg" in
    Flet (Immutable, arg_var, Fexpr (close t env arg),
      name_expr
        (Fprim (Praise kind, [arg_var], Debuginfo.from_raise event, nid ())),
      nid ())
  | Lprim (Pfield i, [Lprim (Pgetglobal id, [])])
      when Ident.same id t.current_unit_id ->
    (* Access to globals of the current module uses a distinguished primitive
       in Flambda. *)
    name_expr (Fprim (Pgetglobalfield (id, i), [], Debuginfo.none,
      nid ~name:"getglobalfield" ()))
  | Lprim (Psetfield (i, _), [Lprim (Pgetglobal id, []); lam]) ->
    assert (Ident.same id t.current_unit_id);
    let exported : Lambda.exported =
      if i < t.exported_fields then Exported else Not_exported
    in
    let arg_var = fresh_variable ~name:"raise_arg" in
    Flet (Immutable, arg_var, Fexpr (close t env lam),
      name_expr (Fprim (Psetglobalfield (exported, i), [arg_var],
        Debuginfo.none, nid ~name:"setglobalfield" ())),
      nid ())
  | Lprim (Pgetglobal id, []) when not (Ident.is_predef_exn id) ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    name_expr (Fsymbol (symbol, nid ~name:"external_global" ()))
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
        name_expr (Fprim (p, args, Debuginfo.none, nid ~name:"prim" ())))
  | Lswitch (arg, sw) ->
    let aux (i, lam) = i, close t env lam in
    let zero_to_n = Ext_types.IntSet.zero_to_n in
    Fswitch (close t env arg,
      { numconsts = zero_to_n (sw.sw_numconsts - 1);
        consts = List.map aux sw.sw_consts;
        numblocks = zero_to_n (sw.sw_numblocks - 1);
        blocks = List.map aux sw.sw_blocks;
        failaction = Misc.may_map (close t env) sw.sw_failaction;
      },
      nid ~name:"switch" ())
  | Lstringswitch (arg, sw, def) ->
    Fstringswitch (
      close t env arg,
      List.map (fun (s, e) -> s, close t env e) sw,
      Misc.may_map (close t env) def,
      nid ~name:"stringswitch" ())
  | Lstaticraise (i, args) ->
    Fstaticraise (Env.find_static_exception env i, close_list t env args,
      nid ())
  | Lstaticcatch (body, (i, ids), handler) ->
    let st_exn = Static_exception.create () in
    let env = Env.add_static_exception env i st_exn in
    let vars = List.map (create_var) ids in
    Fstaticcatch (st_exn, vars, close t env body,
      close t (Env.add_vars env ids vars) handler, nid ())
  | Ltrywith (body, id, handler) ->
    let var = create_var id in
    Ftrywith (close t env body, var, close t (Env.add_var env id var) handler,
      nid ())
  | Lifthenelse (arg, ifso, ifnot) ->
    Fifthenelse (close t env arg, close t env ifso, close t env ifnot,
      nid ~name:"if" ())
  | Lsequence (lam1, lam2) ->
    let var = fresh_variable ~name:"sequence" in
    let lam1 = Flambda.Fexpr (close t env lam1) in
    let lam2 = close t env lam2 in
    Flet (Immutable, var, lam1, lam2, nid ~name:"sequence" ())
  | Lwhile (cond, body) -> Fwhile (close t env cond, close t env body, nid ())
  | Lfor (id, lo, hi, dir, body) ->
    let var = create_var id in
    Ffor (var, close t env lo, close t env hi, dir,
      close t (Env.add_var env id var) body, nid ())
  | Lassign (id, lam) -> Fassign (Env.find_var env id, close t env lam, nid ())
  | Levent (lam, ev) -> add_debug_info ev (close t env lam)
  (* XCR mshinwell for pchambart: What is [Lifused] and why is this
     [assert false]?
     pchambart: [Lifused] is used to mark that this expression should be alive
     only if an identifier is. Every uses should have been removed by
     [Simplif.simplify_lets], either by replacing by the inner expression, or
     by completely removing it (replacing by unit) *)
  | Lifused _ -> assert false

(* Perform closure conversion on a set of function declarations, returning a
   set of closures.  (The set will often only contain a single function;
   the only case where it cannot is for "let rec".) *)
and close_functions t external_env function_declarations : _ Flambda.named =
  let closure_env_without_parameters =
    Function_decls.closure_env_without_parameters function_declarations
      ~create_var:(create_var)
  in
  let all_free_idents = Function_decls.all_free_idents function_declarations in
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
      List.fold_right (fun id env -> Env.add_var env id (create_var id))
        params closure_env_without_parameters
    in
    let free_variables =
      IdentSet.fold
        (fun id set -> Variable.Set.add (Env.find_var closure_env id) set)
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
    let params = List.map (Env.find_var closure_env) params in
    let closure_bound_var = Function_decl.closure_bound_var decl in
    let body = close t closure_env body in
    let fun_decl : _ Flambda.function_declaration =
      { stub; params; dbg; free_variables; body; }
    in
    match Function_decl.kind decl with
    | Curried -> Variable.Map.add closure_bound_var fun_decl map
    | Tupled ->
      let tuplified_version = rename_var t closure_bound_var in
      let generic_function_stub =
        tupled_function_call_stub t params tuplified_version
      in
      Variable.Map.add tuplified_version fun_decl
        (Variable.Map.add closure_bound_var generic_function_stub map)
  in
  let fun_decls : _ Flambda.function_declarations =
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
  let set_of_closures : _ Flambda.set_of_closures =
    { function_decls = fun_decls;
      free_vars =
        IdentSet.fold (fun var map ->
            let internal_var =
              Env.find_var closure_env_without_parameters var
            in
            let external_var = Env.find_var external_env var in
            Variable.Map.add internal_var external_var map)
          all_free_idents Variable.Map.empty;
      specialised_args = Variable.Map.empty;
    }
  in
  Fset_of_closures (set_of_closures, nid ~name:"set_of_closures" ())

and close_list t sb l = List.map (close t sb) l

(* Ensure that [let] and [let rec]-bound functions have appropriate names. *)
and close_let_bound_expression t ?let_rec_ident let_bound_var env
      (lam : Lambda.lambda) : _ Flambda.named =
  match lam with
  | Lfunction (kind, params, body) ->
    let closure_bound_var = rename_var t let_bound_var in
    let decl =
      Function_decl.create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~body
    in
    let set_of_closures_var = fresh_variable ~name:"set_of_closures_var" in
    let set_of_closures = close_functions t env [decl] in
    let project_closure : Flambda.project_closure =
      { set_of_closures = set_of_closures_var;
        closure_id = Closure_id.wrap closure_bound_var;
      }
    in
    Fexpr (Flet (Immutable, set_of_closures_var, set_of_closures,
      name_expr (Fproject_closure (project_closure,
        nid ~name:"project_closure" ())),
      nid ~name:"close_let_bound_expression" ()))
  | lam ->
    match close t env lam with
    (* Remove unnecessary [let]s introduced by [name_expr], above. *)
    | Flet (Immutable, var1, named, Fvar (var2, _), _)
        when Variable.equal var1 var2 -> named
    | flam -> Fexpr flam

let lambda_to_flambda ~backend ~(exported_fields:int) lam =
  let module Backend = (val backend : Backend_intf.S) in
  let t =
    { current_unit_id =
        Compilation_unit.get_persistent_ident
          (Compilation_unit.get_current_exn ());
      symbol_for_global' = Backend.symbol_for_global';
      exported_fields;
    }
  in
  let flam = close t Env.empty lam in
  flam
