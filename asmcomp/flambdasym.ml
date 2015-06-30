(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
Transform an expression to prepare conversion to clambda
- attributes symbols to structured constants
- replace access to constants from the current compilation unit by Fsymbol nodes,
  including access to fields from a constant closure inside the body of the function.
- Find used closure fields and remove unused ones.
- build value approximations for export

During symbol assignment, some alias can be created (when building let rec for instance).
They are replaced by their canonical representent in the Prepare functor application.

Then the tables needed to build the Flambdaexport.exported type are build.
*)

module ET = Flambdaexport_types

let all_closures expr =
  let closures = ref Set_of_closures_id.Set.empty in
  Flambdaiter.iter_on_sets_of_closures (fun cl _ ->
      closures := Set_of_closures_id.Set.add
        cl.function_decls.set_of_closures_id !closures)
    expr;
  !closures

let constant_closures constant_result expr =
  Set_of_closures_id.Set.diff
    (all_closures expr)
    (constant_result.Inconstant_idents.closure)

let functions expr =
  let cf_map = ref Closure_id.Map.empty in
  let fun_id_map = ref Set_of_closures_id.Map.empty in
  let argument_kept = ref Set_of_closures_id.Map.empty in
  let aux (({ function_decls } as cl) : _ Flambda.set_of_closures) _ =
    let add var _ map =
      Closure_id.Map.add (Closure_id.wrap var) function_decls map in
    cf_map := Variable.Map.fold add function_decls.funs !cf_map;
    fun_id_map := Set_of_closures_id.Map.add
      cl.function_decls.set_of_closures_id cl.function_decls !fun_id_map;
    argument_kept :=
      Set_of_closures_id.Map.add cl.function_decls.set_of_closures_id
        (Invariant_params.unchanging_params_in_recursion function_decls)
        !argument_kept
  in
  Flambdaiter.iter_on_sets_of_closures aux expr;
  !fun_id_map, !cf_map, !argument_kept

let list_used_variable_within_closure expr =
  let used = ref Var_within_closure.Set.empty in
  let aux (expr : _ Flambda.t) =
    match expr with
    | Fproject_var({ var },_) ->
      used := Var_within_closure.Set.add var !used
    | _ -> ()
  in
  Flambdaiter.iter aux expr;
  !used

module type Param1 = sig
  type t
  val expr : t Flambda.t
  val inconstants : Inconstant_idents.result
  val constant_closures : Set_of_closures_id.Set.t
end

type const_sym =
  | Lbl of Symbol.t
  | No_lbl
  | Not_const
  | Const_closure

type infos = {
  global : (int, ET.approx) Hashtbl.t;
  ex_table : ET.descr Export_id.Map.t ref;
  ex_symbol_id : Export_id.t Symbol.Map.t ref;
  constants : unit Flambda.t Symbol.Tbl.t;
  symbol_alias : Symbol.t Symbol.Tbl.t;
}

let init_infos () =
  { global = Hashtbl.create 10;
    ex_table = ref Export_id.Map.empty;
    ex_symbol_id = ref Symbol.Map.empty;
    constants = Symbol.Tbl.create 10;
    symbol_alias = Symbol.Tbl.create 10 }

let rec canonical_symbol s infos =
  try
    let s' = Symbol.Tbl.find infos.symbol_alias s in
    let s'' = canonical_symbol s' infos in
    if s' != s''
    then Symbol.Tbl.replace infos.symbol_alias s s'';
    s''
  with Not_found -> s

let new_descr descr infos =
  let id = Export_id.create (Compilenv.current_unit ()) in
  infos.ex_table := Export_id.Map.add id descr !(infos.ex_table);
  id

module Conv(P:Param1) = struct
  let _functions, closures, ex_kept_arguments = functions P.expr

  (* functions comming from a linked module *)
  let ex_closures () =
    (Compilenv.approx_env ()).ET.ex_functions_off

  let used_variable_within_closure = list_used_variable_within_closure P.expr

  type ('a,'b) declaration_position =
    | Local of 'a
    | External of 'b
    | Not_declared

  let function_declaration_position cf =
    try Local (Closure_id.Map.find cf closures) with
    | Not_found ->
        try External (Closure_id.Map.find cf (ex_closures ())) with
        | Not_found ->
            Not_declared

  let is_local_function_constant cf =
    match function_declaration_position cf with
    | Local { set_of_closures_id } ->
      Set_of_closures_id.Set.mem set_of_closures_id P.constant_closures
    | External _ -> false
    | Not_declared ->
        Misc.fatal_error (Format.asprintf "missing closure %a"
                       Closure_id.print cf)

  let function_arity fun_id =
    let arity clos _off =
      Flambdautils.function_arity
        (Flambdautils.find_declaration fun_id clos)
    in
    try arity (Closure_id.Map.find fun_id closures) fun_id with
    | Not_found ->
        try arity (Closure_id.Map.find fun_id (ex_closures ())) fun_id with
        | Not_found ->
            Misc.fatal_error (Format.asprintf "missing closure %a"
                           Closure_id.print fun_id)

  let inconstants = P.inconstants
  let is_constant id = not (Variable.Set.mem id inconstants.Inconstant_idents.id)

  type env =
    { sb : unit Flambda.t Variable.Map.t; (* substitution *)
      cm : Symbol.t Variable.Map.t; (* variables associated to constants *)
      approx : ET.approx Variable.Map.t;
      toplevel : bool }

  let infos = init_infos ()

  let empty_env =
    { sb = Variable.Map.empty;
      cm = Variable.Map.empty;
      approx = Variable.Map.empty;
      toplevel = false }

  let canonical_symbol s = canonical_symbol s infos
  let set_symbol_alias s1 s2 =
    let s1' = canonical_symbol s1 in
    let s2' = canonical_symbol s2 in
    if s1' <> s2'
    then Symbol.Tbl.add infos.symbol_alias s1' s2'

  let add_sb id subst env =
    { env with sb = Variable.Map.add id subst env.sb }

  let add_cm id const env =
    { env with cm = Variable.Map.add id const env.cm }

  let copy_env id' id env =
    try
      let const = Variable.Map.find id env.cm in
      add_cm id' const env
    with Not_found -> env

  let add_global i approx =
    Hashtbl.add infos.global i approx
  let get_global i =
    try Hashtbl.find infos.global i
    with Not_found ->
      (* Value_unknown *)
      Misc.fatal_error (Format.asprintf "no global %i" i)

  let add_approx id approx env =
    { env with approx = Variable.Map.add id approx env.approx }
  let get_approx id env =
    try Variable.Map.find id env.approx
    with Not_found -> ET.Value_unknown

  let extern_symbol_descr sym =
    if Compilenv.is_predefined_exception sym
    then None
    else
      let export =
        Compilenv.approx_for_global (Symbol.compilation_unit sym)
      in
      try
        let id = Symbol.Map.find sym export.ex_symbol_id in
        let descr = Flambdaexport.find_description id export in
        Some descr
      with
      | Not_found -> None

  let extern_id_descr ex =
    let export = Compilenv.approx_env () in
    try Some (Flambdaexport.find_description ex export)
    with Not_found -> None

  let get_descr (approx : ET.approx) =
    match approx with
    | Value_unknown -> None
    | Value_id ex ->
        (try Some (Export_id.Map.find ex !(infos.ex_table)) with
         | Not_found ->
             extern_id_descr ex)
    | Value_symbol sym ->
        try
          let ex = Symbol.Map.find sym !(infos.ex_symbol_id) in
          Some (Export_id.Map.find ex !(infos.ex_table))
        with Not_found ->
          extern_symbol_descr sym

  let add_symbol sym id =
    infos.ex_symbol_id := Symbol.Map.add sym id !(infos.ex_symbol_id)

  let symbol_id sym =
    try Some (Symbol.Map.find sym !(infos.ex_symbol_id)) with Not_found -> None

  let add_constant lam ex_id =
    let sym = Compilenv.new_const_symbol' () in
    Symbol.Tbl.add infos.constants sym lam;
    add_symbol sym ex_id;
    sym

  let new_descr descr = new_descr descr infos
  let unit_approx () : ET.approx = Value_id (new_descr (Value_constptr 0))

  let rec conv env expr = fst (conv_approx env expr)
  and conv_approx (env : env) (flam : _ Flambda.t)
        : unit Flambda.t * ET.approx =
    match flam with
    | Fvar (id,_) ->
        begin
          (* If the variable reference a constant, it is replaced by the
             constant label *)
          try
            let lbl = Variable.Map.find id env.cm in
            Fsymbol(lbl, ()), Value_symbol lbl
          with Not_found ->

            (* If the variable is a recursive access to the function
               currently being defined: it is replaced by an offset in the
               closure. If the variable is bound by the closure, it is
               replace by a field access inside the closure *)
            try
              let lam = Variable.Map.find id env.sb in
              lam, get_approx id env
            with Not_found ->
              Fvar (id, ()), get_approx id env
        end

    | Fsymbol (sym,_) ->
        Fsymbol (sym,()),
        Value_symbol sym

    | Fconst (Fconst_base c as cst,_) ->
        begin
          match c with
          | Const_int i ->
              Fconst(cst, ()),
              Value_id (new_descr (Value_int i))
          | Const_char c ->
              Fconst(cst, ()),
              Value_id (new_descr (Value_int (Char.code c)))
          | Const_float s ->
              Fconst (cst, ()),
              Value_id (new_descr (Value_float (float_of_string s)))
          | Const_int32 i ->
              Fconst(cst, ()),
              Value_id (new_descr (Value_boxed_int (Int32, i)))
          | Const_int64 i ->
              Fconst(cst, ()),
              Value_id (new_descr (Value_boxed_int (Int64, i)))
          | Const_nativeint i ->
              Fconst(cst, ()),
              Value_id (new_descr (Value_boxed_int (Nativeint, i)))
          | Const_string (s,_) ->
              let v_string : ET.value_string =
                { size = String.length s; contents = None }
              in
              let ex_id = new_descr (Value_string v_string) in
              Fsymbol (add_constant (Fconst (cst,())) ex_id,()),
              Value_id ex_id
        end
    | Fconst (Fconst_float f as cst, _) ->
        Fconst (cst, ()), Value_id (new_descr (Value_float f))
    | Fconst (Fconst_pointer c as cst,_) ->
        Fconst (cst, ()), Value_id (new_descr (Value_constptr c))
    | Fconst (Fconst_float_array c as cst, _) ->
        let ex_id = new_descr (Value_float_array (List.length c)) in
        Fsymbol(add_constant (Fconst (cst,())) ex_id,()),
        Value_id ex_id
    | Fconst (Fconst_immstring c as cst, _) ->
        let v_string : ET.value_string =
          { size = String.length c; contents = Some c }
        in
        let ex_id = new_descr (Value_string v_string) in
        Fsymbol(add_constant (Fconst (cst,())) ex_id,()),
        Value_id ex_id

    | Flet(str, id, lam, body, _) ->
        let lam, approx = conv_approx env lam in
        let env =
          if is_constant id || str = Flambda.Immutable
          then add_approx id approx env
          else add_approx id Value_unknown env
        in
        begin match is_constant id, constant_symbol lam, str with
        | _, _, Mutable
        | false, (Not_const | No_lbl | Const_closure), _ ->
            let ubody, body_approx = conv_approx env body in
            Flet(str, id, lam, ubody, ()), body_approx
        | true, No_lbl, Immutable ->
            (* no label: the value is an integer: substitute it *)
            conv_approx (add_sb id lam env) body
        | _, Lbl lbl, Immutable ->
            (* label: the value is a block: reference it *)
            conv_approx (add_cm id lbl env) body
        | true, Const_closure, Immutable ->
            conv_approx env body
        | true, Not_const, Immutable ->
            Format.eprintf "%a@.%a" Variable.print id
              Printflambda.flambda lam;
            assert false
        end

    | Fletrec(defs, body, _) ->
        let consts, not_consts =
          List.partition (fun (id,_) -> is_constant id) defs in

        let env, consts = List.fold_left
            (fun (env, acc) (id, (def : _ Flambda.t)) ->
               match def with
               | Fconst (( Fconst_pointer _
                         | Fconst_base
                             (Const_int _ | Const_char _
                             | Const_float _ | Const_int32 _
                             | Const_int64 _ | Const_nativeint _)), _) ->
                   (* When the value is an integer constant, we cannot affect a label
                      to it: hence we must substitute it directly.
                      For other numerical constant, a label could be attributed, but
                      unboxing doesn't handle it well *)
                   add_sb id (conv env def) env, acc
               | Fvar (var_id, _) ->
                   assert(List.for_all(fun (id,_) -> not (Variable.equal var_id id)) consts);
                   (* For variables: the variable could have been substituted to
                      a constant: avoid it by substituting it directly *)
                   add_sb id (conv env def) env, acc
               | _ ->
                   let sym = Compilenv.new_const_symbol' () in
                   let env = add_cm id sym env in
                   env, (id,sym,def)::acc) (env,[]) consts in

        List.iter (fun (id,sym,def) ->
            match constant_symbol (conv env def) with
            | Lbl sym' ->
                (match symbol_id sym' with
                 | None -> ()
                 | Some eid -> add_symbol sym eid);
                set_symbol_alias sym sym'
            | _ ->
                Misc.fatal_error (Format.asprintf
                               "recursive constant value without symbol %a"
                               Variable.print id))
          consts;

        let not_consts, env =
          List.fold_right (fun (id,def) (not_consts,env') ->
              let flam, approx = conv_approx env def in
              let env' = add_approx id approx env' in
              (id, flam) :: not_consts, env') not_consts ([],env) in

        let body, approx = conv_approx env body in
        (match not_consts with
         | [] -> body
         | _ -> Fletrec(not_consts, body, ())),
        approx

    | Fset_of_closures ({ function_decls = funct;
                  free_vars = fv;
                  specialised_args = spec_arg }, _) ->
        let args_approx = Variable.Map.map (fun id -> get_approx id env) spec_arg in
        conv_closure env funct args_approx spec_arg fv

    | Fproject_closure({ set_of_closures = lam; closure_id = id }, _) as expr ->
        let ulam, fun_approx = conv_approx env lam in
        if is_local_function_constant id
        then
          (* Only functions declared in the current module may need
             rewriting to a symbol. For external functions it should
             already have been done at the original declaration. *)
          let sym = Compilenv.closure_symbol id in
          Fsymbol (sym,()),
          Value_symbol sym
        else
          let approx : ET.approx =
            match get_descr fun_approx with
            | Some (Value_set_of_closures set_of_closures)
            | Some (Value_closure { set_of_closures }) ->
                let ex = new_descr (Value_closure { fun_id = id; set_of_closures }) in
                Value_id ex
            | _ when not (Closure_id.in_compilation_unit
                            (Compilenv.current_unit ())
                            id) ->
                (* If some cmx files are missing, the value could be unknown.
                   Notice that this is valid only for something coming from
                   another compilation unit, otherwise this is a bug. *)
                Value_unknown
            | Some _ -> assert false
            | _ ->
                Format.printf "Unknown closure in offset %a@."
                  Printflambda.flambda expr;
                assert false
          in
          Fselect_closure({ set_of_closures = ulam; closure_id = id; relative_to = rel },()),
          approx

    | Fvar_within_closure({closure = lam;var = env_var;closure_id = env_fun_id}, _) as expr ->
        let ulam, fun_approx = conv_approx env lam in
        let approx : ET.approx =
          match get_descr fun_approx with
          | Some (Value_closure { set_of_closures = { bound_var } }) ->
              (try Var_within_closure.Map.find env_var bound_var with
               | Not_found ->
                   Format.printf "Wrong closure in env_field %a@.%a@."
                     Printflambda.flambda expr
                     Printflambda.flambda ulam;
                   assert false)
          | _ when not (Closure_id.in_compilation_unit
                          (Compilenv.current_unit ())
                          env_fun_id) ->
              (* If some cmx files are missing, the value could be unknown.
                   Notice that this is valid only for something coming from
                   another compilation unit, otherwise this is a bug. *)
              Value_unknown
          | Some _ -> assert false
          | None ->
              Format.printf "Unknown closure in env_field %a@.%a@."
                Printflambda.flambda expr
                Printflambda.flambda ulam;
              assert false in
        Fvar_within_closure({closure = ulam;var = env_var;closure_id = env_fun_id}, ()),
        approx

    | Fapply({func = funct; args; kind = direct; dbg = dbg}, _) ->
        let ufunct, fun_approx = conv_approx env funct in
        let direct : Flambda.call_kind =
          match direct with
          | Direct _ -> direct
          | Indirect -> match get_descr fun_approx with
            (* We mark some calls as direct when it is unknown:
               for instance if simplify wasn't run before. *)
            | Some (Value_closure { fun_id }) when
                (function_arity fun_id) = List.length args ->
                Direct fun_id
            | _ -> Indirect
        in
        let approx : ET.approx =
          match get_descr fun_approx with
          | Some(Value_closure { fun_id; set_of_closures = { results } }) ->
              Closure_id.Map.find fun_id results
          | _ -> Value_unknown
        in
        Fapply({func = ufunct; args = conv_list env args;
                kind = direct;
                dbg = dbg}, ()),
        approx

    | Fswitch(arg, sw, _) ->
        Fswitch(conv env arg,
                { sw with
                  consts = List.map (fun (i,lam) -> i, conv env lam) sw.consts;
                  blocks = List.map (fun (i,lam) -> i, conv env lam) sw.blocks;
                  failaction = Misc.may_map (conv env) sw.failaction }, ()),
        Value_unknown

    | Fstringswitch(arg, sw, def, _) ->
        Fstringswitch
          (conv env arg,
           List.map (fun (i,lam) -> i, conv env lam) sw,
           Misc.may_map (conv env) def, ()),
        Value_unknown

    | Fprim(Lambda.Pgetglobal id, l, _, _) ->
        assert(l = []);
        let sym = Compilenv.symbol_for_global' id in
        Fsymbol (sym, ()),
        Value_symbol sym

    | Fprim(Lambda.Pgetglobalfield(id,i), l, dbg, v) ->
        assert(l = []);
        let lam : _ Flambda.t =
          Fprim(Lambda.Pfield i,
            [Flambda.Fprim(Lambda.Pgetglobal id, l, dbg, v)], dbg, v)
        in
        if id = Compilenv.current_unit_id ()
        then let approx = get_global i in
          match approx with
          | Value_symbol sym ->
              Fsymbol(sym,()), approx
          | _ ->
              conv env lam, approx
        else
          conv_approx env lam

    | Fprim(Lambda.Psetglobalfield (ex, i), [arg], dbg, _) ->
        let uarg, approx = conv_approx env arg in
        add_global i approx;
        Fprim(Lambda.Psetglobalfield (ex, i), [uarg], dbg, ()),
        Value_unknown

    | Fprim(Lambda.Pmakeblock(tag, Asttypes.Immutable) as p, args, dbg, _) ->
        let args, approxs = conv_list_approx env args in
        let block : _ Flambda.t = Fprim(p, args, dbg, ()) in
        let tag = Tag.create_exn tag in
        let ex = new_descr (Value_block (tag, Array.of_list approxs)) in
        if not (List.for_all is_simple_constant args)
        then block, Value_id ex
        else
          let sym = add_constant block ex in
          Fsymbol(sym, ()), Value_symbol sym

(*  (* If global mutables are allowed: *)
    | Fprim(Lambda.Pmakeblock(tag, Asttypes.Mutable) as p, args, dbg, _)
      when env.toplevel ->
        let args, _approxs = conv_list_approx env args in
        let block = Fprim(p, args, dbg, ()) in
        let ex = new_descr (Value_mutable_block (tag, List.length args)) in
        if not (List.for_all is_simple_constant args)
        then block, Value_id ex
        else
          let sym = add_constant block ex in
          Fsymbol(sym, ()), Value_symbol sym
*)

    | Fprim(Lambda.Pfield i, [arg], dbg, _) ->
        let block, block_approx = conv_approx env arg in
        let approx : ET.approx =
          match get_descr block_approx with
          | Some (Value_block (_,fields)) ->
              if i >= 0 && i < Array.length fields
              then fields.(i)
              else Value_unknown
          | _ -> Value_unknown
        in
        Fprim(Lambda.Pfield i, [block], dbg, ()),
        approx

    | Fprim(p, args, dbg, _) ->
        Fprim(p, conv_list env args, dbg, ()),
        Value_unknown

    | Fstaticraise (i, args, _) ->
        Fstaticraise (i, conv_list env args, ()),
        Value_unknown

    | Fstaticcatch (i, vars, body, handler, _) ->
        Fstaticcatch (i, vars, conv env body, conv env handler, ()),
        Value_unknown

    | Ftrywith(body, id, handler, _) ->
        Ftrywith(conv env body, id, conv env handler, ()),
        Value_unknown

    | Fifthenelse(arg, ifso, ifnot, _) ->
        Fifthenelse(conv env arg, conv env ifso, conv env ifnot, ()),
        Value_unknown

    | Fsequence(lam1, lam2, _) ->
        let ulam1 = conv env lam1 in
        let ulam2, approx = conv_approx env lam2 in
        Fsequence(ulam1, ulam2, ()),
        approx

    | Fwhile(cond, body, _) ->
        Fwhile(conv env cond, conv env body, ()),
        unit_approx ()

    | Ffor(id, lo, hi, dir, body, _) ->
        Ffor(id, conv env lo, conv env hi, dir, conv env body, ()),
        unit_approx ()

    | Fassign(id, lam, _) ->
        Fassign(id, conv env lam, ()),
        unit_approx ()

    | Fsend(kind, met, obj, args, dbg, _) ->
        Fsend(kind, conv env met, conv env obj, conv_list env args, dbg, ()),
        Value_unknown

    | Funreachable _ ->
        Funreachable (),
        Value_unknown

  and conv_closure env (functs : _ Flambda.function_declarations)
        param_approxs spec_arg fv =
    let closed =
      Set_of_closures_id.Set.mem functs.set_of_closures_id P.constant_closures
    in
    let fv_ulam_approx = Variable.Map.map (conv_approx env) fv in
    let fv_ulam =
      Variable.Map.map (fun (lam, _approx) -> lam) fv_ulam_approx
    in
    let kept_fv id =
      let cv = Var_within_closure.wrap id in
      not (is_constant id)
      || (Var_within_closure.Set.mem cv used_variable_within_closure) in

    let used_fv_approx = Variable.Map.filter (fun id _ -> kept_fv id) fv_ulam_approx in
    let used_fv =
      Variable.Map.map (fun (lam, _approx) -> lam) used_fv_approx
    in
    let varmap_to_closfun_map map =
      Variable.Map.fold (fun var v acc ->
          let cf = Closure_id.wrap var in
          Closure_id.Map.add cf v acc)
        map Closure_id.Map.empty in

    let value_closure' : ET.value_set_of_closures =
      { set_of_closures_id = functs.set_of_closures_id;
        bound_var =
          Variable.Map.fold (fun off_id (_,approx) map ->
              let cv = Var_within_closure.wrap off_id in
              Var_within_closure.Map.add cv approx map)
            used_fv_approx Var_within_closure.Map.empty;
        results =
          varmap_to_closfun_map
            (Variable.Map.map (fun _ -> ET.Value_unknown) functs.funs) } in

    (* add informations about free variables *)
    let env =
      Variable.Map.fold (fun id (_,approx) -> add_approx id approx)
        fv_ulam_approx env in

    (* add info about symbols in specialised_arg *)
    let env = Variable.Map.fold copy_env spec_arg env in

    (* Constant closures will be moved out of their scope and assigned to
       symbols.  When this happens, we must erase any constraint that
       specializes an argument to another variable, since that variable may
       no longer be in scope.  (Specializations of variables to values that
       are now referenced by symbols, rather than variables, will already have
       been performed.  As such, the operation is equivalent to erasing all
       specialization information.) *)
    let spec_arg =
      if closed then Variable.Map.empty
      else
        Variable.Map.filter (fun _ id -> not (Variable.Map.mem id env.cm))
          spec_arg
    in

    let conv_function _id (func : _ Flambda.function_declaration) =

      (* inside the body of the function, we cannot access variables
         declared outside, so take a clean substitution table. *)
      let env = { env with sb = Variable.Map.empty } in
      let env = { env with toplevel = false } in

      (* add informations about currently defined functions to
         allow direct call *)
      let env =
        Variable.Map.fold (fun id _ env ->
            let fun_id = Closure_id.wrap id in
            let desc : ET.descr =
              Value_closure { fun_id; set_of_closures = value_closure' }
            in
            let ex = new_descr desc in
            if closed then add_symbol (Compilenv.closure_symbol fun_id) ex;
            add_approx id (Value_id ex) env)
          functs.funs env
      in

      let env =
        (* param_approxs must be constants: part of specialised_args *)
        Variable.Map.fold (fun id approx env -> add_approx id approx env)
          param_approxs env in

      (* Add to the substitution the value of the free variables *)
      let add_env_variable id lam env =
        match constant_symbol lam with
        | Not_const ->
            assert(not closed);
            env
        | No_lbl ->
            add_sb id lam env
        | Lbl lbl ->
            add_cm id lbl env
        | Const_closure ->
            env
      in
      let env = Variable.Map.fold add_env_variable fv_ulam env in

      let env =
        if closed
        then
          (* if the function is closed, recursive call access those constants *)
          Variable.Map.fold (fun id _ env ->
              let fun_id = Closure_id.wrap id in
              add_cm id (Compilenv.closure_symbol fun_id) env) functs.funs env
        else env
      in
      let body, approx = conv_approx env func.body in
      { func with
        free_variables = Variable.Set.filter kept_fv func.free_variables;
        body }, approx
    in

    let funs_approx = Variable.Map.mapi conv_function functs.funs in

    let ufunct = { functs with funs = Variable.Map.map fst funs_approx } in

    let value_closure' =
      { value_closure' with
        results = varmap_to_closfun_map (Variable.Map.map snd funs_approx) } in

    let closure_ex_id = new_descr (Value_set_of_closures value_closure') in
    let value_closure : ET.approx = Value_id closure_ex_id in

    let expr : _ Flambda.t =
      let expr : _ Flambda.t =
        Fset_of_closures ({ function_decls = ufunct;
                    free_vars = used_fv;
                    specialised_args = spec_arg }, ()) in
      if Set_of_closures_id.Set.mem ufunct.set_of_closures_id P.constant_closures
      then
        let sym = add_constant expr closure_ex_id in
        Fsymbol(sym, ())
      else expr in
    expr, value_closure


  and conv_list env l = List.map (conv env) l
  and conv_list_approx env l =
    List.split (List.map (conv_approx env) l)

  and is_simple_constant (flam : _ Flambda.t) =
    match flam with
    | Fconst _
    | Fsymbol _ -> true
    | _ -> false

  and constant_symbol : unit Flambda.t -> const_sym = function
    | Fsymbol(sym, ()) ->
        Lbl sym
    | Fconst(_, ()) ->
        No_lbl
    | Fset_of_closures ({ function_decls }, _) ->
        if Set_of_closures_id.Set.mem function_decls.set_of_closures_id P.constant_closures
        then Const_closure
        else Not_const
    | _ -> Not_const


  let expr = conv { empty_env with toplevel = true } P.expr

end

module type Param2 = sig
  include Param1
  val infos : infos
  val expr : unit Flambda.t
end

module Prepare(P:Param2) = struct
  (*** Preparing export information: Replacing every symbol by its
       canonical representative ***)

  let canonical_symbol s = canonical_symbol s P.infos

  (* Replace all symbols' occurrences by their representative *)
  let expr, constants =
    let use_canonical_symbols (flam : _ Flambda.t) : _ Flambda.t =
      match flam with
      | Fsymbol(sym, ()) as expr ->
          let sym' = canonical_symbol sym in
          if sym == sym' then expr
          else Fsymbol(sym', ())
      | expr -> expr in
    let aux sym lam map =
      let sym' = canonical_symbol sym in
      Symbol.Map.add sym' (Flambdaiter.map use_canonical_symbols lam) map
    in
    Flambdaiter.map use_canonical_symbols P.expr,
    Symbol.Tbl.fold aux P.infos.constants Symbol.Map.empty

  let ex_functions =
    let ex_functions = ref Set_of_closures_id.Map.empty in
    let aux ({ function_decls } : _ Flambda.set_of_closures) _ =
      ex_functions := Set_of_closures_id.Map.add function_decls.set_of_closures_id function_decls !ex_functions
    in
    Flambdaiter.iter_on_sets_of_closures aux expr;
    Symbol.Map.iter (fun _ -> Flambdaiter.iter_on_sets_of_closures aux) constants;
    !ex_functions

  (* Preparing export information *)

  let canonical_approx (approx : ET.approx) : ET.approx =
    match approx with
    | Value_unknown
    | Value_id _ as v -> v
    | Value_symbol sym ->
        Value_symbol (canonical_symbol sym)

  let rec canonical_descr (descr : ET.descr) : ET.descr =
    match descr with
    | Value_block (tag, fields) ->
        Value_block (tag, Array.map canonical_approx fields)
    | Value_int _
    | Value_constptr _
    | Value_string _
    | Value_float _
    | Value_float_array _
    | ET.Value_boxed_int _ as v -> v
    | Value_closure offset ->
        Value_closure { offset with set_of_closures = (aux_set_of_closures offset.set_of_closures) }
    | Value_set_of_closures clos ->
        Value_set_of_closures (aux_set_of_closures clos)
    | Value_mutable_block (tag, size) ->
        Value_mutable_block (tag, size)

  and aux_set_of_closures (clos : ET.value_set_of_closures)
        : ET.value_set_of_closures =
    { set_of_closures_id = clos.set_of_closures_id;
      bound_var = Var_within_closure.Map.map canonical_approx clos.bound_var;
      results = Closure_id.Map.map canonical_approx clos.results;
    }

  let new_descr descr = new_descr descr P.infos

  (* build the approximation of the root module *)
  let root_id =
    let size_global =
      1 + (Hashtbl.fold (fun k _ acc -> max k acc) P.infos.global (-1)) in
    let fields = Array.init size_global (fun i ->
        try canonical_approx (Hashtbl.find P.infos.global i) with
        | Not_found -> ET.Value_unknown) in
    new_descr (Value_block (Tag.zero,fields))

  let root_approx : ET.approx =
    Value_id root_id

  (* replace symbol by their representative in value approximations *)
  let ex_values =
    Export_id.Map.map canonical_descr !(P.infos.ex_table)

  (* build the symbol to id and id to symbol maps *)
  let module_symbol =
    Compilenv.current_unit_symbol ()

  let ex_symbol_id =
    let aux sym ex map =
      let sym' = canonical_symbol sym in
      Symbol.Map.add sym' ex map
    in
    Symbol.Map.fold aux !(P.infos.ex_symbol_id) Symbol.Map.empty

  let ex_symbol_id =
    Symbol.Map.add module_symbol root_id
      ex_symbol_id
  let ex_id_symbol =
    Symbol.Map.fold (fun sym id map -> Export_id.Map.add id sym map)
      ex_symbol_id Export_id.Map.empty

  let ex_functions_off =
    let aux_fun ffunctions off_id _ map =
      let fun_id = Closure_id.wrap off_id in
      Closure_id.Map.add fun_id ffunctions map in
    let aux _ (f : _ Flambda.function_declarations) map =
      Variable.Map.fold (aux_fun f) f.funs map
    in
    Set_of_closures_id.Map.fold aux ex_functions Closure_id.Map.empty

end

let convert (type a) ~compilation_unit (expr:a Flambda.t) =
  let inconstants = Inconstant_idents.inconstants ~compilation_unit ~for_clambda:true expr in
  let constant_closures = constant_closures inconstants expr in
  let module P1 = struct
    type t = a
    let expr = expr
    let inconstants = inconstants
    let constant_closures = constant_closures
  end in
  let module C = Conv(P1) in
  let module P2 = struct
    include P1
    let expr = C.expr
    let infos = C.infos
  end in
  let module C2 = Prepare(P2) in

  let export : ET.exported =
    { Flambdaexport.empty_export with
      ex_values = Flambdaexport.nest_eid_map C2.ex_values;
      ex_globals = Ident.Map.singleton
          (Compilenv.current_unit_id ()) C2.root_approx;
      ex_symbol_id = C2.ex_symbol_id;
      ex_id_symbol = Flambdaexport.nest_eid_map C2.ex_id_symbol;
      ex_functions = C2.ex_functions;
      ex_functions_off = C2.ex_functions_off;
      ex_constant_closures = constant_closures;
      ex_kept_arguments = C.ex_kept_arguments }
  in
  C2.expr, C2.constants, export

