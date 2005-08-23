(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*         Jerome Vouillon, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Translobj
open Translcore

(* XXX Rajouter des evenements... *)

type error = Illegal_class_expr | Tags of label * label

exception Error of Location.t * error

let lfunction params body =
  if params = [] then body else
  match body with
    Lfunction (Curried, params', body') ->
      Lfunction (Curried, params @ params', body')
  |  _ ->
      Lfunction (Curried, params, body)

let lapply func args =
  match func with
    Lapply(func', args') ->
      Lapply(func', args' @ args)
  | _ ->
      Lapply(func, args)

let lsequence l1 l2 =
  if l2 = lambda_unit then l1 else Lsequence(l1, l2)

let lfield v i = Lprim(Pfield i, [Lvar v])

let transl_label l = share (Const_base (Const_string l))

let rec transl_meth_list lst =
  if lst = [] then Lconst (Const_pointer 0) else
  share (Const_block
            (0, List.map (fun lab -> Const_base (Const_string lab)) lst))

let set_inst_var obj id expr =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  Lprim(Parraysetu kind, [Lvar obj; Lvar id; transl_exp expr])

let copy_inst_var obj id expr templ offset =
  let kind = if Typeopt.maybe_pointer expr then Paddrarray else Pintarray in
  let id' = Ident.create (Ident.name id) in
  Llet(Strict, id', Lprim (Pidentity, [Lvar id]),
  Lprim(Parraysetu kind,
        [Lvar obj; Lvar id';
         Lprim(Parrayrefu kind, [Lvar templ; Lprim(Paddint,
                                                   [Lvar id';
                                                    Lvar offset])])]))

let transl_val tbl create name =
  Lapply (oo_prim (if create then "new_variable" else "get_variable"),
          [Lvar tbl; transl_label name])

let transl_vals tbl create sure vals rem =
  if create && sure && List.length vals > 1 then
    let (_,id0) = List.hd vals in
    let call =
      Lapply(oo_prim "new_variables",
	     [Lvar tbl; transl_meth_list (List.map fst vals)]) in
    let i = ref (List.length vals) in
    Llet(Strict, id0, call,
	 List.fold_right
	   (fun (name,id) rem ->
	     decr i; Llet(Alias, id, Lprim(Poffsetint !i, [Lvar id0]), rem))
	   (List.tl vals) rem)
  else
  List.fold_right
    (fun (name, id) rem ->
      Llet(StrictOpt, id, transl_val tbl create name, rem))
    vals rem

let transl_super tbl meths inh_methods rem =
  List.fold_right
    (fun (nm, id) rem ->
       begin try
         Llet(StrictOpt, id, Lapply (oo_prim "get_method",
                                     [Lvar tbl; Lvar (Meths.find nm meths)]),
              rem)
       with Not_found ->
         rem
       end)
    inh_methods rem

let create_object cl obj init =
  let obj' = Ident.create "self" in
  let (inh_init, obj_init, has_init) = init obj' in
  if obj_init = lambda_unit then
    (inh_init,
     Lapply (oo_prim (if has_init then "create_object_and_run_initializers"
                      else"create_object_opt"),
             [obj; Lvar cl]))
  else begin
   (inh_init,
    Llet(Strict, obj',
            Lapply (oo_prim "create_object_opt", [obj; Lvar cl]),
         Lsequence(obj_init,
                   if not has_init then Lvar obj' else
                   Lapply (oo_prim "run_initializers_opt",
			   [obj; Lvar obj'; Lvar cl]))))
  end

let rec build_object_init cl_table obj params inh_init obj_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      let obj_init = Ident.create "obj_init" in
      let envs, inh_init = inh_init in
      let env =
        match envs with None -> []
        | Some envs -> [Lprim(Pfield (List.length inh_init + 1), [Lvar envs])]
      in
      ((envs, (obj_init, path)::inh_init),
       Lapply(Lvar obj_init, env @ [obj]))
  | Tclass_structure str ->
      create_object cl_table obj (fun obj ->
        let (inh_init, obj_init, has_init) =
          List.fold_right
            (fun field (inh_init, obj_init, has_init) ->
               match field with
                 Cf_inher (cl, _, _) ->
                   let (inh_init, obj_init') =
                     build_object_init cl_table (Lvar obj) [] inh_init
                       (fun _ -> lambda_unit) cl
                   in
                   (inh_init, lsequence obj_init' obj_init, true)
               | Cf_val (_, id, exp) ->
                   (inh_init, lsequence (set_inst_var obj id exp) obj_init,
                    has_init)
               | Cf_meth _ ->
                   (inh_init, obj_init, has_init)
               | Cf_init _ ->
                   (inh_init, obj_init, true)
               | Cf_let (rec_flag, defs, vals) ->
                   (inh_init,
                    Translcore.transl_let rec_flag defs
                      (List.fold_right
                         (fun (id, expr) rem ->
                            lsequence (Lifused(id, set_inst_var obj id expr))
                                      rem)
                         vals obj_init),
                    has_init))
            str.cl_field
            (inh_init, obj_init obj, false)
        in
        (inh_init,
         List.fold_right
           (fun (id, expr) rem ->
              lsequence (Lifused (id, set_inst_var obj id expr)) rem)
           params obj_init,
         has_init))
  | Tclass_fun (pat, vals, cl, partial) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init obj_init cl
      in
      (inh_init,
       let build params rem =
         let param = name_pattern "param" [pat, ()] in
         Lfunction (Curried, param::params,
                    Matching.for_function
                      pat.pat_loc None (Lvar param) [pat, rem] partial)
       in
       begin match obj_init with
         Lfunction (Curried, params, rem) -> build params rem
       | rem                              -> build [] rem
       end)
  | Tclass_apply (cl, oexprs) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj params inh_init obj_init cl
      in
      (inh_init, transl_apply obj_init oexprs)
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, obj_init) =
        build_object_init cl_table obj (vals @ params) inh_init obj_init cl
      in
      (inh_init, Translcore.transl_let rec_flag defs obj_init)
  | Tclass_constraint (cl, vals, pub_meths, concr_meths) ->
      build_object_init cl_table obj params inh_init obj_init cl

let rec build_object_init_0 cl_table params cl copy_env subst_env top ids =
  match cl.cl_desc with
    Tclass_let (rec_flag, defs, vals, cl) ->
      build_object_init_0 cl_table (vals@params) cl copy_env subst_env top ids
  | _ ->
      let self = Ident.create "self" in
      let env = Ident.create "env" in
      let obj = if ids = [] then lambda_unit else Lvar self in
      let envs = if top then None else Some env in
      let ((_,inh_init), obj_init) =
        build_object_init cl_table obj params (envs,[]) (copy_env env) cl in
      let obj_init =
	if ids = [] then obj_init else lfunction [self] obj_init in
      (inh_init, lfunction [env] (subst_env env inh_init obj_init))


let bind_method tbl lab id cl_init =
  Llet(StrictOpt, id, Lapply (oo_prim "get_method_label",
                              [Lvar tbl; transl_label lab]),
       cl_init)

let bind_methods tbl meths cl_init =
  let methl = Meths.fold (fun lab id tl -> (lab,id) :: tl) meths [] in
  let len = List.length methl in
  if len < 2 then Meths.fold (bind_method tbl) meths cl_init else
  let ids = Ident.create "ids" in
  let i = ref len in
  Llet(StrictOpt, ids,
       Lapply (oo_prim "get_method_labels",
               [Lvar tbl; transl_meth_list (List.map fst methl)]),
       List.fold_right
         (fun (lab,id) lam ->
           decr i; Llet(StrictOpt, id, Lprim(Pfield !i, [Lvar ids]), lam))
         methl cl_init)

let output_methods tbl vals methods lam =
  let lam =
    match methods with
      [] -> lam
    | [lab; code] ->
        lsequence (Lapply(oo_prim "set_method", [Lvar tbl; lab; code])) lam
    | _ ->
        lsequence (Lapply(oo_prim "set_methods",
                         [Lvar tbl; Lprim(Pmakeblock(0,Immutable), methods)]))
          lam
  in
  transl_vals tbl true true vals lam

let rec ignore_cstrs cl =
  match cl.cl_desc with
    Tclass_constraint (cl, _, _, _) -> ignore_cstrs cl
  | Tclass_apply (cl, _) -> ignore_cstrs cl
  | _ -> cl

let rec build_class_init cla cstr inh_init cl_init msubst top cl =
  match cl.cl_desc with
    Tclass_ident path ->
      begin match inh_init with
        (obj_init, path')::inh_init ->
	  let lpath = transl_path path in
          (inh_init,
           Llet (Strict, obj_init, 
                 Lapply(Lprim(Pfield 1, [lpath]), Lvar cla ::
			if top then [Lprim(Pfield 3, [lpath])] else []),
                 cl_init))
      | _ ->
          assert false
      end
  | Tclass_structure str ->
      let (inh_init, cl_init, methods, values) =
        List.fold_right
          (fun field (inh_init, cl_init, methods, values) ->
            match field with
              Cf_inher (cl, vals, meths) ->
                let cl_init = output_methods cla values methods cl_init in
                let inh_init, cl_init =
                  build_class_init cla false inh_init
                    (transl_vals cla false false vals
                       (transl_super cla str.cl_meths meths cl_init))
                    msubst top cl in
                (inh_init, cl_init, [], [])
            | Cf_val (name, id, exp) ->
                (inh_init, cl_init, methods, (name, id)::values)
            | Cf_meth (name, exp) ->
                let met_code = msubst true (transl_exp exp) in
                let met_code =
                  if !Clflags.native_code && List.length met_code = 1 then
                    (* Force correct naming of method for profiles *)
                    let met = Ident.create ("method_" ^ name) in
                    [Llet(Strict, met, List.hd met_code, Lvar met)]
                  else met_code
                in
                (inh_init, cl_init,
                 Lvar (Meths.find name str.cl_meths) :: met_code @ methods,
                 values)
                 (*
                 Lsequence(Lapply (oo_prim ("set_method" ^ builtin),
                                   Lvar cla ::
                                   Lvar (Meths.find name str.cl_meths) ::
                                   met_code),
                           cl_init))
                  *)
            | Cf_let (rec_flag, defs, vals) ->
                let vals =
                  List.map (function (id, _) -> (Ident.name id, id)) vals
                in
                (inh_init, cl_init, methods, vals @ values)
            | Cf_init exp ->
                (inh_init,
                 Lsequence(Lapply (oo_prim "add_initializer",
                                   Lvar cla :: msubst false (transl_exp exp)),
                           cl_init),
                 methods, values))
          str.cl_field
          (inh_init, cl_init, [], [])
      in
      let cl_init = output_methods cla values methods cl_init in
      (inh_init, bind_methods cla str.cl_meths cl_init)
  | Tclass_fun (pat, vals, cl, _) ->
      let (inh_init, cl_init) =
        build_class_init cla cstr inh_init cl_init msubst top cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true false vals cl_init)
  | Tclass_apply (cl, exprs) ->
      build_class_init cla cstr inh_init cl_init msubst top cl
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let (inh_init, cl_init) =
        build_class_init cla cstr inh_init cl_init msubst top cl
      in
      let vals = List.map (function (id, _) -> (Ident.name id, id)) vals in
      (inh_init, transl_vals cla true false vals cl_init)
  | Tclass_constraint (cl, vals, meths, concr_meths) ->
      let virt_meths =
        List.filter (fun lab -> not (Concr.mem lab concr_meths)) meths in
      let narrow_args =
	[Lvar cla;
         transl_meth_list vals;
         transl_meth_list virt_meths;
         transl_meth_list (Concr.elements concr_meths)] in
      let cl = ignore_cstrs cl in
      begin match cl.cl_desc, inh_init with
	Tclass_ident path, (obj_init, path')::inh_init ->
	  assert (Path.same path path');
	  let lpath = transl_path path in
          (inh_init,
           Llet (Strict, obj_init, 
		 Lapply(oo_prim "inherits", narrow_args @
			[lpath; Lconst(Const_pointer(if top then 1 else 0))]),
                 cl_init))
      | _ ->
	  let core cl_init =
            build_class_init cla true inh_init cl_init msubst top cl
	  in
	  if cstr then core cl_init else
          let (inh_init, cl_init) =
            core (Lsequence (Lapply (oo_prim "widen", [Lvar cla]), cl_init))
          in
          (inh_init,
           Lsequence(Lapply (oo_prim "narrow", narrow_args), cl_init))
      end

let rec build_class_lets cl =
  match cl.cl_desc with
    Tclass_let (rec_flag, defs, vals, cl) ->
      let env, wrap = build_class_lets cl in
      (env, fun x -> Translcore.transl_let rec_flag defs (wrap x))
  | _ ->
      (cl.cl_env, fun x -> x)

let rec get_class_meths cl =
  match cl.cl_desc with
    Tclass_structure cl ->
      Meths.fold (fun _ -> IdentSet.add) cl.cl_meths IdentSet.empty
  | Tclass_ident _ -> IdentSet.empty
  | Tclass_fun (_, _, cl, _)
  | Tclass_let (_, _, _, cl)
  | Tclass_apply (cl, _)
  | Tclass_constraint (cl, _, _, _) -> get_class_meths cl

(*
   XXX Il devrait etre peu couteux d'ecrire des classes :
     class c x y = d e f
*)
let rec transl_class_rebind obj_init cl =
  match cl.cl_desc with
    Tclass_ident path ->
      (path, obj_init)
  | Tclass_fun (pat, _, cl, partial) ->
      let path, obj_init = transl_class_rebind obj_init cl in
      let build params rem =
        let param = name_pattern "param" [pat, ()] in
        Lfunction (Curried, param::params,
                   Matching.for_function
                     pat.pat_loc None (Lvar param) [pat, rem] partial)
      in
      (path,
       match obj_init with
         Lfunction (Curried, params, rem) -> build params rem
       | rem                              -> build [] rem)
  | Tclass_apply (cl, oexprs) ->
      let path, obj_init = transl_class_rebind obj_init cl in
      (path, transl_apply obj_init oexprs)
  | Tclass_let (rec_flag, defs, vals, cl) ->
      let path, obj_init = transl_class_rebind obj_init cl in
      (path, Translcore.transl_let rec_flag defs obj_init)
  | Tclass_structure _ -> raise Exit
  | Tclass_constraint (cl', _, _, _) ->
      let path, obj_init = transl_class_rebind obj_init cl' in
      let rec check_constraint = function
          Tcty_constr(path', _, _) when Path.same path path' -> ()
        | Tcty_fun (_, _, cty) -> check_constraint cty
        | _ -> raise Exit
      in
      check_constraint cl.cl_type;
      (path, obj_init)

let rec transl_class_rebind_0 self obj_init cl =
  match cl.cl_desc with
    Tclass_let (rec_flag, defs, vals, cl) ->
      let path, obj_init = transl_class_rebind_0 self obj_init cl in
      (path, Translcore.transl_let rec_flag defs obj_init)
  | _ ->
      let path, obj_init = transl_class_rebind obj_init cl in
      (path, lfunction [self] obj_init)

let transl_class_rebind ids cl =
  try
    let obj_init = Ident.create "obj_init"
    and self = Ident.create "self" in
    let obj_init0 = lapply (Lvar obj_init) [Lvar self] in
    let path, obj_init' = transl_class_rebind_0 self obj_init0 cl in
    if not (Translcore.check_recursive_lambda ids obj_init') then
      raise(Error(cl.cl_loc, Illegal_class_expr));
    let id = (obj_init' = lfunction [self] obj_init0) in
    if id then transl_path path else

    let cla = Ident.create "class"
    and new_init = Ident.create "new_init"
    and arg = Ident.create "arg"
    and env_init = Ident.create "env_init"
    and table = Ident.create "table"
    and envs = Ident.create "envs" in
    Llet(
    Strict, new_init, lfunction [obj_init] obj_init',
    Llet(
    Alias, cla, transl_path path,
    Lprim(Pmakeblock(0, Immutable),
          [Lapply(Lvar new_init, [lfield cla 0]);
           lfunction [table]
             (Llet(Strict, env_init,
                   Lapply(lfield cla 1, [Lvar table]),
                   lfunction [envs]
                     (Lapply(Lvar new_init,
                             [Lapply(Lvar env_init, [Lvar envs])]))));
           lfield cla 2;
           lfield cla 3])))
  with Exit ->
    lambda_unit

(* Rewrite a closure using builtins. Improves native code size. *)

let rec module_path = function
    Lvar id ->
      let s = Ident.name id in s <> "" && s.[0] >= 'A' && s.[0] <= 'Z'
  | Lprim(Pfield _, [p])    -> module_path p
  | Lprim(Pgetglobal _, []) -> true
  | _                       -> false

let const_path local = function
    Lvar id -> not (List.mem id local)
  | Lconst _ -> true
  | Lfunction (Curried, _, body) ->
      let fv = free_variables body in
      List.for_all (fun x -> not (IdentSet.mem x fv)) local
  | p -> module_path p

let rec builtin_meths self env env2 body =
  let const_path = const_path (env::self) in
  let conv = function
    (* Lvar s when List.mem s self ->  "_self", [] *)
    | p when const_path p -> "const", [p]
    | Lprim(Parrayrefu _, [Lvar s; Lvar n]) when List.mem s self ->
        "var", [Lvar n]
    | Lprim(Pfield n, [Lvar e]) when Ident.same e env ->
        "env", [Lvar env2; Lconst(Const_pointer n)]
    | Lsend(Self, met, Lvar s, []) when List.mem s self ->
        "meth", [met]
    | _ -> raise Not_found
  in
  match body with
  | Llet(_, s', Lvar s, body) when List.mem s self ->
      builtin_meths (s'::self) env env2 body
  | Lapply(f, [arg]) when const_path f ->
      let s, args = conv arg in ("app_"^s, f :: args)
  | Lapply(f, [arg; p]) when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_"^s^"_const", f :: args @ [p])
  | Lapply(f, [p; arg]) when const_path f && const_path p ->
      let s, args = conv arg in
      ("app_const_"^s, f :: p :: args)
  | Lsend(Self, Lvar n, Lvar s, [arg]) when List.mem s self ->
      let s, args = conv arg in
      ("meth_app_"^s, Lvar n :: args)
  | Lsend(Self, met, Lvar s, []) when List.mem s self ->
      ("get_meth", [met])
  | Lsend(Public, met, arg, []) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lsend(Cached, met, arg, [_;_]) ->
      let s, args = conv arg in
      ("send_"^s, met :: args)
  | Lfunction (Curried, [x], body) ->
      let rec enter self = function
        | Lprim(Parraysetu _, [Lvar s; Lvar n; Lvar x'])
          when Ident.same x x' && List.mem s self ->
            ("set_var", [Lvar n])
        | Llet(_, s', Lvar s, body) when List.mem s self ->
            enter (s'::self) body
        | _ -> raise Not_found
      in enter self body
  | Lfunction _ -> raise Not_found
  | _ ->
      let s, args = conv body in ("get_"^s, args)

module M = struct
  open CamlinternalOO
  let builtin_meths arr self env env2 body =
    let builtin, args = builtin_meths self env env2 body in
    if not arr then [Lapply(oo_prim builtin, args)] else
    let tag = match builtin with
      "get_const" -> GetConst
    | "get_var"   -> GetVar
    | "get_env"   -> GetEnv
    | "get_meth"  -> GetMeth
    | "set_var"   -> SetVar
    | "app_const" -> AppConst
    | "app_var"   -> AppVar
    | "app_env"   -> AppEnv
    | "app_meth"  -> AppMeth
    | "app_const_const" -> AppConstConst
    | "app_const_var"   -> AppConstVar
    | "app_const_env"   -> AppConstEnv
    | "app_const_meth"  -> AppConstMeth
    | "app_var_const"   -> AppVarConst
    | "app_env_const"   -> AppEnvConst
    | "app_meth_const"  -> AppMethConst
    | "meth_app_const"  -> MethAppConst
    | "meth_app_var"    -> MethAppVar
    | "meth_app_env"    -> MethAppEnv
    | "meth_app_meth"   -> MethAppMeth
    | "send_const" -> SendConst
    | "send_var"   -> SendVar
    | "send_env"   -> SendEnv
    | "send_meth"  -> SendMeth
    | _ -> assert false
    in Lconst(Const_pointer(Obj.magic tag)) :: args
end
open M


(*
   Traduction d'une classe.
   Plusieurs cas:
    * reapplication d'une classe connue -> transl_class_rebind
    * classe sans dependances locales -> traduction directe
    * avec dependances locale -> creation d'un arbre de stubs,
      avec un noeud pour chaque classe locale heritee
   Une classe est un 4-uplet:
    (obj_init, class_init, env_init, env)
    obj_init: fonction de creation d'objet (unit -> obj)
    class_init: fonction d'heritage (table -> env_init)
      (une seule par code source)
    env_init: parametrage par l'environnement local (env -> params -> obj_init)
      (une par combinaison de class_init herites)
    env: environnement local
   Si ids=0 (objet immediat), alors on ne conserve que env_init.
*)


let transl_class ids cl_id arity pub_meths cl =
  (* First check if it is not only a rebind *)
  let rebind = transl_class_rebind ids cl in
  if rebind <> lambda_unit then rebind else

  (* Prepare for heavy environment handling *)
  let tables = Ident.create (Ident.name cl_id ^ "_tables") in
  let (top_env, req) = oo_add_class tables in
  let top = not req in
  let cl_env, llets = build_class_lets cl in
  let new_ids = if top then [] else Env.diff top_env cl_env in
  let env2 = Ident.create "env" in
  let meth_ids = get_class_meths cl in
  let subst env lam i0 new_ids' =
    let fv = free_variables lam in
    let fv = List.fold_right IdentSet.remove !new_ids' fv in
    (* IdentSet.iter
      (fun id ->
        if not (List.mem id new_ids) then prerr_endline (Ident.name id))
      fv; *)
    let fv = IdentSet.filter (fun id -> List.mem id new_ids) fv in
    (* need to handle methods specially (PR#3576) *)
    let fm = IdentSet.diff (free_methods lam) meth_ids in
    let fv = IdentSet.union fv fm in
    new_ids' := !new_ids' @ IdentSet.elements fv;
    let i = ref (i0-1) in
    List.fold_left
      (fun subst id ->
        incr i; Ident.add id (lfield env !i)  subst)
      Ident.empty !new_ids'
  in
  let new_ids_meths = ref [] in
  let msubst arr = function
      Lfunction (Curried, self :: args, body) ->
        let env = Ident.create "env" in
        let body' =
          if new_ids = [] then body else
          subst_lambda (subst env body 0 new_ids_meths) body in
        begin try
          (* Doesn't seem to improve size for bytecode *)
          (* if not !Clflags.native_code then raise Not_found; *)
          builtin_meths arr [self] env env2 (lfunction args body')
        with Not_found ->
          [lfunction (self :: args)
             (if not (IdentSet.mem env (free_variables body')) then body' else
              Llet(Alias, env,
                   Lprim(Parrayrefu Paddrarray,
                         [Lvar self; Lvar env2]), body'))]
        end
      | _ -> assert false
  in
  let new_ids_init = ref [] in
  let env1 = Ident.create "env" and env1' = Ident.create "env'" in
  let copy_env envs self =
    if top then lambda_unit else
    Lifused(env2, Lprim(Parraysetu Paddrarray,
                        [Lvar self; Lvar env2; Lvar env1']))
  and subst_env envs l lam =
    if top then lam else
    (* must be called only once! *)
    let lam = subst_lambda (subst env1 lam 1 new_ids_init) lam in
    Llet(Alias, env1, (if l = [] then Lvar envs else lfield envs 0),
    Llet(Alias, env1',
         (if !new_ids_init = [] then Lvar env1 else lfield env1 0),
         lam))
  in

  (* Now we start compiling the class *)
  let cla = Ident.create "class" in
  let (inh_init, obj_init) =
    build_object_init_0 cla [] cl copy_env subst_env top ids in
  if not (Translcore.check_recursive_lambda ids obj_init) then
    raise(Error(cl.cl_loc, Illegal_class_expr));
  let (inh_init', cl_init) =
    build_class_init cla true (List.rev inh_init) obj_init msubst top cl
  in
  assert (inh_init' = []);
  let table = Ident.create "table"
  and class_init = Ident.create (Ident.name cl_id ^ "_init")
  and env_init = Ident.create "env_init"
  and obj_init = Ident.create "obj_init" in
  let pub_meths =
    List.sort
      (fun s s' -> compare (Btype.hash_variant s) (Btype.hash_variant s'))
      pub_meths in
  let tags = List.map Btype.hash_variant pub_meths in
  let rev_map = List.combine tags pub_meths in
  List.iter2
    (fun tag name ->
      let name' = List.assoc tag rev_map in
      if name' <> name then raise(Error(cl.cl_loc, Tags(name, name'))))
    tags pub_meths;
  let ltable table lam =
    Llet(Strict, table,
         Lapply (oo_prim "create_table", [transl_meth_list pub_meths]), lam)
  and ldirect obj_init =
    Llet(Strict, obj_init, cl_init,
         Lsequence(Lapply (oo_prim "init_class", [Lvar cla]),
                   Lapply(Lvar obj_init, [lambda_unit])))
  in
  (* Simplest case: an object defined at toplevel (ids=[]) *)
  if top && ids = [] then llets (ltable cla (ldirect obj_init)) else

  let concrete =
    ids = [] ||
    Typeclass.virtual_methods (Ctype.signature_of_class_type cl.cl_type) = []
  and lclass lam =
    let cl_init = llets (Lfunction(Curried, [cla], cl_init)) in
    Llet(Strict, class_init, cl_init, lam (free_variables cl_init))
  and lbody fv =
    if List.for_all (fun id -> not (IdentSet.mem id fv)) ids then
      Lapply (oo_prim "make_class",[transl_meth_list pub_meths;
				    Lvar class_init])
    else
      ltable table (
      Llet(
      Strict, env_init, Lapply(Lvar class_init, [Lvar table]),
      Lsequence(
      Lapply (oo_prim "init_class", [Lvar table]),
      Lprim(Pmakeblock(0, Immutable),
	    [Lapply(Lvar env_init, [lambda_unit]);
	     Lvar class_init; Lvar env_init; lambda_unit]))))
  and lbody_virt lenvs =
    Lprim(Pmakeblock(0, Immutable),
          [lambda_unit; Lfunction(Curried,[cla], cl_init); lambda_unit; lenvs])
  in
  (* Still easy: a class defined at toplevel *)
  if top && concrete then lclass lbody else
  if top then llets (lbody_virt lambda_unit) else

  (* Now for the hard stuff: prepare for table cacheing *)
  let env_index = Ident.create "env_index"
  and envs = Ident.create "envs" in
  let lenvs =
    if !new_ids_meths = [] && !new_ids_init = [] && inh_init = []
    then lambda_unit
    else Lvar envs in
  let lenv =
    let menv =
      if !new_ids_meths = [] then lambda_unit else
      Lprim(Pmakeblock(0, Immutable),
            List.map (fun id -> Lvar id) !new_ids_meths) in
    if !new_ids_init = [] then menv else
    Lprim(Pmakeblock(0, Immutable),
          menv :: List.map (fun id -> Lvar id) !new_ids_init)
  and linh_envs =
    List.map (fun (_, p) -> Lprim(Pfield 3, [transl_path p]))
      (List.rev inh_init)
  in
  let make_envs lam =
    Llet(StrictOpt, envs,
         (if linh_envs = [] then lenv else
         Lprim(Pmakeblock(0, Immutable), lenv :: linh_envs)),
         lam)
  and def_ids cla lam =
    Llet(StrictOpt, env2,
         Lapply (oo_prim "new_variable", [Lvar cla; transl_label ""]),
         lam)
  in
  let obj_init2 = Ident.create "obj_init"
  and cached = Ident.create "cached" in
  let inh_paths =
    List.filter
      (fun (_,path) -> List.mem (Path.head path) new_ids) inh_init in
  let inh_keys =
    List.map (fun (_,p) -> Lprim(Pfield 1, [transl_path p])) inh_paths in
  let lclass lam =
    Llet(Strict, class_init,
         Lfunction(Curried, [cla], def_ids cla cl_init), lam)
  and lcache lam =
    if inh_keys = [] then Llet(Alias, cached, Lvar tables, lam) else
    Llet(Strict, cached,
         Lapply(oo_prim "lookup_tables",
                [Lvar tables; Lprim(Pmakeblock(0, Immutable), inh_keys)]),
         lam)
  and lset cached i lam =
    Lprim(Psetfield(i, true), [Lvar cached; lam])
  in
  let ldirect () =
    ltable cla
      (Llet(Strict, env_init, def_ids cla cl_init,
            Lsequence(Lapply (oo_prim "init_class", [Lvar cla]),
                      lset cached 0 (Lvar env_init))))
  and lclass_virt () =
    lset cached 0 (Lfunction(Curried, [cla], def_ids cla cl_init))
  in
  llets (
  lcache (
  Lsequence(
  Lifthenelse(lfield cached 0, lambda_unit,
              if ids = [] then ldirect () else
              if not concrete then lclass_virt () else
              lclass (
              Lapply (oo_prim "make_class_store",
                      [transl_meth_list pub_meths;
                       Lvar class_init; Lvar cached]))),
  make_envs (
  if ids = [] then Lapply(lfield cached 0, [lenvs]) else
  Lprim(Pmakeblock(0, Immutable),
        if concrete then
          [Lapply(lfield cached 0, [lenvs]);
           lfield cached 1;
           lfield cached 0;
           lenvs]
        else [lambda_unit; lfield cached 0; lambda_unit; lenvs]
       )))))

(* Dummy for recursive modules *)

let dummy_class undef_fn =
  Lprim(Pmakeblock(0, Mutable), [undef_fn; undef_fn; undef_fn; lambda_unit])

(* Wrapper for class compilation *)

let transl_class ids cl_id arity pub_meths cl =
  oo_wrap cl.cl_env false (transl_class ids cl_id arity pub_meths) cl

let () =
  transl_object := (fun id meths cl -> transl_class [] id 0 meths cl)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_class_expr ->
      fprintf ppf "This kind of class expression is not allowed"
  | Tags (lab1, lab2) ->
      fprintf ppf "Method labels `%s' and `%s' are incompatible.@ %s"
        lab1 lab2 "Change one of them."
