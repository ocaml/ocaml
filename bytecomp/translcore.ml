(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translation from typed abstract syntax to lambda terms,
   for the core language *)

open Misc
open Asttypes
open Primitive
open Types
open Typedtree
open Typeopt
open Lambda

type error =
    Free_super_var
  | Unreachable_reached

exception Error of Location.t * error

let use_dup_for_constant_arrays_bigger_than = 4

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun _cc _rootpath _modl -> assert false) :
      module_coercion -> Path.t option -> module_expr -> lambda)

let transl_object =
  ref (fun _id _s _cl -> assert false :
       Ident.t -> string list -> class_expr -> lambda)

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor env path ext =
  let path =
    Stdlib.Option.map (Printtyp.rewrite_double_underscore_paths env) path
  in
  let name =
    match path, !Clflags.for_package with
      None, _ -> Ident.name ext.ext_id
    | Some p, None -> Path.name p
    | Some p, Some pack -> Printf.sprintf "%s.%s" pack (Path.name p)
  in
  let loc = ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      Lprim (Pmakeblock (Obj.object_tag, Immutable, None),
        [Lconst (Const_base (Const_string (name, None)));
         Lprim (prim_fresh_oo_id, [Lconst (Const_base (Const_int 0))], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_extension_path ~loc env path

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* Push the default values under the functional abstractions *)
(* Also push bindings of module patterns, since this sound *)

type binding =
  | Bind_value of value_binding list
  | Bind_module of Ident.t * string loc * module_expr

let rec push_defaults loc bindings cases partial =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label; param; cases; partial; } }
        as exp}] ->
      let cases = push_defaults exp.exp_loc bindings cases partial in
      [{c_lhs=pat; c_guard=None;
        c_rhs={exp with exp_desc = Texp_function { arg_label; param; cases;
          partial; }}}]
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#default"},_];
             exp_desc = Texp_let
               (Nonrecursive, binds, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_value binds :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [{c_lhs=pat; c_guard=None;
      c_rhs={exp_attributes=[{txt="#modulepat"},_];
             exp_desc = Texp_letmodule
               (id, name, mexpr, ({exp_desc = Texp_function _} as e2))}}] ->
      push_defaults loc (Bind_module (id, name, mexpr) :: bindings)
                   [{c_lhs=pat;c_guard=None;c_rhs=e2}]
                   partial
  | [case] ->
      let exp =
        List.fold_left
          (fun exp binds ->
            {exp with exp_desc =
             match binds with
             | Bind_value binds -> Texp_let(Nonrecursive, binds, exp)
             | Bind_module (id, name, mexpr) ->
                 Texp_letmodule (id, name, mexpr, exp)})
          case.c_rhs bindings
      in
      [{case with c_rhs=exp}]
  | {c_lhs=pat; c_rhs=exp; c_guard=_} :: _ when bindings <> [] ->
      let param = Typecore.name_pattern "param" cases in
      let name = Ident.name param in
      let exp =
        { exp with exp_loc = loc; exp_desc =
          Texp_match
            ({exp with exp_type = pat.pat_type; exp_desc =
              Texp_ident (Path.Pident param, mknoloc (Longident.Lident name),
                          {val_type = pat.pat_type; val_kind = Val_reg;
                           val_attributes = [];
                           Types.val_loc = Location.none;
                          })},
             cases, [], partial) }
      in
      push_defaults loc bindings
        [{c_lhs={pat with pat_desc = Tpat_var (param, mknoloc name)};
          c_guard=None; c_rhs=exp}]
        Total
  | _ ->
      cases

(* Insertion of debugging events *)

let event_before = Translprim.event_before

let event_after = Translprim.event_after

let event_function exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = Env.summary exp.exp_env}))
  else
    lam None

(* Assertions *)

let assert_failed exp =
  let (fname, line, char) =
    Location.get_pos_info exp.exp_loc.Location.loc_start in
  Lprim(Praise Raise_regular, [event_after exp
    (Lprim(Pmakeblock(0, Immutable, None),
          [transl_normal_path Predef.path_assert_failure;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], exp.exp_loc))], exp.exp_loc)
;;

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* Translation of expressions *)

let rec transl_exp e =
  List.iter (Translattribute.check_attribute e) e.exp_attributes;
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 e else
  Translobj.oo_wrap e.exp_env true transl_exp0 e

and transl_exp0 e =
  match e.exp_desc with
  | Texp_ident(path, _, {val_kind = Val_prim p}) ->
      Translprim.transl_primitive e.exp_loc p e.exp_env e.exp_type (Some path)
  | Texp_ident(_, _, {val_kind = Val_anc _}) ->
      raise(Error(e.exp_loc, Free_super_var))
  | Texp_ident(path, _, {val_kind = Val_reg | Val_self _}) ->
      transl_value_path ~loc:e.exp_loc e.exp_env path
  | Texp_ident _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let rec_flag pat_expr_list (event_before body (transl_exp body))
  | Texp_function { arg_label = _; param; cases; partial; } ->
      let ((kind, params), body) =
        event_function e
          (function repr ->
            let pl = push_defaults e.exp_loc [] cases partial in
            transl_function e.exp_loc !Clflags.native_code repr partial
              param pl)
      in
      let attr = {
        default_function_attribute with
        inline = Translattribute.get_inline_attribute e.exp_attributes;
        specialise = Translattribute.get_specialise_attribute e.exp_attributes;
      }
      in
      let loc = e.exp_loc in
      Lfunction{kind; params; body; attr; loc}
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p});
                exp_type = prim_type } as funct, oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg) -> arg <> None) oargs ->
      let argl, extra_args = cut p.prim_arity oargs in
      let arg_exps =
         List.map (function _, Some x -> x | _ -> assert false) argl
      in
      let args = transl_list arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let lam =
        Translprim.transl_primitive_application
          e.exp_loc p e.exp_env prim_type path
          prim_exp args arg_exps
      in
      if extra_args = [] then lam
      else begin
        let should_be_tailcall, funct =
          Translattribute.get_tailcall_attribute funct
        in
        let inlined, funct =
          Translattribute.get_and_remove_inlined_attribute funct
        in
        let specialised, funct =
          Translattribute.get_and_remove_specialised_attribute funct
        in
        let e = { e with exp_desc = Texp_apply(funct, oargs) } in
        event_after e
          (transl_apply ~should_be_tailcall ~inlined ~specialised
             lam extra_args e.exp_loc)
      end
  | Texp_apply(funct, oargs) ->
      let should_be_tailcall, funct =
        Translattribute.get_tailcall_attribute funct
      in
      let inlined, funct =
        Translattribute.get_and_remove_inlined_attribute funct
      in
      let specialised, funct =
        Translattribute.get_and_remove_specialised_attribute funct
      in
      let e = { e with exp_desc = Texp_apply(funct, oargs) } in
      event_after e
        (transl_apply ~should_be_tailcall ~inlined ~specialised
           (transl_exp funct) oargs e.exp_loc)
  | Texp_match(arg, pat_expr_list, exn_pat_expr_list, partial) ->
    transl_match e arg pat_expr_list exn_pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_pattern "exn" pat_expr_list in
      Ltrywith(transl_exp body, id,
               Matching.for_trywith (Lvar id) (transl_cases_try pat_expr_list))
  | Texp_tuple el ->
      let ll, shape = transl_list_with_shape el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape), ll, e.exp_loc)
      end
  | Texp_construct(_, cstr, args) ->
      let ll, shape = transl_list_with_shape args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(Const_pointer n)
      | Cstr_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, Some shape), ll, e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          if is_const then
            transl_extension_path e.exp_env path
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape)),
                  transl_extension_path e.exp_env path :: ll, e.exp_loc)
      end
  | Texp_extension_constructor (_, path) ->
      transl_extension_path e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(Const_pointer tag)
      | Some arg ->
          let lam = transl_exp arg in
          try
            Lconst(Const_block(0, [Const_base(Const_int tag);
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None),
                  [Lconst(Const_base(Const_int tag)); lam], e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression} ->
      transl_record e.exp_loc e.exp_env fields representation
        extended_expression
  | Texp_field(arg, _, lbl) ->
      let targ = transl_exp arg in
      begin match lbl.lbl_repres with
          Record_regular | Record_inlined _ ->
          Lprim (Pfield lbl.lbl_pos, [targ], e.exp_loc)
        | Record_unboxed _ -> targ
        | Record_float -> Lprim (Pfloatfield lbl.lbl_pos, [targ], e.exp_loc)
        | Record_extension ->
          Lprim (Pfield (lbl.lbl_pos + 1), [targ], e.exp_loc)
      end
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular
        | Record_inlined _ ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, Assignment)
        | Record_unboxed _ -> assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
        | Record_extension ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, Assignment)
      in
      Lprim(access, [transl_exp arg; transl_exp newval], e.exp_loc)
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list expr_list in
      begin try
        (* For native code the decision as to which compilation strategy to
           use is made later.  This enables the Flambda passes to lift certain
           kinds of array definitions to symbols. *)
        (* Deactivate constant optimization if array is small enough *)
        if List.length ll <= use_dup_for_constant_arrays_bigger_than
        then begin
          raise Not_constant
        end;
        begin match List.map extract_constant ll with
        | exception Not_constant when kind = Pfloatarray ->
            (* We cannot currently lift [Pintarray] arrays safely in Flambda
               because [caml_modify] might be called upon them (e.g. from
               code operating on polymorphic arrays, or functions such as
               [caml_array_blit].
               To avoid having different Lambda code for
               bytecode/Closure vs.  Flambda, we always generate
               [Pduparray] here, and deal with it in [Bytegen] (or in
               the case of Closure, in [Cmmgen], which already has to
               handle [Pduparray Pmakearray Pfloatarray] in the case
               where the array turned out to be inconstant).
               When not [Pfloatarray], the exception propagates to the handler
               below. *)
            let imm_array =
              Lprim (Pmakearray (kind, Immutable), ll, e.exp_loc)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array], e.exp_loc)
        | cl ->
            let imm_array =
              match kind with
              | Paddrarray | Pintarray ->
                  Lconst(Const_block(0, cl))
              | Pfloatarray ->
                  Lconst(Const_float_array(List.map extract_float cl))
              | Pgenarray ->
                  raise Not_constant    (* can this really happen? *)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array], e.exp_loc)
        end
      with Not_constant ->
        Lprim(Pmakearray (kind, Mutable), ll, e.exp_loc)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  event_before ifnot (transl_exp ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp cond,
                  event_before ifso (transl_exp ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp expr1, event_before expr2 (transl_exp expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp cond, event_before body (transl_exp body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp low, transl_exp high, dir,
           event_before body (transl_exp body))
  | Texp_send(_, _, Some exp) -> transl_exp exp
  | Texp_send(expr, met, None) ->
      let obj = transl_exp expr in
      let lam =
        match met with
          Tmeth_val id -> Lsend (Self, Lvar id, obj, [], e.exp_loc)
        | Tmeth_name nm ->
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, e.exp_loc)
      in
      event_after e lam
  | Texp_new (cl, {Location.loc=loc}, _) ->
      Lapply{ap_should_be_tailcall=false;
             ap_loc=loc;
             ap_func=Lprim(Pfield 0, [transl_class_path ~loc e.exp_env cl], loc);
             ap_args=[lambda_unit];
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise}
  | Texp_instvar(path_self, path, _) ->
      Lprim(Pfield_computed,
            [transl_normal_path path_self; transl_normal_path path], e.exp_loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      transl_setinstvar e.exp_loc (transl_normal_path path_self) path expr
  | Texp_override(path_self, modifs) ->
      let cpy = Ident.create "copy" in
      Llet(Strict, Pgenval, cpy,
           Lapply{ap_should_be_tailcall=false;
                  ap_loc=Location.none;
                  ap_func=Translobj.oo_prim "copy";
                  ap_args=[transl_normal_path path_self];
                  ap_inlined=Default_inline;
                  ap_specialised=Default_specialise},
           List.fold_right
             (fun (path, _, expr) rem ->
                Lsequence(transl_setinstvar Location.none
                            (Lvar cpy) path expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(id, loc, modl, body) ->
      let defining_expr =
        Levent (!transl_module Tcoerce_none None modl, {
          lev_loc = loc.loc;
          lev_kind = Lev_module_definition id;
          lev_repr = None;
          lev_env = Env.summary Env.empty;
        })
      in
      Llet(Strict, Pgenval, id, defining_expr, transl_exp body)
  | Texp_letexception(cd, body) ->
      Llet(Strict, Pgenval,
           cd.ext_id, transl_extension_constructor e.exp_env None cd,
           transl_exp body)
  | Texp_pack modl ->
      !transl_module Tcoerce_none None modl
  | Texp_assert {exp_desc=Texp_construct(_, {cstr_name="false"}, _)} ->
      assert_failed e
  | Texp_assert (cond) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp cond, lambda_unit, assert_failed e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* a constant expr of type <> float gets compiled as itself *)
         transl_exp e
      | `Float ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float. *)
          Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                [transl_exp e], e.exp_loc)
      | `Identifier `Forward_value ->
         (* CR-someday mshinwell: Consider adding a new primitive
            that expresses the construction of forward_tag blocks.
            We need to use [Popaque] here to prevent unsound
            optimisation in Flambda, but the concept of a mutable
            block doesn't really match what is going on here.  This
            value may subsequently turn into an immediate... *)
         Lprim (Popaque,
                [Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                       [transl_exp e], e.exp_loc)],
                e.exp_loc)
      | `Identifier `Other ->
         transl_exp e
      | `Other ->
         (* other cases compile to a lazy block holding a function *)
         let fn = Lfunction {kind = Curried; params = [Ident.create "param"];
                             attr = default_function_attribute;
                             loc = e.exp_loc;
                             body = transl_exp e} in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, None), [fn], e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create "class" in
      !transl_object cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))

and transl_list expr_list =
  List.map transl_exp expr_list

and transl_list_with_shape expr_list =
  let transl_with_shape e =
    let shape = Typeopt.value_kind e.exp_env e.exp_type in
    transl_exp e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard guard rhs =
  let expr = event_before rhs (transl_exp rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before cond (Lifthenelse(transl_exp cond, expr, staticfail))

and transl_case {c_lhs; c_guard; c_rhs} =
  c_lhs, transl_guard c_guard c_rhs

and transl_cases cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map transl_case cases

and transl_case_try {c_lhs; c_guard; c_rhs} =
  let rec iter_exn_names f pat =
    match pat.pat_desc with
    | Tpat_var (id, _) -> f id
    | Tpat_alias (p, id, _) ->
        f id;
        iter_exn_names f p
    | _ -> ()
  in
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () -> c_lhs, transl_guard c_guard c_rhs)
    (fun () ->
       iter_exn_names Translprim.remove_exception_ident c_lhs)

and transl_cases_try cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map transl_case_try cases

and transl_tupled_cases patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map (fun (patl, guard, expr) -> (patl, transl_guard guard expr))
    patl_expr_list

and transl_apply ?(should_be_tailcall=false) ?(inlined = Default_inline)
      ?(specialised = Default_specialise) lam sargs loc =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, loc) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, loc), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply ap ->
        Lapply {ap with ap_args = ap.ap_args @ args; ap_loc = loc}
    | lexp ->
        Lapply {ap_should_be_tailcall=should_be_tailcall;
                ap_loc=loc;
                ap_func=lexp;
                ap_args=args;
                ap_inlined=inlined;
                ap_specialised=specialised;}
  in
  let rec build_apply lam args = function
      (None, optional) :: l ->
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        let args, args' =
          if List.for_all (fun (_,opt) -> opt) args then [], args
          else args, [] in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args) in
        let handle = protect "func" lam
        and l = List.map (fun (arg, opt) -> may_map (protect "arg") arg, opt) l
        and id_arg = Ident.create "param" in
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction{kind = Curried; params = ids; body = lam; attr; loc} ->
              Lfunction{kind = Curried; params = id_arg::ids; body = lam; attr;
                        loc}
          | Levent(Lfunction{kind = Curried; params = ids;
                             body = lam; attr; loc}, _) ->
              Lfunction{kind = Curried; params = id_arg::ids; body = lam; attr;
                        loc}
          | lam ->
              Lfunction{kind = Curried; params = [id_arg]; body = lam;
                        attr = default_stub_attribute; loc = loc}
        in
        List.fold_left
          (fun body (id, lam) -> Llet(Strict, Pgenval, id, lam, body))
          body !defs
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  (build_apply lam [] (List.map (fun (l, x) ->
                                   may_map transl_exp x, Btype.is_optional l)
                                sargs)
     : Lambda.lambda)

and transl_function loc untuplify_fn repr partial param cases =
  match cases with
    [{c_lhs=pat; c_guard=None;
      c_rhs={exp_desc = Texp_function { arg_label = _; param = param'; cases;
        partial = partial'; }} as exp}]
    when Parmatch.inactive ~partial pat ->
      let ((_, params), body) =
        transl_function exp.exp_loc false repr partial' param' cases in
      ((Curried, param :: params),
       Matching.for_function loc None (Lvar param) [pat, body] partial)
  | {c_lhs={pat_desc = Tpat_tuple pl}} :: _ when untuplify_fn ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let params = List.map (fun _ -> Ident.create "param") pl in
        ((Tupled, params),
         Matching.for_tupled_function loc params
           (transl_tupled_cases pats_expr_list) partial)
      with Matching.Cannot_flatten ->
        ((Curried, [param]),
         Matching.for_function loc repr (Lvar param)
           (transl_cases cases) partial)
      end
  | _ ->
      ((Curried, [param]),
       Matching.for_function loc repr (Lvar param)
         (transl_cases cases) partial)

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_attributes=attr; vb_loc} :: rem ->
          let lam = transl_exp expr in
          let lam =
            Translattribute.add_inline_attribute lam vb_loc attr
          in
          let lam =
            Translattribute.add_specialise_attribute lam vb_loc attr
          in
          let mk_body = transl rem in
          fun body -> Matching.for_let pat.pat_loc lam pat (mk_body body)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case {vb_expr=expr; vb_attributes; vb_loc} id =
        let lam = transl_exp expr in
        let lam =
          Translattribute.add_inline_attribute lam vb_loc
            vb_attributes
        in
        let lam =
          Translattribute.add_specialise_attribute lam vb_loc
            vb_attributes
        in
        (id, lam) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Lletrec(lam_bds, body)

and transl_setinstvar loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment),
    [self; transl_normal_path var; transl_exp expr], loc)

and transl_record loc env fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  if no_init || size < Config.max_young_wosize
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let init_id = Ident.create "init" in
    let lv =
      Array.mapi
        (fun i (_, definition) ->
           match definition with
           | Kept typ ->
               let field_kind = value_kind env typ in
               let access =
                 match repres with
                   Record_regular | Record_inlined _ -> Pfield i
                 | Record_unboxed _ -> assert false
                 | Record_extension -> Pfield (i + 1)
                 | Record_float -> Pfloatfield i in
               Lprim(access, [Lvar init_id], loc), field_kind
           | Overridden (_lid, expr) ->
               let field_kind = value_kind expr.exp_env expr.exp_type in
               transl_exp expr, field_kind)
        fields
    in
    let ll, shape = List.split (Array.to_list lv) in
    let mut =
      if Array.exists (fun (lbl, _) -> lbl.lbl_mut = Mutable) fields
      then Mutable
      else Immutable in
    let lam =
      try
        if mut = Mutable then raise Not_constant;
        let cl = List.map extract_constant ll in
        match repres with
        | Record_regular -> Lconst(Const_block(0, cl))
        | Record_inlined tag -> Lconst(Const_block(tag, cl))
        | Record_unboxed _ -> Lconst(match cl with [v] -> v | _ -> assert false)
        | Record_float ->
            Lconst(Const_float_array(List.map extract_float cl))
        | Record_extension ->
            raise Not_constant
      with Not_constant ->
        match repres with
          Record_regular ->
            Lprim(Pmakeblock(0, mut, Some shape), ll, loc)
        | Record_inlined tag ->
            Lprim(Pmakeblock(tag, mut, Some shape), ll, loc)
        | Record_unboxed _ -> (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakearray (Pfloatarray, mut), ll, loc)
        | Record_extension ->
            let path =
              let (label, _) = fields.(0) in
              match label.lbl_res.desc with
              | Tconstr(p, _, _) -> p
              | _ -> assert false
            in
            let slot = transl_extension_path env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape)), slot :: ll, loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Pgenval, init_id,
                             transl_exp init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create "newrecord" in
    let update_field cont (lbl, definition) =
      match definition with
      | Kept _type -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_regular
            | Record_inlined _ ->
                Psetfield(lbl.lbl_pos, maybe_pointer expr, Assignment)
            | Record_unboxed _ -> assert false
            | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
            | Record_extension ->
                Psetfield(lbl.lbl_pos + 1, maybe_pointer expr, Assignment)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp expr], loc), cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, Pgenval, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp init_expr], loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match e arg pat_expr_list exn_pat_expr_list partial =
  let id = Typecore.name_pattern "exn" exn_pat_expr_list
  and cases = transl_cases pat_expr_list
  and exn_cases = transl_cases_try exn_pat_expr_list in
  let static_catch body val_ids handler =
    let static_exception_id = next_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, body), id,
                 Matching.for_trywith (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  match arg, exn_cases with
  | {exp_desc = Texp_tuple argl}, [] ->
    Matching.for_multiple_match e.exp_loc (transl_list argl) cases partial
  | {exp_desc = Texp_tuple argl}, _ :: _ ->
    let val_ids = List.map (fun _ -> Typecore.name_pattern "val" []) argl in
    let lvars = List.map (fun id -> Lvar id) val_ids in
    static_catch (transl_list argl) val_ids
      (Matching.for_multiple_match e.exp_loc lvars cases partial)
  | arg, [] ->
    Matching.for_function e.exp_loc None (transl_exp arg) cases partial
  | arg, _ :: _ ->
    let val_id = Typecore.name_pattern "val" pat_expr_list in
    static_catch [transl_exp arg] [val_id]
      (Matching.for_function e.exp_loc None (Lvar val_id) cases partial)


(* Wrapper for class compilation *)

(*
let transl_exp = transl_exp_wrap

let transl_let rec_flag pat_expr_list body =
  match pat_expr_list with
    [] -> body
  | (_, expr) :: _ ->
      Translobj.oo_wrap expr.exp_env false
        (transl_let rec_flag pat_expr_list) body
*)

(* Error report *)

open Format

let report_error ppf = function
  | Free_super_var ->
      fprintf ppf
        "Ancestor names can only be used to select inherited methods"
  | Unreachable_reached ->
      fprintf ppf "Unreachable expression was reached"

let () =
  Location.register_error_of_exn
    (function
      | Error (loc, err) ->
          Some (Location.error_of_printer loc report_error err)
      | _ ->
        None
    )
