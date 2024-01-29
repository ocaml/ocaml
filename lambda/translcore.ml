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
open Debuginfo.Scoped_location

type error =
    Free_super_var
  | Unreachable_reached

exception Error of Location.t * error

let use_dup_for_constant_arrays_bigger_than = 4

(* Forward declaration -- to be filled in by Translmod.transl_module *)
let transl_module =
  ref((fun ~scopes:_ _cc _rootpath _modl -> assert false) :
      scopes:scopes -> module_coercion -> Path.t option ->
      module_expr -> lambda)

let transl_object =
  ref (fun ~scopes:_ _id _s _cl -> assert false :
       scopes:scopes -> Ident.t -> string list -> class_expr -> lambda)

(* Compile an exception/extension definition *)

let prim_fresh_oo_id =
  Pccall (Primitive.simple ~name:"caml_fresh_oo_id" ~arity:1 ~alloc:false)

let transl_extension_constructor ~scopes env path ext =
  let path =
    Printtyp.wrap_printing_env env ~error:true (fun () ->
      Option.map (Printtyp.rewrite_double_underscore_paths env) path)
  in
  let name =
    match path, !Clflags.for_package with
      None, _ -> Ident.name ext.ext_id
    | Some p, None -> Path.name p
    | Some p, Some pack -> Printf.sprintf "%s.%s" pack (Path.name p)
  in
  let loc = of_location ~scopes ext.ext_loc in
  match ext.ext_kind with
    Text_decl _ ->
      Lprim (Pmakeblock (Obj.object_tag, Immutable, None),
        [Lconst (Const_base (Const_string (name, ext.ext_loc, None)));
         Lprim (prim_fresh_oo_id, [Lconst (const_int 0)], loc)],
        loc)
  | Text_rebind(path, _lid) ->
      transl_extension_path loc env path

(* To propagate structured constants *)

exception Not_constant

let extract_constant = function
    Lconst sc -> sc
  | _ -> raise Not_constant

let extract_float = function
    Const_base(Const_float f) -> f
  | _ -> fatal_error "Translcore.extract_float"

(* Insertion of debugging events *)

let event_before ~scopes exp lam =
  Translprim.event_before (of_location ~scopes exp.exp_loc) exp lam

let event_after ~scopes exp lam =
  Translprim.event_after (of_location ~scopes exp.exp_loc) exp lam

let event_function ~scopes exp lam =
  if !Clflags.debug && not !Clflags.native_code then
    let repr = Some (ref 0) in
    let (info, body) = lam repr in
    (info,
     Levent(body, {lev_loc = of_location ~scopes exp.exp_loc;
                   lev_kind = Lev_function;
                   lev_repr = repr;
                   lev_env = exp.exp_env}))
  else
    lam None

(* Assertions *)

let assert_failed loc ~scopes exp =
  let slot =
    transl_extension_path Loc_unknown
      Env.initial Predef.path_assert_failure
  in
  let (fname, line, char) =
    Location.get_pos_info loc.Location.loc_start
  in
  let loc = of_location ~scopes exp.exp_loc in
  Lprim(Praise Raise_regular, [event_after ~scopes exp
    (Lprim(Pmakeblock(0, Immutable, None),
          [slot;
           Lconst(Const_block(0,
              [Const_base(Const_string (fname, exp.exp_loc, None));
               Const_base(Const_int line);
               Const_base(Const_int char)]))], loc))], loc)

(* In cases where we're careful to preserve syntactic arity, we disable
   the arity fusion attempted by simplif.ml *)
let function_attribute_disallowing_arity_fusion =
  { default_function_attribute with may_fuse_arity = false }

let rec cut n l =
  if n = 0 then ([],l) else
  match l with [] -> failwith "Translcore.cut"
  | a::l -> let (l1,l2) = cut (n-1) l in (a::l1,l2)

(* [fuse_method_arity] is what ensures that a n-ary method is compiled as a
   (n+1)-ary function, where the first parameter is self. It fuses together the
   self and method parameters.

   Input:  fun self -> fun method_param_1 ... method_param_n -> body
   Output: fun self method_param_1 ... method_param_n -> body

   It detects whether the AST is a method by the presence of [Texp_poly] on the
   inner function. This is only ever added to methods.
*)
let fuse_method_arity parent_params parent_body =
  match parent_body with
  | Tfunction_body
      { exp_desc = Texp_function (method_params, method_body);
        exp_extra;
      }
      when
        List.exists
          (function (Texp_poly _, _, _) -> true | _ -> false)
          exp_extra
    -> parent_params @ method_params, method_body
  | _ -> parent_params, parent_body

(* Translation of expressions *)

let rec iter_exn_names f pat =
  match pat.pat_desc with
  | Tpat_var (id, _, _) -> f id
  | Tpat_alias (p, id, _, _) ->
      f id;
      iter_exn_names f p
  | _ -> ()

let transl_ident loc env ty path desc =
  match desc.val_kind with
  | Val_prim p ->
      Translprim.transl_primitive loc p env ty (Some path)
  | Val_anc _ ->
      raise(Error(to_location loc, Free_super_var))
  | Val_reg | Val_self _ ->
      transl_value_path loc env path
  |  _ -> fatal_error "Translcore.transl_exp: bad Texp_ident"

let rec transl_exp ~scopes e =
  transl_exp1 ~scopes ~in_new_scope:false e

(* ~in_new_scope tracks whether we just opened a new scope.

   When we just opened a new scope, we avoid introducing an extraneous anonymous
   function scope and instead inherit the new scope. E.g., [let f x = ...] is
   parsed as a let-bound Pexp_function node [let f = fun x -> ...].
   We give it f's scope.
*)
and transl_exp1 ~scopes ~in_new_scope e =
  let eval_once =
    (* Whether classes for immediate objects must be cached *)
    match e.exp_desc with
      Texp_function _ | Texp_for _ | Texp_while _ -> false
    | _ -> true
  in
  if eval_once then transl_exp0 ~scopes ~in_new_scope  e else
  Translobj.oo_wrap e.exp_env true (transl_exp0 ~scopes ~in_new_scope) e

and transl_exp0 ~in_new_scope ~scopes e =
  match e.exp_desc with
  | Texp_ident(path, _, desc) ->
      transl_ident (of_location ~scopes e.exp_loc)
        e.exp_env e.exp_type path desc
  | Texp_constant cst ->
      Lconst(Const_base cst)
  | Texp_let(rec_flag, pat_expr_list, body) ->
      transl_let ~scopes rec_flag pat_expr_list
        (event_before ~scopes body (transl_exp ~scopes body))
  | Texp_function (params, body) ->
      let scopes =
        if in_new_scope then scopes
        else enter_anonymous_function ~scopes
      in
      transl_function ~scopes e params body
  | Texp_apply({ exp_desc = Texp_ident(path, _, {val_kind = Val_prim p});
                exp_type = prim_type } as funct, oargs)
    when List.length oargs >= p.prim_arity
    && List.for_all (fun (_, arg) -> arg <> None) oargs ->
      let argl, extra_args = cut p.prim_arity oargs in
      let arg_exps =
         List.map (function _, Some x -> x | _ -> assert false) argl
      in
      let args = transl_list ~scopes arg_exps in
      let prim_exp = if extra_args = [] then Some e else None in
      let lam =
        Translprim.transl_primitive_application
          (of_location ~scopes e.exp_loc) p e.exp_env prim_type path
          prim_exp args arg_exps
      in
      if extra_args = [] then lam
      else begin
        let tailcall = Translattribute.get_tailcall_attribute funct in
        let inlined = Translattribute.get_inlined_attribute funct in
        let specialised = Translattribute.get_specialised_attribute funct in
        let e = { e with exp_desc = Texp_apply(funct, oargs) } in
        event_after ~scopes e
          (transl_apply ~scopes ~tailcall ~inlined ~specialised
             lam extra_args (of_location ~scopes e.exp_loc))
      end
  | Texp_apply(funct, oargs) ->
      let tailcall = Translattribute.get_tailcall_attribute funct in
      let inlined = Translattribute.get_inlined_attribute funct in
      let specialised = Translattribute.get_specialised_attribute funct in
      let e = { e with exp_desc = Texp_apply(funct, oargs) } in
      event_after ~scopes e
        (transl_apply ~scopes ~tailcall ~inlined ~specialised
           (transl_exp ~scopes funct) oargs (of_location ~scopes e.exp_loc))
  | Texp_match(arg, pat_expr_list, partial) ->
      transl_match ~scopes e arg pat_expr_list partial
  | Texp_try(body, pat_expr_list) ->
      let id = Typecore.name_cases "exn" pat_expr_list in
      Ltrywith(transl_exp ~scopes body, id,
               Matching.for_trywith ~scopes e.exp_loc (Lvar id)
                 (transl_cases_try ~scopes pat_expr_list))
  | Texp_tuple el ->
      let ll, shape = transl_list_with_shape ~scopes el in
      begin try
        Lconst(Const_block(0, List.map extract_constant ll))
      with Not_constant ->
        Lprim(Pmakeblock(0, Immutable, Some shape), ll,
              (of_location ~scopes e.exp_loc))
      end
  | Texp_construct(_, cstr, args) ->
      let ll, shape = transl_list_with_shape ~scopes args in
      if cstr.cstr_inlined <> None then begin match ll with
        | [x] -> x
        | _ -> assert false
      end else begin match cstr.cstr_tag with
        Cstr_constant n ->
          Lconst(const_int n)
      | Cstr_unboxed ->
          (match ll with [v] -> v | _ -> assert false)
      | Cstr_block n ->
          begin try
            Lconst(Const_block(n, List.map extract_constant ll))
          with Not_constant ->
            Lprim(Pmakeblock(n, Immutable, Some shape), ll,
                  of_location ~scopes e.exp_loc)
          end
      | Cstr_extension(path, is_const) ->
          let lam = transl_extension_path
                      (of_location ~scopes e.exp_loc) e.exp_env path in
          if is_const then lam
          else
            Lprim(Pmakeblock(0, Immutable, Some (Pgenval :: shape)),
                  lam :: ll, of_location ~scopes e.exp_loc)
      end
  | Texp_extension_constructor (_, path) ->
      transl_extension_path (of_location ~scopes e.exp_loc) e.exp_env path
  | Texp_variant(l, arg) ->
      let tag = Btype.hash_variant l in
      begin match arg with
        None -> Lconst(const_int tag)
      | Some arg ->
          let lam = transl_exp ~scopes arg in
          try
            Lconst(Const_block(0, [const_int tag;
                                   extract_constant lam]))
          with Not_constant ->
            Lprim(Pmakeblock(0, Immutable, None),
                  [Lconst(const_int tag); lam],
                  of_location ~scopes e.exp_loc)
      end
  | Texp_record {fields; representation; extended_expression} ->
      transl_record ~scopes e.exp_loc e.exp_env
        fields representation extended_expression
  | Texp_field(arg, _, lbl) ->
      let targ = transl_exp ~scopes arg in
      begin match lbl.lbl_repres with
          Record_regular | Record_inlined _ ->
          Lprim (Pfield (lbl.lbl_pos, maybe_pointer e, lbl.lbl_mut), [targ],
                 of_location ~scopes e.exp_loc)
        | Record_unboxed _ -> targ
        | Record_float ->
          Lprim (Pfloatfield lbl.lbl_pos, [targ],
                 of_location ~scopes e.exp_loc)
        | Record_extension _ ->
          Lprim (Pfield (lbl.lbl_pos + 1, maybe_pointer e, lbl.lbl_mut), [targ],
                 of_location ~scopes e.exp_loc)
      end
  | Texp_setfield(arg, _, lbl, newval) ->
      let access =
        match lbl.lbl_repres with
          Record_regular
        | Record_inlined _ ->
          Psetfield(lbl.lbl_pos, maybe_pointer newval, Assignment)
        | Record_unboxed _ -> assert false
        | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
        | Record_extension _ ->
          Psetfield (lbl.lbl_pos + 1, maybe_pointer newval, Assignment)
      in
      Lprim(access, [transl_exp ~scopes arg; transl_exp ~scopes newval],
            of_location ~scopes e.exp_loc)
  | Texp_array expr_list ->
      let kind = array_kind e in
      let ll = transl_list ~scopes expr_list in
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
              Lprim (Pmakearray (kind, Immutable), ll,
                     of_location ~scopes e.exp_loc)
            in
            Lprim (Pduparray (kind, Mutable), [imm_array],
                   of_location ~scopes e.exp_loc)
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
            Lprim (Pduparray (kind, Mutable), [imm_array],
                   of_location ~scopes e.exp_loc)
        end
      with Not_constant ->
        Lprim(Pmakearray (kind, Mutable), ll,
              of_location ~scopes e.exp_loc)
      end
  | Texp_ifthenelse(cond, ifso, Some ifnot) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  event_before ~scopes ifnot (transl_exp ~scopes ifnot))
  | Texp_ifthenelse(cond, ifso, None) ->
      Lifthenelse(transl_exp ~scopes cond,
                  event_before ~scopes ifso (transl_exp ~scopes ifso),
                  lambda_unit)
  | Texp_sequence(expr1, expr2) ->
      Lsequence(transl_exp ~scopes expr1,
                event_before ~scopes expr2 (transl_exp ~scopes expr2))
  | Texp_while(cond, body) ->
      Lwhile(transl_exp ~scopes cond,
             event_before ~scopes body (transl_exp ~scopes body))
  | Texp_for(param, _, low, high, dir, body) ->
      Lfor(param, transl_exp ~scopes low, transl_exp ~scopes high, dir,
           event_before ~scopes body (transl_exp ~scopes body))
  | Texp_send(expr, met) ->
      let lam =
        let loc = of_location ~scopes e.exp_loc in
        match met with
        | Tmeth_val id ->
            let obj = transl_exp ~scopes expr in
            Lsend (Self, Lvar id, obj, [], loc)
        | Tmeth_name nm ->
            let obj = transl_exp ~scopes expr in
            let (tag, cache) = Translobj.meth obj nm in
            let kind = if cache = [] then Public else Cached in
            Lsend (kind, tag, obj, cache, loc)
        | Tmeth_ancestor(meth, path_self) ->
            let self = transl_value_path loc e.exp_env path_self in
            Lapply {ap_loc = loc;
                    ap_func = Lvar meth;
                    ap_args = [self];
                    ap_tailcall = Default_tailcall;
                    ap_inlined = Default_inline;
                    ap_specialised = Default_specialise}
      in
      event_after ~scopes e lam
  | Texp_new (cl, {Location.loc=loc}, _) ->
      let loc = of_location ~scopes loc in
      Lapply{
        ap_loc=loc;
        ap_func=
          Lprim(Pfield (0, Pointer, Mutable),
                [transl_class_path loc e.exp_env cl], loc);
        ap_args=[lambda_unit];
        ap_tailcall=Default_tailcall;
        ap_inlined=Default_inline;
        ap_specialised=Default_specialise;
      }
  | Texp_instvar(path_self, path, _) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      Lprim(Pfield_computed, [self; var], loc)
  | Texp_setinstvar(path_self, path, _, expr) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let var = transl_value_path loc e.exp_env path in
      transl_setinstvar ~scopes loc self var expr
  | Texp_override(path_self, modifs) ->
      let loc = of_location ~scopes e.exp_loc in
      let self = transl_value_path loc e.exp_env path_self in
      let cpy = Ident.create_local "copy" in
      Llet(Strict, Pgenval, cpy,
           Lapply{
             ap_loc=Loc_unknown;
             ap_func=Translobj.oo_prim "copy";
             ap_args=[self];
             ap_tailcall=Default_tailcall;
             ap_inlined=Default_inline;
             ap_specialised=Default_specialise;
           },
           List.fold_right
             (fun (id, _, expr) rem ->
                Lsequence(transl_setinstvar ~scopes Loc_unknown
                            (Lvar cpy) (Lvar id) expr, rem))
             modifs
             (Lvar cpy))
  | Texp_letmodule(None, loc, Mp_present, modl, body) ->
      let lam = !transl_module ~scopes Tcoerce_none None modl in
      Lsequence(Lprim(Pignore, [lam], of_location ~scopes loc.loc),
                transl_exp ~scopes body)
  | Texp_letmodule(Some id, _loc, Mp_present, modl, body) ->
      let defining_expr =
        let mod_scopes = enter_module_definition ~scopes id in
        !transl_module ~scopes:mod_scopes Tcoerce_none None modl
      in
      Llet(Strict, Pgenval, id, defining_expr, transl_exp ~scopes body)
  | Texp_letmodule(_, _, Mp_absent, _, body) ->
      transl_exp ~scopes body
  | Texp_letexception(cd, body) ->
      Llet(Strict, Pgenval,
           cd.ext_id, transl_extension_constructor ~scopes e.exp_env None cd,
           transl_exp ~scopes body)
  | Texp_pack modl ->
      !transl_module ~scopes Tcoerce_none None modl
  | Texp_assert ({exp_desc=Texp_construct(_, {cstr_name="false"}, _)}, loc) ->
      assert_failed loc ~scopes e
  | Texp_assert (cond, loc) ->
      if !Clflags.noassert
      then lambda_unit
      else Lifthenelse (transl_exp ~scopes cond, lambda_unit,
                        assert_failed loc ~scopes e)
  | Texp_lazy e ->
      (* when e needs no computation (constants, identifiers, ...), we
         optimize the translation just as Lazy.lazy_from_val would
         do *)
      begin match Typeopt.classify_lazy_argument e with
      | `Constant_or_function ->
        (* A constant expr (of type <> float if [Config.flat_float_array] is
           true) gets compiled as itself. *)
         transl_exp ~scopes e
      | `Float_that_cannot_be_shortcut ->
          (* We don't need to wrap with Popaque: this forward
             block will never be shortcutted since it points to a float
             and Config.flat_float_array is true. *)
          Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                [transl_exp ~scopes e], of_location ~scopes e.exp_loc)
      | `Identifier `Forward_value ->
         (* CR-someday mshinwell: Consider adding a new primitive
            that expresses the construction of forward_tag blocks.
            We need to use [Popaque] here to prevent unsound
            optimisation in Flambda, but the concept of a mutable
            block doesn't really match what is going on here.  This
            value may subsequently turn into an immediate... *)
         Lprim (Popaque,
                [Lprim(Pmakeblock(Obj.forward_tag, Immutable, None),
                       [transl_exp ~scopes e],
                       of_location ~scopes e.exp_loc)],
                of_location ~scopes e.exp_loc)
      | `Identifier `Other ->
         transl_exp ~scopes e
      | `Other ->
         (* other cases compile to a lazy block holding a function *)
         let fn = lfunction ~kind:Curried
                            ~params:[Ident.create_local "param", Pgenval]
                            ~return:Pgenval
                            (* The translation of [e] may be a function, in
                               which case disallowing arity fusion gives a very
                               small performance improvement.
                            *)
                            ~attr:function_attribute_disallowing_arity_fusion
                            ~loc:(of_location ~scopes e.exp_loc)
                            ~body:(transl_exp ~scopes e) in
          Lprim(Pmakeblock(Config.lazy_tag, Mutable, None), [fn],
                of_location ~scopes e.exp_loc)
      end
  | Texp_object (cs, meths) ->
      let cty = cs.cstr_type in
      let cl = Ident.create_local "object" in
      !transl_object ~scopes cl meths
        { cl_desc = Tcl_structure cs;
          cl_loc = e.exp_loc;
          cl_type = Cty_signature cty;
          cl_env = e.exp_env;
          cl_attributes = [];
         }
  | Texp_letop{let_; ands; param; body; partial} ->
      event_after ~scopes e
        (transl_letop ~scopes e.exp_loc e.exp_env let_ ands param body partial)
  | Texp_unreachable ->
      raise (Error (e.exp_loc, Unreachable_reached))
  | Texp_open (od, e) ->
      let pure = pure_module od.open_expr in
      (* this optimization shouldn't be needed because Simplif would
          actually remove the [Llet] when it's not used.
          But since [scan_used_globals] runs before Simplif, we need to
          do it. *)
      begin match od.open_bound_items with
      | [] when pure = Alias -> transl_exp ~scopes e
      | _ ->
          let oid = Ident.create_local "open" in
          let body, _ =
            List.fold_left (fun (body, pos) id ->
              Llet(Alias, Pgenval, id,
                   Lprim(Pfield (pos, Pointer, Mutable), [Lvar oid],
                         of_location ~scopes od.open_loc), body),
              pos + 1
            ) (transl_exp ~scopes e, 0)
              (bound_value_identifiers od.open_bound_items)
          in
          Llet(pure, Pgenval, oid,
               !transl_module ~scopes Tcoerce_none None od.open_expr, body)
      end

and pure_module m =
  match m.mod_desc with
    Tmod_ident _ -> Alias
  | Tmod_constraint (m,_,_,_) -> pure_module m
  | _ -> Strict

and transl_list ~scopes expr_list =
  List.map (transl_exp ~scopes) expr_list

and transl_list_with_shape ~scopes expr_list =
  let transl_with_shape e =
    let shape = Typeopt.value_kind e.exp_env e.exp_type in
    transl_exp ~scopes e, shape
  in
  List.split (List.map transl_with_shape expr_list)

and transl_guard ~scopes guard rhs =
  let expr = event_before ~scopes rhs (transl_exp ~scopes rhs) in
  match guard with
  | None -> expr
  | Some cond ->
      event_before ~scopes cond
        (Lifthenelse(transl_exp ~scopes cond, expr, staticfail))

and transl_case ~scopes {c_lhs; c_guard; c_rhs} =
  (c_lhs, transl_guard ~scopes c_guard c_rhs)

and transl_cases ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case ~scopes) cases

and transl_case_try ~scopes {c_lhs; c_guard; c_rhs} =
  iter_exn_names Translprim.add_exception_ident c_lhs;
  Misc.try_finally
    (fun () -> c_lhs, transl_guard ~scopes c_guard c_rhs)
    ~always:(fun () ->
        iter_exn_names Translprim.remove_exception_ident c_lhs)

and transl_cases_try ~scopes cases =
  let cases =
    List.filter (fun c -> c.c_rhs.exp_desc <> Texp_unreachable) cases in
  List.map (transl_case_try ~scopes) cases

and transl_tupled_cases ~scopes patl_expr_list =
  let patl_expr_list =
    List.filter (fun (_,_,e) -> e.exp_desc <> Texp_unreachable)
      patl_expr_list in
  List.map (fun (patl, guard, expr) -> (patl, transl_guard ~scopes guard expr))
    patl_expr_list

and transl_apply ~scopes
      ?(tailcall=Default_tailcall)
      ?(inlined = Default_inline)
      ?(specialised = Default_specialise)
      lam sargs loc
  =
  let lapply funct args =
    match funct with
      Lsend(k, lmet, lobj, largs, _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Levent(Lsend(k, lmet, lobj, largs, _), _) ->
        Lsend(k, lmet, lobj, largs @ args, loc)
    | Lapply ap ->
        Lapply {ap with ap_args = ap.ap_args @ args; ap_loc = loc}
    | lexp ->
        Lapply {
          ap_loc=loc;
          ap_func=lexp;
          ap_args=args;
          ap_tailcall=tailcall;
          ap_inlined=inlined;
          ap_specialised=specialised;
        }
  in
  (* Build a function application.
     Particular care is required for out-of-order partial applications.
     The following code guarantees that:
     * arguments are evaluated right-to-left according to their order in
       the type of the function, before the function is called;
     * side-effects occurring after receiving a non-optional parameter
       will occur exactly when all the arguments up to this parameter
       have been received;
     * side-effects occurring after receiving an optional parameter
       will occur at the latest when all the arguments up to the first
       non-optional parameter that follows it have been received.
  *)
  let rec build_apply lam args = function
      (None, optional) :: l ->
        (* Out-of-order partial application; we will need to build a closure *)
        let defs = ref [] in
        let protect name lam =
          match lam with
            Lvar _ | Lconst _ -> lam
          | _ ->
              let id = Ident.create_local name in
              defs := (id, lam) :: !defs;
              Lvar id
        in
        (* If all arguments in [args] were optional, delay their application
           until after this one is received *)
        let args, args' =
          if List.for_all (fun (_,opt) -> opt) args then [], args
          else args, []
        in
        let lam =
          if args = [] then lam else lapply lam (List.rev_map fst args)
        in
        (* Evaluate the function, applied to the arguments in [args] *)
        let handle = protect "func" lam in
        (* Evaluate the arguments whose applications was delayed;
           if we already passed here this is a no-op. *)
        let args' =
          List.map (fun (arg, opt) -> protect "arg" arg, opt) args'
        in
        (* Evaluate the remaining arguments;
           if we already passed here this is a no-op. *)
        let l =
          List.map (fun (arg, opt) -> Option.map (protect "arg") arg, opt) l
        in
        let id_arg = Ident.create_local "param" in
        (* Process remaining arguments and build closure *)
        let body =
          match build_apply handle ((Lvar id_arg, optional)::args') l with
            Lfunction{kind = Curried; params = ids; return; body; attr; loc}
            when List.length ids < Lambda.max_arity () ->
              lfunction ~kind:Curried ~params:((id_arg, Pgenval)::ids)
                        ~return ~body ~attr ~loc
          | body ->
              lfunction ~kind:Curried ~params:[id_arg, Pgenval]
                        ~return:Pgenval ~body
                        ~attr:default_stub_attribute ~loc
        in
        (* Wrap "protected" definitions, starting from the left,
           so that evaluation is right-to-left. *)
        List.fold_right
          (fun (id, lam) body -> Llet(Strict, Pgenval, id, lam, body))
          !defs body
    | (Some arg, optional) :: l ->
        build_apply lam ((arg, optional) :: args) l
    | [] ->
        lapply lam (List.rev_map fst args)
  in
  (build_apply lam [] (List.map (fun (l, x) ->
                                   Option.map (transl_exp ~scopes) x,
                                   Btype.is_optional l)
                                sargs)
     : Lambda.lambda)

(* There are two cases in function translation:
    - [Tupled]. It takes a tupled argument, and we can flatten it.
    - [Curried]. It takes each argument individually.

   We first try treating the function as taking a flattened tupled argument (in
   [trans_tupled_function]) and, if that doesn't work, we fall back to treating
   the function as taking each argument individually (in
   [trans_curried_function]).
*)
and transl_function_without_attributes ~scopes loc repr params body =
  let return =
    match body with
    | Tfunction_body body ->
        value_kind body.exp_env body.exp_type
    | Tfunction_cases { cases = { c_rhs } :: _ } ->
        value_kind c_rhs.exp_env c_rhs.exp_type
    | Tfunction_cases { cases = [] } ->
        (* With Camlp4/ppx, a pattern matching might be empty *)
        Pgenval
  in
  transl_tupled_function ~scopes loc return repr params body

and transl_tupled_function ~scopes loc return repr params body =
  (* Cases are eligible for flattening if they belong to the only param. *)
  let eligible_cases =
    match params, body with
    | [], Tfunction_cases { cases; partial } ->
        Some (cases, partial)
    | [ { fp_kind = Tparam_pat pat; fp_partial } ], Tfunction_body body ->
        let case = { c_lhs = pat; c_guard = None; c_rhs = body } in
        Some ([ case ], fp_partial)
    | _ -> None
  in
  match eligible_cases with
  | Some (({ c_lhs = { pat_desc = Tpat_tuple pl } } :: _) as cases, partial)
    when !Clflags.native_code
      && List.length pl <= (Lambda.max_arity ()) ->
      begin try
        let size = List.length pl in
        let pats_expr_list =
          List.map
            (fun {c_lhs; c_guard; c_rhs} ->
              (Matching.flatten_pattern size c_lhs, c_guard, c_rhs))
            cases in
        let kinds =
          (* All the patterns might not share the same types. We must take the
             union of the patterns types *)
          match pats_expr_list with
          | [] -> assert false
          | (pats, _, _) :: cases ->
              let first_case_kinds =
                List.map (fun pat -> value_kind pat.pat_env pat.pat_type) pats
              in
              List.fold_left
                (fun kinds (pats, _, _) ->
                  List.map2 (fun kind pat ->
                    value_kind_union kind
                      (value_kind pat.pat_env pat.pat_type))
                    kinds pats)
                first_case_kinds cases
        in
        let tparams =
          List.map (fun kind -> Ident.create_local "param", kind) kinds
        in
        let params = List.map fst tparams in
        ((Tupled, tparams, return),
         Matching.for_tupled_function ~scopes loc params
           (transl_tupled_cases ~scopes pats_expr_list) partial)
    with Matching.Cannot_flatten ->
      transl_curried_function ~scopes loc return repr params body
      end
  | _ -> transl_curried_function ~scopes loc return repr params body

and transl_curried_function ~scopes loc return repr params body =
  let cases_param, body =
    match body with
    | Tfunction_body body ->
        None, event_before ~scopes body (transl_exp ~scopes body)
    | Tfunction_cases { cases; partial; param; loc = cases_loc } ->
        let kind =
          match cases with
          | [] ->
            (* With Camlp4/ppx, a pattern matching might be empty *)
            Pgenval
          | {c_lhs=pat} :: other_cases ->
            (* All the patterns might not share the same types. We must take the
              union of the patterns types *)
            List.fold_left (fun k {c_lhs=pat} ->
              Typeopt.value_kind_union k
                (value_kind pat.pat_env pat.pat_type))
              (value_kind pat.pat_env pat.pat_type) other_cases
        in
        let body =
          Matching.for_function ~scopes cases_loc repr (Lvar param)
            (transl_cases ~scopes cases) partial
        in
        Some (param, kind), body
  in
  let body, params =
    List.fold_right (fun fp (body, params) ->
      let param = fp.fp_param in
      let param_loc = fp.fp_loc in
      match fp.fp_kind with
      | Tparam_pat pat ->
          let kind = value_kind pat.pat_env pat.pat_type in
          let body =
            Matching.for_function ~scopes param_loc None (Lvar param)
              [ pat, body ]
              fp.fp_partial
          in
          body, (param, kind) :: params
      | Tparam_optional_default (pat, default_arg) ->
          let default_arg =
            event_before ~scopes default_arg (transl_exp ~scopes default_arg)
          in
          let body =
            Matching.for_optional_arg_default
              ~scopes param_loc pat body ~default_arg ~param
          in
          (* The optional param is Pgenval as it's an option. *)
          body, (param, Pgenval) :: params)
    params
    (body, Option.to_list cases_param)
  in
  (* chunk params according to Lambda.max_arity. If Lambda.max_arity = n and
     N>n, then the translation of an N-ary typedtree function is an n-ary lambda
     function returning the translation of an (N-n)-ary typedtree function.
  *)
  let params, return, body =
    match Misc.Stdlib.List.chunks_of (Lambda.max_arity ()) params with
    | [] ->
        Misc.fatal_error "attempted to translate a function with zero arguments"
    | first_chunk :: rest_of_chunks ->
      let body, return =
        List.fold_right
          (fun chunk (body, return) ->
            let attr = function_attribute_disallowing_arity_fusion in
            let loc = of_location ~scopes loc in
            let body =
              lfunction ~kind:Curried ~params:chunk ~return ~body ~attr ~loc
            in
            (* we return Pgenval (for a function) after the rightmost chunk. *)
            body, Pgenval)
          rest_of_chunks
          (body, return)
      in
      first_chunk, return, body
  in
  ((Curried, params, return), body)

and transl_function ~scopes e params body =
  let ((kind, params, return), body) =
    event_function ~scopes e
      (function repr ->
         let params, body = fuse_method_arity params body in
         transl_function_without_attributes ~scopes e.exp_loc repr params body)
  in
  let attr = function_attribute_disallowing_arity_fusion in
  let loc = of_location ~scopes e.exp_loc in
  let lam = lfunction ~kind ~params ~return ~body ~attr ~loc in
  let attrs =
    (* Collect attributes from the Pexp_newtype node for locally abstract types.
       Otherwise we'd ignore the attribute in, e.g.:
           fun [@inline] (type a) x -> ...
    *)
    List.fold_left
      (fun attrs (extra_exp, _, extra_attrs) ->
         match extra_exp with
         | Texp_newtype _ -> extra_attrs @ attrs
         | (Texp_constraint _ | Texp_coerce _ | Texp_poly _) -> attrs)
      e.exp_attributes e.exp_extra
  in
  Translattribute.add_function_attributes lam e.exp_loc attrs

(* Like transl_exp, but used when a new scope was just introduced. *)
and transl_scoped_exp ~scopes expr =
  transl_exp1 ~scopes ~in_new_scope:true expr

(* Decides whether a pattern binding should introduce a new scope. *)
and transl_bound_exp ~scopes ~in_structure pat expr =
  let should_introduce_scope =
    match expr.exp_desc with
    | Texp_function _ -> true
    | _ when in_structure -> true
    | _ -> false in
  match pat_bound_idents pat with
  | (id :: _) when should_introduce_scope ->
     transl_scoped_exp ~scopes:(enter_value_definition ~scopes id) expr
  | _ -> transl_exp ~scopes expr

(*
  Notice: transl_let consumes (ie compiles) its pat_expr_list argument,
  and returns a function that will take the body of the lambda-let construct.
  This complication allows choosing any compilation order for the
  bindings and body of let constructs.
*)
and transl_let ~scopes ?(in_structure=false) rec_flag pat_expr_list =
  match rec_flag with
    Nonrecursive ->
      let rec transl = function
        [] ->
          fun body -> body
      | {vb_pat=pat; vb_expr=expr; vb_rec_kind=_; vb_attributes=attr; vb_loc}
        :: rem ->
          let lam = transl_bound_exp ~scopes ~in_structure pat expr in
          let lam = Translattribute.add_function_attributes lam vb_loc attr in
          let mk_body = transl rem in
          fun body ->
            Matching.for_let ~scopes pat.pat_loc lam pat (mk_body body)
      in transl pat_expr_list
  | Recursive ->
      let idlist =
        List.map
          (fun {vb_pat=pat} -> match pat.pat_desc with
              Tpat_var (id,_,_) -> id
            | Tpat_alias ({pat_desc=Tpat_any}, id,_,_) -> id
            | _ -> assert false)
        pat_expr_list in
      let transl_case {vb_expr=expr; vb_attributes; vb_rec_kind = rkind;
                       vb_loc; vb_pat} id =
        let def = transl_bound_exp ~scopes ~in_structure vb_pat expr in
        let def =
          Translattribute.add_function_attributes def vb_loc vb_attributes
        in
        ( id, rkind, def ) in
      let lam_bds = List.map2 transl_case pat_expr_list idlist in
      fun body -> Value_rec_compiler.compile_letrec lam_bds body

and transl_setinstvar ~scopes loc self var expr =
  Lprim(Psetfield_computed (maybe_pointer expr, Assignment),
    [self; var; transl_exp ~scopes expr], loc)

and transl_record ~scopes loc env fields repres opt_init_expr =
  let size = Array.length fields in
  (* Determine if there are "enough" fields (only relevant if this is a
     functional-style record update *)
  let no_init = match opt_init_expr with None -> true | _ -> false in
  if no_init || size < Config.max_young_wosize
  then begin
    (* Allocate new record with given fields (and remaining fields
       taken from init_expr if any *)
    let init_id = Ident.create_local "init" in
    let lv =
      Array.mapi
        (fun i (_, definition) ->
           match definition with
           | Kept (typ, mut) ->
               let field_kind = value_kind env typ in
               let access =
                 match repres with
                   Record_regular | Record_inlined _ ->
                     Pfield (i, maybe_pointer_type env typ, mut)
                 | Record_unboxed _ -> assert false
                 | Record_extension _ ->
                     Pfield (i + 1, maybe_pointer_type env typ, mut)
                 | Record_float -> Pfloatfield i in
               Lprim(access, [Lvar init_id],
                     of_location ~scopes loc),
               field_kind
           | Overridden (_lid, expr) ->
               let field_kind = value_kind expr.exp_env expr.exp_type in
               transl_exp ~scopes expr, field_kind)
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
        | Record_extension _ ->
            raise Not_constant
      with Not_constant ->
        let loc = of_location ~scopes loc in
        match repres with
          Record_regular ->
            Lprim(Pmakeblock(0, mut, Some shape), ll, loc)
        | Record_inlined tag ->
            Lprim(Pmakeblock(tag, mut, Some shape), ll, loc)
        | Record_unboxed _ -> (match ll with [v] -> v | _ -> assert false)
        | Record_float ->
            Lprim(Pmakearray (Pfloatarray, mut), ll, loc)
        | Record_extension path ->
            let slot = transl_extension_path loc env path in
            Lprim(Pmakeblock(0, mut, Some (Pgenval :: shape)), slot :: ll, loc)
    in
    begin match opt_init_expr with
      None -> lam
    | Some init_expr -> Llet(Strict, Pgenval, init_id,
                             transl_exp ~scopes init_expr, lam)
    end
  end else begin
    (* Take a shallow copy of the init record, then mutate the fields
       of the copy *)
    let copy_id = Ident.create_local "newrecord" in
    let update_field cont (lbl, definition) =
      match definition with
      | Kept _ -> cont
      | Overridden (_lid, expr) ->
          let upd =
            match repres with
              Record_regular
            | Record_inlined _ ->
                Psetfield(lbl.lbl_pos, maybe_pointer expr, Assignment)
            | Record_unboxed _ -> assert false
            | Record_float -> Psetfloatfield (lbl.lbl_pos, Assignment)
            | Record_extension _ ->
                Psetfield(lbl.lbl_pos + 1, maybe_pointer expr, Assignment)
          in
          Lsequence(Lprim(upd, [Lvar copy_id; transl_exp ~scopes expr],
                          of_location ~scopes loc),
                    cont)
    in
    begin match opt_init_expr with
      None -> assert false
    | Some init_expr ->
        Llet(Strict, Pgenval, copy_id,
             Lprim(Pduprecord (repres, size), [transl_exp ~scopes init_expr],
                   of_location ~scopes loc),
             Array.fold_left update_field (Lvar copy_id) fields)
    end
  end

and transl_match ~scopes e arg pat_expr_list partial =
  let rewrite_case (val_cases, exn_cases, static_handlers as acc)
        ({ c_lhs; c_guard; c_rhs } as case) =
    if c_rhs.exp_desc = Texp_unreachable then acc else
    let val_pat, exn_pat = split_pattern c_lhs in
    match val_pat, exn_pat with
    | None, None -> assert false
    | Some pv, None ->
        let val_case =
          transl_case ~scopes { case with c_lhs = pv }
        in
        val_case :: val_cases, exn_cases, static_handlers
    | None, Some pe ->
        let exn_case = transl_case_try ~scopes { case with c_lhs = pe } in
        val_cases, exn_case :: exn_cases, static_handlers
    | Some pv, Some pe ->
        assert (c_guard = None);
        let lbl  = next_raise_count () in
        let static_raise ids =
          Lstaticraise (lbl, List.map (fun id -> Lvar id) ids)
        in
        (* Simplif doesn't like it if binders are not uniq, so we make sure to
           use different names in the value and the exception branches. *)
        let ids_full = Typedtree.pat_bound_idents_full pv in
        let ids = List.map (fun (id, _, _, _) -> id) ids_full in
        let ids_kinds =
          List.map (fun (id, _, ty, _) -> id, Typeopt.value_kind pv.pat_env ty)
            ids_full
        in
        let vids = List.map Ident.rename ids in
        let pv = alpha_pat (List.combine ids vids) pv in
        (* Also register the names of the exception so Re-raise happens. *)
        iter_exn_names Translprim.add_exception_ident pe;
        let rhs =
          Misc.try_finally
            (fun () -> event_before ~scopes c_rhs
                         (transl_exp ~scopes c_rhs))
            ~always:(fun () ->
                iter_exn_names Translprim.remove_exception_ident pe)
        in
        (pv, static_raise vids) :: val_cases,
        (pe, static_raise ids) :: exn_cases,
        (lbl, ids_kinds, rhs) :: static_handlers
  in
  let val_cases, exn_cases, static_handlers =
    let x, y, z = List.fold_left rewrite_case ([], [], []) pat_expr_list in
    List.rev x, List.rev y, List.rev z
  in
  (* In presence of exception patterns, the code we generate for

       match <scrutinees> with
       | <val-patterns> -> <val-actions>
       | <exn-patterns> -> <exn-actions>

     looks like

       staticcatch
         (try (exit <val-exit> <scrutinees>)
          with <exn-patterns> -> <exn-actions>)
       with <val-exit> <val-ids> ->
          match <val-ids> with <val-patterns> -> <val-actions>

     In particular, the 'exit' in the value case ensures that the
     value actions run outside the try..with exception handler.
  *)
  let static_catch scrutinees val_ids handler =
    let id = Typecore.name_pattern "exn" (List.map fst exn_cases) in
    let static_exception_id = next_raise_count () in
    Lstaticcatch
      (Ltrywith (Lstaticraise (static_exception_id, scrutinees), id,
                 Matching.for_trywith ~scopes e.exp_loc (Lvar id) exn_cases),
       (static_exception_id, val_ids),
       handler)
  in
  let classic =
    match arg, exn_cases with
    | {exp_desc = Texp_tuple argl}, [] ->
      assert (static_handlers = []);
      Matching.for_multiple_match ~scopes e.exp_loc
        (transl_list ~scopes argl) val_cases partial
    | {exp_desc = Texp_tuple argl}, _ :: _ ->
        let val_ids =
          List.map
            (fun arg ->
               Typecore.name_pattern "val" [],
               Typeopt.value_kind arg.exp_env arg.exp_type
            )
            argl
        in
        let lvars = List.map (fun (id, _) -> Lvar id) val_ids in
        static_catch (transl_list ~scopes argl) val_ids
          (Matching.for_multiple_match ~scopes e.exp_loc
             lvars val_cases partial)
    | arg, [] ->
      assert (static_handlers = []);
      Matching.for_function ~scopes e.exp_loc
        None (transl_exp ~scopes arg) val_cases partial
    | arg, _ :: _ ->
        let val_id = Typecore.name_pattern "val" (List.map fst val_cases) in
        let k = Typeopt.value_kind arg.exp_env arg.exp_type in
        static_catch [transl_exp ~scopes arg] [val_id, k]
          (Matching.for_function ~scopes e.exp_loc
             None (Lvar val_id) val_cases partial)
  in
  List.fold_left (fun body (static_exception_id, val_ids, handler) ->
    Lstaticcatch (body, (static_exception_id, val_ids), handler)
  ) classic static_handlers

and transl_letop ~scopes loc env let_ ands param case partial =
  let rec loop prev_lam = function
    | [] -> prev_lam
    | and_ :: rest ->
        let left_id = Ident.create_local "left" in
        let right_id = Ident.create_local "right" in
        let op =
          transl_ident (of_location ~scopes and_.bop_op_name.loc) env
            and_.bop_op_type and_.bop_op_path and_.bop_op_val
        in
        let exp = transl_exp ~scopes and_.bop_exp in
        let lam =
          bind Strict right_id exp
            (Lapply{
               ap_loc = of_location ~scopes and_.bop_loc;
               ap_func = op;
               ap_args=[Lvar left_id; Lvar right_id];
               ap_tailcall = Default_tailcall;
               ap_inlined = Default_inline;
               ap_specialised = Default_specialise;
             })
        in
        bind Strict left_id prev_lam (loop lam rest)
  in
  let op =
    transl_ident (of_location ~scopes let_.bop_op_name.loc) env
      let_.bop_op_type let_.bop_op_path let_.bop_op_val
  in
  let exp = loop (transl_exp ~scopes let_.bop_exp) ands in
  let func =
    let (kind, params, return), body =
      event_function ~scopes case.c_rhs
        (function repr ->
           let loc = case.c_rhs.exp_loc in
           let ghost_loc = { loc with loc_ghost = true } in
           transl_function_without_attributes ~scopes loc repr []
             (Tfunction_cases
                { cases = [case]; param; partial; loc = ghost_loc;
                  exp_extra = None; attributes = []; }))
    in
    let attr = function_attribute_disallowing_arity_fusion in
    let loc = of_location ~scopes case.c_rhs.exp_loc in
    lfunction ~kind ~params ~return ~body ~attr ~loc
  in
  Lapply{
    ap_loc = of_location ~scopes loc;
    ap_func = op;
    ap_args=[exp; func];
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inline;
    ap_specialised = Default_specialise;
  }

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
          Some (Location.error_of_printer ~loc report_error err)
      | _ ->
        None
    )
