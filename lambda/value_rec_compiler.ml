(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Lambda

(* Simple blocks *)
type block_size =
  | Regular_block of int
  | Float_record of int

type size =
  | Unreachable
  | Constant
  | Function
  | Block of block_size

type binding_size = (lambda_with_env, size) Lazy_backtrack.t
and lambda_with_env = {
  lambda : lambda;
  env : binding_size Ident.Map.t;
}

let dynamic_size () =
  Misc.fatal_error "letrec: No size found for Static binding"

let join_sizes size1 size2 =
  match size1, size2 with
  | Unreachable, size | size, Unreachable -> size
  | _, _ -> dynamic_size ()

let compute_static_size lam =
  let rec compute_expression_size env lam =
    match lam with
    | Lvar v ->
      begin match Ident.Map.find_opt v env with
      | None ->
        dynamic_size ()
      | Some binding_size ->
        Lazy_backtrack.force
          (fun { lambda; env } -> compute_expression_size env lambda)
          binding_size
      end
    | Lmutvar _ -> dynamic_size ()
    | Lconst _ -> Constant
    | Lapply _ -> dynamic_size ()
    | Lfunction _ -> Function
    | Llet (_, _, id, def, body) ->
      let env =
        Ident.Map.add id (Lazy_backtrack.create { lambda = def; env }) env
      in
      compute_expression_size env body
    | Lmutlet(_, _, _, body) ->
      compute_expression_size env body
    | Lletrec (bindings, body) ->
      let env =
        List.fold_left (fun env_acc { id; def = _ } ->
            Ident.Map.add id (Lazy_backtrack.create_forced Function) env_acc)
          env bindings
      in
      compute_expression_size env body
    | Lprim (p, args, _) ->
      size_of_primitive env p args
    | Lswitch (_, sw, _) ->
      let fail_case =
        match sw.sw_failaction with
        | None -> []
        | Some fail -> [0 (* ignored *), fail]
      in
      compute_and_join_sizes_switch env [sw.sw_consts; sw.sw_blocks; fail_case]
    | Lstringswitch (_, cases, fail, _) ->
      let fail_case =
        match fail with
        | None -> []
        | Some fail -> ["" (* ignored *), fail]
      in
      compute_and_join_sizes_switch env [cases; fail_case]
    | Lstaticraise _ -> Unreachable
    | Lstaticcatch (body, _, handler)
    | Ltrywith (body, _, handler) ->
      compute_and_join_sizes env [body; handler]
    | Lifthenelse (_cond, ifso, ifnot) ->
      compute_and_join_sizes env [ifso; ifnot]
    | Lsequence (_, e) ->
      compute_expression_size env e
    | Lwhile _
    | Lfor _
    | Lassign _ -> Constant
    | Lsend _ -> dynamic_size ()
    | Levent (e, _) ->
      compute_expression_size env e
    | Lifused _ -> Constant
  and compute_and_join_sizes env branches =
    List.fold_left (fun size branch ->
        join_sizes size (compute_expression_size env branch))
      Unreachable branches
  and compute_and_join_sizes_switch :
    type a. binding_size Ident.Map.t -> (a * lambda) list list -> size =
    fun env all_cases ->
      List.fold_left (fun size cases ->
          List.fold_left (fun size (_key, action) ->
              join_sizes size (compute_expression_size env action))
            size cases)
        Unreachable all_cases
  and size_of_primitive env p args =
    match p with
    | Pignore
    | Psetfield _
    | Psetfield_computed _
    | Psetfloatfield _
    | Poffsetint _
    | Poffsetref _
    | Pbytessetu
    | Pbytessets
    | Parraysetu _
    | Parraysets _
    | Pbigarrayset _
    | Pbytes_set_16 _
    | Pbytes_set_32 _
    | Pbytes_set_64 _
    | Pbigstring_set_16 _
    | Pbigstring_set_32 _
    | Pbigstring_set_64 _ ->
        (* Unit-returning primitives. Most of these are only generated from
           external declarations and not special-cased by [Value_rec_check],
           but it doesn't hurt to be consistent. *)
        Constant

    | Pduprecord (repres, size) ->
        begin match repres with
        | Record_regular | Record_inlined _ | Record_extension _ ->
            Block (Regular_block size)
        | Record_float ->
            Block (Float_record size)
        | Record_unboxed _ ->
            Misc.fatal_error "size_of_primitive"
        end
    | Pmakeblock _ ->
        (* The block shape is unfortunately an option, so we rely on the
           number of arguments instead.
           Note that flat float arrays/records use Pmakearray, so we don't need
           to check the tag here. *)
        Block (Regular_block (List.length args))
    | Pmakearray (kind, _) ->
        let size = List.length args in
        begin match kind with
        | Pgenarray | Paddrarray | Pintarray ->
            Block (Regular_block size)
        | Pfloatarray ->
            Block (Float_record size)
        end
    | Pduparray _ ->
        (* The size has to be recovered from the size of the argument *)
        begin match args with
        | [arg] ->
            compute_expression_size env arg
        | [] | _ :: _ :: _ ->
            Misc.fatal_error "size_of_primitive"
        end

    | Praise _ ->
        Unreachable

    | Pctconst _ ->
        (* These primitives are not special-cased by [Value_rec_check],
           so we should never end up here; but these are constants anyway *)
        Constant

    | Pbytes_to_string
    | Pbytes_of_string
    | Pgetglobal _
    | Psetglobal _
    | Pfield _
    | Pfield_computed
    | Pfloatfield _
    | Prunstack
    | Pperform
    | Presume
    | Preperform
    | Pccall _
    | Psequand | Psequor | Pnot
    | Pnegint | Paddint | Psubint | Pmulint
    | Pdivint _ | Pmodint _
    | Pandint | Porint | Pxorint
    | Plslint | Plsrint | Pasrint
    | Pintcomp _
    | Pcompare_ints | Pcompare_floats | Pcompare_bints _
    | Pintoffloat | Pfloatofint
    | Pnegfloat | Pabsfloat
    | Paddfloat | Psubfloat | Pmulfloat | Pdivfloat
    | Pfloatcomp _
    | Pstringlength | Pstringrefu  | Pstringrefs
    | Pbyteslength | Pbytesrefu | Pbytesrefs
    | Parraylength _
    | Parrayrefu _
    | Parrayrefs _
    | Pisint
    | Pisout
    | Pbintofint _
    | Pintofbint _
    | Pcvtbint _
    | Pnegbint _
    | Paddbint _
    | Psubbint _
    | Pmulbint _
    | Pdivbint _
    | Pmodbint _
    | Pandbint _
    | Porbint _
    | Pxorbint _
    | Plslbint _
    | Plsrbint _
    | Pasrbint _
    | Pbintcomp _
    | Pbigarrayref _
    | Pbigarraydim _
    | Pstring_load_16 _
    | Pstring_load_32 _
    | Pstring_load_64 _
    | Pbytes_load_16 _
    | Pbytes_load_32 _
    | Pbytes_load_64 _
    | Pbigstring_load_16 _
    | Pbigstring_load_32 _
    | Pbigstring_load_64 _
    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer
    | Patomic_load _
    | Patomic_exchange
    | Patomic_cas
    | Patomic_fetch_add
    | Popaque
    | Pdls_get ->
        dynamic_size ()
  in
  compute_expression_size Ident.Map.empty lam

let lfunction_with_body { kind; params; return; body = _; attr; loc } body =
  lfunction' ~kind ~params ~return ~body ~attr ~loc

(* The backend doesn't handle mutable empty block correctly *)
let lifted_block_mut : Asttypes.mutable_flag = Immutable

let no_loc = Debuginfo.Scoped_location.Loc_unknown

type lifted_function =
  { lfun : Lambda.lfunction;
    free_vars_block_size : int;
  }

type 'a split_result =
  | Unreachable
  | Reachable of lifted_function * 'a

let ( let** ) res f =
  match res with
  | Unreachable -> Unreachable
  | Reachable (func, lam) -> Reachable (func, f lam)

let rec split_static_function block_var local_idents lam :
  Lambda.lambda split_result =
  match lam with
  | Lvar v ->
    (* Eta-expand *)
    (* Note: knowing the arity might let us generate slightly better code *)
    let param = Ident.create_local "let_rec_param" in
    let ap_func =
      Lprim (Pfield (0, Pointer, lifted_block_mut), [Lvar block_var], no_loc)
    in
    let body =
      Lapply {
        ap_func;
        ap_args = [Lvar param];
        ap_loc = no_loc;
        ap_tailcall = Default_tailcall;
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
    in
    let wrapper =
      lfunction'
        ~kind:Curried
        ~params:[param, Pgenval]
        ~return:Pgenval
        ~body
        ~attr:default_stub_attribute
        ~loc:no_loc
    in
    let lifted = { lfun = wrapper; free_vars_block_size = 1 } in
    Reachable (lifted,
               Lprim (Pmakeblock (0, lifted_block_mut, None), [Lvar v], no_loc))
  | Lfunction lfun ->
    let free_vars = Lambda.free_variables lfun.body in
    let local_free_vars = Ident.Set.inter free_vars local_idents in
    let free_vars_block_size, subst, block_fields_rev =
      Ident.Set.fold (fun var (i, subst, fields) ->
          let access =
            Lprim (Pfield (i, Pointer, lifted_block_mut),
                   [Lvar block_var],
                   no_loc)
          in
          (succ i, Ident.Map.add var access subst, Lvar var :: fields))
        local_free_vars (0, Ident.Map.empty, [])
    in
    let new_fun =
      lfunction_with_body lfun
        (Lambda.subst (fun _ _ env -> env) subst lfun.body)
    in
    let lifted = { lfun = new_fun; free_vars_block_size } in
    let block =
      Lprim (Pmakeblock (0, lifted_block_mut, None),
             List.rev block_fields_rev,
             no_loc)
    in
    Reachable (lifted, block)
  | Llet (lkind, vkind, var, def, body) ->
    let** body =
      split_static_function block_var (Ident.Set.add var local_idents) body
    in
    Llet (lkind, vkind, var, def, body)
  | Lmutlet (vkind, var, def, body) ->
    let** body =
      split_static_function block_var (Ident.Set.add var local_idents) body
    in
    Lmutlet (vkind, var, def, body)
  | Lletrec (bindings, body) ->
    let local_idents =
      List.fold_left (fun ids { id } -> Ident.Set.add id ids)
        local_idents bindings
    in
    let** body =
      split_static_function block_var local_idents body
    in
    Lletrec (bindings, body)
  | Lprim (Praise _, _, _) -> Unreachable
  | Lstaticraise _ -> Unreachable
  | Lswitch (arg, sw, loc) ->
    let sw_consts_res = rebuild_arms block_var local_idents sw.sw_consts in
    let sw_blocks_res = rebuild_arms block_var local_idents sw.sw_blocks in
    let sw_failaction_res =
      Option.map (split_static_function block_var local_idents) sw.sw_failaction
    in
    begin match sw_consts_res, sw_blocks_res, sw_failaction_res with
    | Unreachable, Unreachable, (None | Some Unreachable) -> Unreachable
    | Reachable (lfun, sw_consts), Unreachable, (None | Some Unreachable) ->
      Reachable (lfun, Lswitch (arg, { sw with sw_consts }, loc))
    | Unreachable, Reachable (lfun, sw_blocks), (None | Some Unreachable) ->
      Reachable (lfun, Lswitch (arg, { sw with sw_blocks }, loc))
    | Unreachable, Unreachable, Some (Reachable (lfun, failaction)) ->
      let switch =
        Lswitch (arg, { sw with sw_failaction = Some failaction }, loc)
      in
      Reachable (lfun, switch)
    | Reachable _, Reachable _, _ | Reachable _, _, Some (Reachable _)
    | _, Reachable _, Some (Reachable _) ->
      Misc.fatal_error "letrec: multiple functions"
    end
  | Lstringswitch (arg, arms, failaction, loc) ->
    let arms_res = rebuild_arms block_var local_idents arms in
    let failaction_res =
      Option.map (split_static_function block_var local_idents) failaction
    in
    begin match arms_res, failaction_res with
    | Unreachable, (None | Some Unreachable) -> Unreachable
    | Reachable (lfun, arms), (None | Some Unreachable) ->
      Reachable (lfun, Lstringswitch (arg, arms, failaction, loc))
    | Unreachable, Some (Reachable (lfun, failaction)) ->
      Reachable (lfun, Lstringswitch (arg, arms, Some failaction, loc))
    | Reachable _, Some (Reachable _) ->
      Misc.fatal_error "letrec: multiple functions"
    end
  | Lstaticcatch (body, (nfail, params), handler) ->
    let body_res = split_static_function block_var local_idents body in
    let handler_res =
      let local_idents =
        List.fold_left (fun vars (var, _) -> Ident.Set.add var vars)
          local_idents params
      in
      split_static_function block_var local_idents handler
    in
    begin match body_res, handler_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, body), Unreachable ->
      Reachable (lfun, Lstaticcatch (body, (nfail, params), handler))
    | Unreachable, Reachable (lfun, handler) ->
      Reachable (lfun, Lstaticcatch (body, (nfail, params), handler))
    | Reachable _, Reachable _ ->
      Misc.fatal_error "letrec: multiple functions"
    end
  | Ltrywith (body, exn_var, handler) ->
    let body_res = split_static_function block_var local_idents body in
    let handler_res =
      split_static_function block_var
        (Ident.Set.add exn_var local_idents) handler
    in
    begin match body_res, handler_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, body), Unreachable ->
      Reachable (lfun, Ltrywith (body, exn_var, handler))
    | Unreachable, Reachable (lfun, handler) ->
      Reachable (lfun, Ltrywith (body, exn_var, handler))
    | Reachable _, Reachable _ ->
      Misc.fatal_error "letrec: multiple functions"
    end
  | Lifthenelse (cond, ifso, ifnot) ->
    let ifso_res = split_static_function block_var local_idents ifso in
    let ifnot_res = split_static_function block_var local_idents ifnot in
    begin match ifso_res, ifnot_res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, ifso), Unreachable ->
      Reachable (lfun, Lifthenelse (cond, ifso, ifnot))
    | Unreachable, Reachable (lfun, ifnot) ->
      Reachable (lfun, Lifthenelse (cond, ifso, ifnot))
    | Reachable _, Reachable _ ->
      Misc.fatal_error "letrec: multiple functions"
    end
  | Lsequence (e1, e2) ->
    let** e2 = split_static_function block_var local_idents e2 in
    Lsequence (e1, e2)
  | Levent (lam, lev) ->
    let** lam = split_static_function block_var local_idents lam in
    Levent (lam, lev)
  | Lmutvar _
  | Lconst _
  | Lapply _
  | Lprim _
  | Lwhile _
  | Lfor _
  | Lassign _
  | Lsend _
  | Lifused _ -> Misc.fatal_error "letrec binding is not a static function"
and rebuild_arms :
  type a. _ -> _ -> (a * Lambda.lambda) list ->
  (a * Lambda.lambda) list split_result =
  fun block_var local_idents arms ->
  match arms with
  | [] -> Unreachable
  | (i, lam) :: arms ->
    let res = rebuild_arms block_var local_idents arms in
    let lam_res = split_static_function block_var local_idents lam in
    match lam_res, res with
    | Unreachable, Unreachable -> Unreachable
    | Reachable (lfun, lam), Unreachable ->
      Reachable (lfun, (i, lam) :: arms)
    | Unreachable, Reachable (lfun, arms) ->
      Reachable (lfun, (i, lam) :: arms)
    | Reachable _, Reachable _ ->
      Misc.fatal_error "letrec: multiple functions"

type rec_bindings =
  { static : (Ident.t * block_size * Lambda.lambda) list;
    functions : (Ident.t * Lambda.lfunction) list;
    dynamic : (Ident.t * Lambda.lambda) list;
  }

let empty_bindings =
  { static = [];
    functions = [];
    dynamic = [];
  }

let compile_letrec input_bindings body =
  let alloc_prim =
    Primitive.simple ~name:"caml_alloc_dummy" ~arity:1 ~alloc:true
  in
  let alloc_float_record_prim =
    Primitive.simple ~name:"caml_alloc_dummy_float" ~arity:1 ~alloc:true
  in
  let update_prim =
    (* Note: [alloc] could be false, but who cares *)
    Primitive.simple ~name:"caml_update_dummy" ~arity:2 ~alloc:true
  in
  let dummy_lambda = Lambda.Lconst (Lambda.const_int (0xbadbad / 2)) in
  let subst_for_constants =
    List.fold_left (fun subst (id, _, _) ->
        Ident.Map.add id dummy_lambda subst)
      Ident.Map.empty input_bindings
  in
  let all_bindings_rev =
    List.fold_left (fun rev_bindings (id, rkind, def) ->
        match (rkind : Value_rec_types.recursive_binding_kind) with
        | Dynamic ->
          { rev_bindings with dynamic = (id, def) :: rev_bindings.dynamic }
        | Static ->
          let size = compute_static_size def in
          begin match size with
          | Constant | Unreachable ->
            (* The result never escapes any recursive variables, so as we know
               it doesn't inspect them either we can just bind the recursive
               variables to dummy values and evaluate the definition normally.
            *)
            let def =
              Lambda.subst (fun _ _ env -> env) subst_for_constants def
            in
            { rev_bindings with dynamic = (id, def) :: rev_bindings.dynamic }
          | Block size ->
            { rev_bindings with
              static = (id, size, def) :: rev_bindings.static }
          | Function ->
            begin match def with
            | Lfunction lfun ->
              { rev_bindings with
                functions = (id, lfun) :: rev_bindings.functions
              }
            | _ ->
              let block_var = Ident.create_local "letrec_lifted_block" in
              begin match split_static_function block_var Ident.Set.empty def with
              | Unreachable -> Misc.fatal_error "letrec: no function for binding"
              | Reachable ({ lfun; free_vars_block_size }, lam) ->
                let functions = (id, lfun) :: rev_bindings.functions in
                let static =
                  (block_var, Regular_block free_vars_block_size, lam) ::
                  rev_bindings.static
                in
                { rev_bindings with functions; static }
              end
            end
          end)
      empty_bindings input_bindings
  in
  let body_with_patches =
    List.fold_left (fun body (id, _size, lam) ->
        let update =
          Lprim (Pccall update_prim, [Lvar id; lam], no_loc)
        in
        Lsequence (update, body))
      body (all_bindings_rev.static)
  in
  let body_with_functions =
    match all_bindings_rev.functions with
    | [] -> body_with_patches
    | bindings_rev ->
      let function_bindings =
        List.rev_map (fun (id, lfun) ->
            { id; def = lfun })
          bindings_rev
      in
      Lletrec (function_bindings, body_with_patches)
  in
  let body_with_dynamic_values =
    List.fold_left (fun body (id, lam) ->
        Llet(Strict, Pgenval, id, lam, body))
      body_with_functions all_bindings_rev.dynamic
  in
  let body_with_pre_allocations =
    List.fold_left (fun body (id, size, _lam) ->
        let alloc_prim, size =
          match size with
          | Regular_block size -> alloc_prim, size
          | Float_record size -> alloc_float_record_prim, size
        in
        let alloc =
          Lprim (Pccall alloc_prim,
                 [Lconst (Lambda.const_int size)],
                 no_loc)
        in
        Llet(Strict, Pgenval, id, alloc, body))
      body_with_dynamic_values all_bindings_rev.static
  in
  body_with_pre_allocations
