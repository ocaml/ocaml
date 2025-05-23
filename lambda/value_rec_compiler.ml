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

(** Compilation of generic recursive definitions *)

(** The surface language allows a wide range of recursive definitions, but
    Lambda only allows syntactic functions in recursive bindings.
    This file implements the translation from generic definitions to Lambda.

    The first step occurs during typechecking, in [Value_rec_check]:
    [Dynamic] bindings need to be compiled as normal let bindings. This file
    mostly deals with the [Static] bindings.

    The three phases in this module are the following:

    - Sizing: we first classify the definitions by their size, which determines
      the compilation strategy for each binding.

    - Function lifting: we then apply a transformation from general function
      definitions to syntactic functions accepted by [Lletrec].
      Examples:
      {[
        let rec f x = f x (* Syntactic *)
        let rec f = fun x -> f x (* Syntactic *)
        let rec f = let g x = f x in g (* Not syntactic *)
        let rec f = let a = ... in (fun x -> f x) (* Not syntactic *)
      ]}

    - Compilation: we finally combine all of this to produce a Lambda term
      for the recursive bindings.
*)

open Lambda

(** Allocation and backpatching primitives *)

let alloc_prim =
  Primitive.simple ~name:"caml_alloc_dummy" ~arity:1 ~alloc:true

let alloc_float_record_prim =
  Primitive.simple ~name:"caml_alloc_dummy_float" ~arity:1 ~alloc:true

let alloc_lazy_prim =
  Primitive.simple ~name:"caml_alloc_dummy_lazy" ~arity:1 ~alloc:true

let update_prim =
  (* Note: [alloc] could be false, but it probably doesn't matter *)
  Primitive.simple ~name:"caml_update_dummy" ~arity:2 ~alloc:true

let update_lazy_prim =
  Primitive.simple ~name:"caml_update_dummy_lazy" ~arity:2 ~alloc:true


(** {1. Sizing} *)

(* Simple blocks *)
type block_size =
  | Regular_block of int
  | Float_record of int
  | Lazy_block

type size =
  | Unreachable
  (** Non-returning expressions, like [raise exn].
      In [Value_rec_check], they would be classified as [Dynamic],
      but some of those appear during translation to Lambda.
      For example, in [let rec f = let [| x |] = ... in fun y -> x + y]
      the inner let binding gets translated to code that raises
      [Match_failure] for non-matching branches.
      Tracking [Unreachable] explicitly allows us to recover the size
      of the only non-raising branch. *)
  | Constant
  (** Constant values.
      Can be either an integer-like constant ([0], ['a'], [None],
      the empty list or the unit constructor), or a structured constant
      (["hello"], [Some 1], ...).

      Integer constants cannot be pre-allocated, so need their own
      classification and compilation scheme (See {!Compilation} below).
      Structured constants could fit into the [Block] category, but we
      choose to reuse the [constant] classification to avoid sorting
      through the [Lconst] definitions.
      It also generates slightly better code. *)
  | Function
  (** Function definitions.
      This includes more than just obvious, syntactic function definitions;
      see {!Function Lifting} for details. *)
  | Block of block_size
  (** Allocated values of a fixed size.
      This corresponds to expressions ending in a single obvious allocation,
      but also some more complex expressions where the block is bound to
      an intermediate variable before being returned.
  *)
  | Variable of { id : Ident.t }
  (** Unknown size, but looking up the definition of the variable could
      give us the actual size. *)

let dynamic_size () =
  Misc.fatal_error "letrec: No size found for Static binding"

let no_loc = Debuginfo.Scoped_location.Loc_unknown

(** Allocation and backpatching code *)

let compile_alloc size =
  let alloc prim size =
    Lprim (Pccall prim,
           [Lconst (Lambda.const_int size)],
           no_loc)
  in
  (* if you add new allocation primitives below,
     you should update {!find_size_of_alloc_prim} as well. *)
  match size with
  | Regular_block size ->
      alloc alloc_prim size
  | Float_record size ->
      alloc alloc_float_record_prim size
  | Lazy_block ->
      Lprim(Pccall alloc_lazy_prim,
            [Lambda.lambda_unit],
            no_loc)

let compile_lazy_indirect newval =
  let indirect = Lambda.transl_prim "CamlinternalLazy" "indirect" in
  Lapply {
    ap_func = indirect;
    ap_args = [newval];
    ap_loc = no_loc;
    ap_tailcall = Default_tailcall;
    ap_inlined = Default_inline;
    ap_specialised = Default_specialise;
  }

let compile_update size dummy newval =
  let prim, newval =
    match size with
    | Regular_block _ | Float_record _ ->
      update_prim, newval
    | Lazy_block ->
      (* Consider the following example from Vincent Laviron:
         {[let rec v =
             let l = lazy (expensive computation) in
             let () = maybe_force_in_another_domain l in
             l
         ]}

         The naive/simple compilation scheme would do
         a [caml_update_dummy_lazy(v, l)], and the dummy-update code
         could run concurrently with another domain forcing [l].

         To avoid this issue, lazy blocks get updated via
         [caml_update_dummy_lazy(dummy, CamlinternalLazy.indirect newval)],
         where [CamlinternalLazy.indirect] returns a fresh/local thunk
         that is not getting forced concurrently (whereas [newval]
         might be).
      *)
      update_lazy_prim,
      begin match newval with
        | Lprim(Pmakelazyblock _, _, _) ->
          (* No need to wrap the thunk if was just constructed.
             This removes indirections on terms defined as lazy thunks
             at the toplevel: [let rec x = lazy ...] *)
          newval
        | _ -> compile_lazy_indirect newval
      end
  in
  Lprim (Pccall prim, [dummy; newval],
         no_loc)

let add_update_dummy id size lam =
  let update = compile_update size (Lvar id) lam in
  Lsequence (update, Lvar id)

(* [join_sizes] is used to compute the size of an expression with multiple
   branches. Such expressions are normally classified as [Dynamic] by
   [Value_rec_check], so the default behaviour is a compile-time failure.
   However, for partial pattern-matching (typically in let bindings)
   the compiler will later add a branch for the failing cases, and this
   is handled here with the [Unreachable] case.
   Note that the current compilation scheme would work if we allowed the
   [Constant] and [Block] cases to be joined, but [Function] needs to be
   a single function. *)
let join_sizes size1 size2 =
  match size1, size2 with
  | Unreachable, size | size, Unreachable -> size
  | _, _ -> dynamic_size ()

(* We need to recognize the Pmakeblock that we transformed into
   primitive calls, to support size compilation in nested recursive
   definitions. Consider this example from Vincent Laviron:
   {[let f a =
       let rec x =
         let rec y = Some a in y
       in x
   ]}

   [let rec y = Some a in y] gets compiled to
   {[let y = caml_alloc_dummy 1 in
     caml_update_dummy(y, ...);
     y]}
   and we need to recognize from this definition that this
   value has known size [1].
*)
let find_size_of_alloc_prim prim args =
  let same_as other_prim =
    let open Primitive in
    String.equal prim.prim_name other_prim.prim_name
  in
  let int_arg = match args with
    | [Lconst (Const_base (Const_int n))] -> Some n
    | _ ->  None
  in
  if same_as alloc_prim then
    Option.map (fun n -> Regular_block n) int_arg
  else if same_as alloc_float_record_prim then
    Option.map (fun n -> Float_record n) int_arg
  else if same_as alloc_lazy_prim then
    Some Lazy_block
  else None

let compute_static_size bound_id lam =
  let rec compute_expression_size lam =
    match lam with
    | Lvar id -> lam, Variable { id }
    | Lmutvar _ -> dynamic_size ()
    | Lconst _ -> lam, Constant
    | Lapply _ -> dynamic_size ()
    | Lfunction _ -> lam, Function
    | Llet (kind, vk, id, def, body) ->
      let body, size = compute_expression_size body in
      begin match size with
      | Variable { id = id' } when Ident.same id id' ->
        let new_def, size = compute_expression_size def in
        Llet(kind, vk, id, new_def, body), size
      | _ ->
        Llet(kind, vk, id, def, body), size
      end
    | Lmutlet(vk, id, def, body) ->
      let body, size = compute_expression_size body in
      Lmutlet(vk, id, def, body), size
    | Lletrec (bindings, body) ->
      let body, size = compute_expression_size body in
      begin match size with
      | Variable { id = id' }
        when List.exists (fun { id; _ } -> Ident.same id id') bindings ->
        Lletrec (bindings, body), Function
      | _ ->
        Lletrec (bindings, body), size
      end
    | Lprim (p, args, _) ->
      begin match size_of_primitive p args with
      | (Constant | Function | Unreachable) as size ->
        lam, size
      | Block block_size as size ->
        add_update_dummy bound_id block_size lam, size
      | Variable { id = _ } ->
        (* This case cannot be handled well.
           Currently it can only happen if the primitive is [Pduparray],
           and the argument is something that ends with a variable.
           This is never generated directly by Translcore, and the
           rewritings done during this pass should also guarantee
           that we do not end up in this case. *)
        dynamic_size ()
      end
    | Lswitch (arg, sw, loc) ->
      let fail_case =
        match sw.sw_failaction with
        | None -> []
        | Some fail -> [0 (* ignored *), fail]
      in
      let all_cases = [sw.sw_consts; sw.sw_blocks; fail_case] in
      begin match compute_and_join_sizes_switch all_cases with
      | [sw_consts; sw_blocks; maybe_fail], size ->
        let sw_failaction =
          match maybe_fail with
          | [] -> None
          | [ _, fail ] -> Some fail
          | _ ->
            Misc.fatal_error
              "Unexpected result from compute_and_join_sizes_switch"
        in
        Lswitch (arg, { sw with sw_consts; sw_blocks; sw_failaction }, loc),
        size
      | _ ->
          Misc.fatal_error
            "Unexpected result from compute_and_join_sizes_switch"
      end
    | Lstringswitch (arg, cases, fail, loc) ->
      let fail_case =
        match fail with
        | None -> []
        | Some fail -> ["" (* ignored *), fail]
      in
      let all_cases = [cases; fail_case] in
      begin match compute_and_join_sizes_switch all_cases with
      | [cases; maybe_fail], size ->
        let fail =
          match maybe_fail with
          | [] -> None
          | [ _, fail ] -> Some fail
          | _ ->
            Misc.fatal_error
              "Unexpected result from compute_and_join_sizes_switch"
        in
        Lstringswitch (arg, cases, fail, loc), size
      | _ ->
          Misc.fatal_error
            "Unexpected result from compute_and_join_sizes_switch"
      end
    | Lstaticraise _ -> lam, Unreachable
    | Lstaticcatch (body, params, handler) ->
      (* Note: we don't follow aliases through handler parameters *)
      begin match compute_and_join_sizes [body; handler] with
      | [body; handler], size ->
        Lstaticcatch (body, params, handler), size
      | ([] | [_] | _::_::_::_), _ ->
          Misc.fatal_error
            "Unexpected result from compute_and_join_sizes"
      end
    | Ltrywith (body, id, handler) ->
      begin match compute_and_join_sizes [body; handler] with
      | [body; handler], size ->
        Ltrywith (body, id, handler), size
      | ([] | [_] | _::_::_::_), _ ->
          Misc.fatal_error
            "Unexpected result from compute_and_join_sizes"
      end
    | Lifthenelse (cond, ifso, ifnot) ->
      begin match compute_and_join_sizes [ifso; ifnot] with
      | [ifso; ifnot], size ->
        Lifthenelse (cond, ifso, ifnot), size
      | ([] | [_] | _::_::_::_), _ ->
          Misc.fatal_error
            "Unexpected result from compute_and_join_sizes"
      end
    | Lsequence (e1, e2) ->
      let e2, size = compute_expression_size e2 in
      Lsequence (e1, e2), size
    | Lwhile _
    | Lfor _
    | Lassign _ -> lam, Constant
    | Lsend _ -> dynamic_size ()
    | Levent (e, ev) ->
      let e, size = compute_expression_size e in
      Levent (e, ev), size
    | Lifused _ -> lam, Constant
  and compute_and_join_sizes branches =
    List.fold_right (fun branch (branches, size) ->
        let branch, size_branch = compute_expression_size branch in
        let size = join_sizes size size_branch in
        branch :: branches, size)
      branches ([], Unreachable)
  and compute_and_join_sizes_switch :
    type a. (a * lambda) list list -> (a * lambda) list list * size =
    fun all_cases ->
      List.fold_right (fun cases (all_cases, size) ->
          let cases, size =
            List.fold_right (fun (key, action) (cases, size) ->
                let action, size_action = compute_expression_size action in
                let size = join_sizes size size_action in
                (key, action) :: cases, size)
              cases ([], size)
          in
          cases :: all_cases, size)
        all_cases ([], Unreachable)
  and size_of_primitive p args =
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
    | Pbigstring_set_64 _
    | Ppoll ->
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
    | Pmakelazyblock _ ->
        Block Lazy_block
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
            (* Note: We're ignoring the rewritten expression, because in this
               case we want to push the rewriting outwards, around the
               [Pduparray] primitive. *)
            let _, size = compute_expression_size arg in
            size
        | [] | _ :: _ :: _ ->
            Misc.fatal_error "size_of_primitive"
        end

    | Praise _ ->
        Unreachable

    | Pctconst _ ->
        (* These primitives are not special-cased by [Value_rec_check],
           so we should never end up here; but these are constants anyway. *)
        Constant

    | Pccall prim ->
        begin match find_size_of_alloc_prim prim args with
        | Some size -> Block size
        | None -> dynamic_size ()
        end

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
    | Patomic_load
    | Popaque
    | Pdls_get ->
        dynamic_size ()
  in
  match compute_expression_size lam with
  | _, (Constant | Function | Unreachable | Variable _ as size) ->
    (* See comment in the Lprim case: we drop the rewritten term because
       it may contain calls to [caml_update_dummy] added preventively *)
    lam, size
  | (_rewritten, Block _) as result -> result

let lfunction_with_body { kind; params; return; body = _; attr; loc } body =
  lfunction' ~kind ~params ~return ~body ~attr ~loc

(** {1. Function Lifting} *)

(* The compiler allows recursive definitions of functions that are not
   syntactic functions:
   {[
     let rec f_syntactic_function = fun x ->
       f_syntactic_function x

     let rec g_needs_lift =
       let () = ... in
       (fun x -> g_needs_lift (foo x))

     let rec h_needs_lift_and_closure =
       let v = ref 0 in
       (fun x -> incr v; h_needs_lift_and_closure (bar x))

     let rec i_needs_lift_and_eta =
       let aux x = i_needs_lift_and_eta (baz x) in
       aux
   ]}

   We need to translate those using only syntactic functions or blocks.
   For some functions, we only need to lift a syntactic function in tail
   position from its surrounding context:
   {[
     let rec g_context =
       let () = ... in
       ()
     and g_lifted = fun x ->
       g_lifted (foo x)
   ]}

   In general the function may refer to local variables, so we perform
   a local closure conversion before lifting:
   {[
     let rec h_context =
       let v = ref 0 in
       { v }
     and h_lifted = fun x ->
       incr h_context.v;
       h_lifted (bar x)
   ]}
   Note that the closure environment computed from the context is passed as a
   mutually recursive definition, that is, a free variable, and not as an
   additional function parameter (which is customary for closure conversion).

   Finally, when the tail expression is a variable, we perform an eta-expansion
   to get a syntactic function, that we can then close and lift:
   {[
     let rec i_context =
       let aux x = i_lifted (baz x) in
       { aux }
     and i_lifted = fun x -> i_context.aux x
   ]}
*)

type lifted_function =
  { lfun : Lambda.lfunction;
    free_vars_block_size : int;
  }

type 'a split_result =
  | Unreachable
  | Reachable of lifted_function * 'a

let ( let+ ) res f =
  match res with
  | Unreachable -> Unreachable
  | Reachable (func, lam) -> Reachable (func, f lam)

(* The closure blocks are immutable.
   (Note: It is usually safe to declare immutable blocks as mutable,
   but in this case the blocks might be empty and declaring them as Mutable
   would cause errors later.) *)
let lifted_block_mut : Asttypes.mutable_flag = Immutable

let build_closure_block block_var args =
  let lam = Lprim (Pmakeblock (0, lifted_block_mut, None), args, no_loc) in
  let block_size = Regular_block (List.length args) in
  add_update_dummy block_var block_size lam

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
    Reachable (lifted, build_closure_block block_var [Lvar v])
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
    (* Note: When there are no local free variables, we don't need the
       substitution and we don't need to generate code for pre-allocating
       and backpatching a block of size 0.
       However, the general scheme also works and it's unlikely to be
       noticeably worse, so we use it for simplicity. *)
    let new_fun =
      lfunction_with_body lfun
        (Lambda.subst (fun _ _ env -> env) subst lfun.body)
    in
    let lifted = { lfun = new_fun; free_vars_block_size } in
    let block = build_closure_block block_var (List.rev block_fields_rev) in
    Reachable (lifted, block)
  | Llet (lkind, vkind, var, def, body) ->
    let+ body =
      split_static_function block_var (Ident.Set.add var local_idents) body
    in
    Llet (lkind, vkind, var, def, body)
  | Lmutlet (vkind, var, def, body) ->
    let+ body =
      split_static_function block_var (Ident.Set.add var local_idents) body
    in
    Lmutlet (vkind, var, def, body)
  | Lletrec (bindings, body) ->
    let local_idents =
      List.fold_left (fun ids { id } -> Ident.Set.add id ids)
        local_idents bindings
    in
    let+ body =
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
    let+ e2 = split_static_function block_var local_idents e2 in
    Lsequence (e1, e2)
  | Levent (lam, lev) ->
    let+ lam = split_static_function block_var local_idents lam in
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

(** {1. Compilation} *)

(** The bindings are split into three categories.
    Static bindings are the ones that we can pre-allocate and backpatch later.
    Function bindings are syntactic functions.
    Dynamic bindings are non-recursive expressions.

    The evaluation order is as follows:
    - Evaluate all dynamic bindings
    - Pre-allocate all static bindings
    - Define all functions
    - Backpatch all static bindings

    Constants (and unreachable expressions) end up in the dynamic category,
    because we substitute all occurrences of recursive variables in their
    definition by a dummy expression, making them non-recursive.

    This is correct because:
    - [Value_rec_check] ensured that they never dereference the value of
      those recursive variables
    - their final value cannot depend on them either.

    Functions that are not already in syntactic form also generate an additional
    binding for the context. This binding fits into the static category.

    Example input:
    {[
      let rec a x =
        (* syntactic function *)
        b x
      and b =
        (* non-syntactic function *)
        let tbl = Hashtbl.make 17 in
        fun x -> ... (tbl, c, a) ...
      and c =
        (* block *)
        Some (d, default)
      and d =
        (* 'dynamic' value (not recursive *)
        Array.make 5 0
      and default =
        (* constant, with (spurious) use
           of a recursive neighbor *)
        let _ = a in
        42
    ]}

    Example output:
    {[
      (* Dynamic bindings *)
      let d = Array.make 5 0
      let default =
        let _ = *dummy_rec_value* in
        42

      (* Pre-allocations *)
      let c = caml_alloc_dummy 2
      let b_context = caml_alloc_dummy 1

      (* Functions *)
      let rec a x = b x
      and b =
        fun x -> ... (b_context.tbl, c, a) ...

      (* Backpatching *)
      let () =
        caml_update_dummy c (Some (d, default));
        caml_update_dummy b_context
          (let tbl = Hashtbl.make 17 in
           { tbl })
    ]}

    Note on performance for non-syntactic functions:
    The compiler would previously pre-allocate and backpatch function
    closures. The new approach is designed to avoid back-patching
    closures -- besides, we could not pre-allocate at this point in the
    compiler pipeline, as the closure size will only be determined later.

    For non-syntactic functions with local free variables, we now store the
    local free variables in a block, which incurs an additional indirection
    whenever a local variable is accessed by the function. On the other hand,
    we generate regular function definitions, so the rest of the compiler
    can either inline them or generate direct calls, and use the compact
    representation for mutually recursive closures.
 *)

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

(** Compilation function *)

let compile_letrec input_bindings body =
  let subst_for_constants =
    List.fold_left (fun subst (id, _, _) ->
        Ident.Map.add id Lambda.dummy_constant subst)
      Ident.Map.empty input_bindings
  in
  let all_bindings_rev =
    List.fold_left (fun rev_bindings (id, rkind, def) ->
        match (rkind : Value_rec_types.recursive_binding_kind) with
        | Dynamic ->
          { rev_bindings with dynamic = (id, def) :: rev_bindings.dynamic }
        | Static ->
          let def, size = compute_static_size id def in
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
              let ctx_id = Ident.create_local "letrec_function_context" in
              begin match split_static_function ctx_id Ident.Set.empty def with
              | Unreachable ->
                Misc.fatal_error "letrec: no function for binding"
              | Reachable ({ lfun; free_vars_block_size }, lam) ->
                let functions = (id, lfun) :: rev_bindings.functions in
                let static =
                  (ctx_id, Regular_block free_vars_block_size, lam) ::
                  rev_bindings.static
                in
                { rev_bindings with functions; static }
              end
            end
          | Variable { id = id' } ->
            Misc.fatal_errorf "Definition of %a has a size that depends on %a"
              Ident.print id Ident.print id'
          end)
      empty_bindings input_bindings
  in
  let body_with_patches =
    List.fold_left (fun body (_id, _size, def) ->
        (* The definition contains a call to [caml_update_dummy] somewhere,
           so we run it for its side effects and discard the result. *)
        Lsequence (def, body))
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
        let alloc = compile_alloc size in
        Llet(Strict, Pgenval, id, alloc, body))
      body_with_dynamic_values all_bindings_rev.static
  in
  body_with_pre_allocations
