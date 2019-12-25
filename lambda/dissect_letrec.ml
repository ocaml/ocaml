(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Pierre Chambart, Vincent Laviron and Louis Gesbert, OCamlPro      *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Lambda

(* Converts let-rec containing values into an initialization then
   assignment sequence.

   We assume that the typechecker correctly validated that the letrec
   is compilable (See typing/Rec_check.is_valid_recursive_expression).

   That is, for every expression to which a variable is bound in
   the let-rec verify:
   * It does not inspect any variable bound by the let-rec.
     This implies that the value of a variable bound by the let-rec
     does not need to be valid during the computation of the other
     bindings. Only the address is needed. This means that we can
     preallocate values for which we know the size.
   * If the value can't be preallocated (it can be because the
     size can't be statically known, or because the value is not
     allocated, like integers), then the value does not
     depend on any other rec-bound variables.
     This implies that any value that can't be preallocated can be
     computed before any other binding. Note that this does not mean
     that other variables can't be refered to by the expression,
     but if it happens, then the value is useless.

   We consider two cases for expressions that we consider of
   known size (the [Static] case in [is_valid_recursive_expression]):
   the makeblock primitive and function declarations. Makeblocks will
   be preallocated, while all the functions will be turned into a single
   letrec.

   The structure of the generated code will be:
   {[
     let c = const in
     (* [consts]: constants *)
     ...
     let v = caml_alloc_dummy n in
     (* [blocks]: With n the staticaly known size of blocks *)
     ...
     let p = expr in
     (* [pre]: Values that do not depend on any other from the let-rec in
        [Rec_check.Dereference] position *)
     ...
     let rec f x = ...
     and g x = ...
     and ...
     in
     (* [functions]: All the functions from the let-rec *)
     caml_update_dummy v v_contents;
     (* Initialisation ([effects]) *)
     ...
   ]}

   Special care is taken to handle nested [let rec]s:
   the recursive values of a letrec are often more than the ones
   bound by the letrec expression itself. For instance

   let rec f =
     let rec g x = h (x+1)
     and h x = i x
     in
     fun x -> g x
   and i x =
     if x > 33 then x
     else f x

   in this expression every function is recursively defined with
   any other. Hence this is equivalent to

   let rec f = fun x -> g x
   and i x =
     if x > 33 then x
     else f x
   and g x = h (x+1)
   and h x = i x

   However, there might be (a subset of) a local [let rec] that is indeed
   internally recursive, but that is used by the top-level [let rec] in
   [Dereference] positions. We carefully lift those within [pre], and handle
   them recursively in the same pass to preserve the order of evaluation.

   The analysis for which variables should remain in the inner [let rec], and
   which are indeed part of the outer [let rec] is equivalent to the
   [Rec_check.value_bindings] case.
*)

type block_type = Normal of int (* tag *) | Boxed_float

type block = { block_type : block_type; size : int; }

type letrec = {
  blocks : (Ident.t * block) list;
  (* Pre-allocated blocks.
     Will result in [let id = caml_alloc_dummy size] or
     [let id = caml_alloc_dummy_float size] *)
  consts : (Ident.t * Lambda.structured_constant) list;
  (* Statically known values *)
  pre : tail:Lambda.lambda -> Lambda.lambda;
  (* Prefix of the expression that does not depends on any recursive part.
     This is presented as a function for easy 'concatenation':
     to append 'expr': [fun ~tail -> Lsequence (expr, letrec.pre ~tail)] *)
  effects : Lambda.lambda;
  (* Effects that are applied afterwards. *)
  functions : (Ident.t * Lambda.lfunction) list;
  substitution : Ident.t Ident.Map.t;
  (* Alias to recursive variables should be forbidden, to prevent
     non-productive definition like 'let rec a = a'. But some aliases
     are allowed, for instance 'let rec a = let c = b in 1 :: b and b = 2 :: a'.
     The simplest way to handle those aliases is simply
     to apply a substitute of all these aliases afterward. *)
  letbound : Ident.Set.t;
  (* Set of known immutable variables. Mutable ones cannot define aliases *)
}

type let_def = {
  let_kind : Lambda.let_kind;
  value_kind : Lambda.value_kind;
  ident : Ident.t;
}

exception Bug

let () = ignore Bug

let lsequence (lam1, lam2) =
  match lam1 with
  | Lsequence (lam, Lconst (Const_pointer 0)) -> Lsequence (lam, lam2)
  | Lconst (Const_pointer 0) -> lam2
  | _ -> Lsequence (lam1, lam2)

let caml_update_dummy_prim =
  Primitive.simple ~name:"caml_update_dummy" ~arity:2 ~alloc:true

let update_dummy var expr =
  Lprim (Pccall caml_update_dummy_prim, [Lvar var; expr], Location.none)

let build_block let_def size block_type expr letrec =
  { letrec with
    blocks = (let_def.ident, { block_type; size }) :: letrec.blocks;
    effects = Lsequence (update_dummy let_def.ident expr, letrec.effects);
  }

let is_simple (lam:Lambda.lambda) =
  match lam with
  | Lvar _
  | Lconst _ -> true
  | _ -> false

let dead_code lam letrec =
  (* Some cases generate code without effects, and bound to nothing. We use this
     function to insert it as [Lsequence] in [effects], for documentation.
     It would be correct to discard and just return [letrec] instead. *)
  { letrec with effects = lsequence (lam, letrec.effects) }

(* We desconstruct the let-rec into a description *)

let rec prepare_letrec
    (recursive_set:Ident.Set.t)
    (* Variables that depends on the let-rec bound variables *)
    (current_let:let_def option)
    (* The variable to which the current expression is bound.
       current_let.ident is part of recursive_set *)
    (lam:Lambda.lambda)
    (letrec:letrec) =
  match lam with
  | Lfunction funct -> begin
      match current_let with
      | Some current_let when Ident.Set.mem current_let.ident recursive_set ->
          { letrec with functions =
                          (current_let.ident, funct) :: letrec.functions }
      | Some current_let ->
          (* If the currently bound function does not depend on any
             recursive variable *)
          let pre ~tail : Lambda.lambda =
            Llet (current_let.let_kind, current_let.value_kind,
                  current_let.ident, lam, letrec.pre ~tail)
          in
          { letrec with pre }
      | None -> dead_code lam letrec
    end
  | Lprim ((Pmakeblock _ | Pmakearray (_, _) | Pduprecord (_, _))
           as prim, args, dbg)
    when not (List.for_all is_simple args) ->
      (* If there are some non-trivial expressions as arguments, we
         first extract the arguments (to let-bound variables) before
         deconstructing. Arguments could contain side effects and other
         blocks declarations. *)
      let defs, args =
        List.fold_right (fun (def:Lambda.lambda) (defs, args) ->
            (* Fold-right to preserve the list order *)
            if is_simple def then
              (* This prevents looping on variables *)
              defs, def :: args
            else
              let id = Ident.create_local "lift_in_letrec" in
              (id, def) :: defs, (Lambda.Lvar id) :: args)
          args ([], [])
      in
      (* Bytecode evaluates effects in blocks from right to left,
         so reverse defs to preserve evaluation order.
         Relevant test: letrec/evaluation_order_3 *)
      let lam =
        List.fold_left (fun body (id, def) : Lambda.lambda ->
            Llet (Strict, Pgenval, id, def, body))
          (Lambda.Lprim (prim, args, dbg)) defs
      in
      prepare_letrec recursive_set current_let lam letrec
  | Lprim (Pmakeblock _, args, _)
  | Lprim (Pmakearray ((Paddrarray|Pintarray), _), args, _) -> begin
      match current_let with
      | Some cl -> build_block cl (List.length args) (Normal 0) lam letrec
      | None -> dead_code lam letrec
      (* We know that [args] are all "simple" at this point, so no effects *)
    end
  | Lprim (Pmakearray (Pfloatarray, _), args, _) -> begin
      match current_let with
      | Some cl -> build_block cl (List.length args) Boxed_float lam letrec
      | None -> dead_code lam letrec
    end
  | Lprim (Pduprecord (kind, size), args, _) -> begin
      match current_let with
      | Some cl ->
          let arg =
            match args with
            | [ arg ] -> arg
            | _ -> Misc.fatal_error "Dissect_letrec.prepare_letrec duprecord"
          in
          (match kind with
           | Types.Record_regular ->
               build_block cl size (Normal 0) arg letrec
           | Types.Record_inlined tag ->
               build_block cl size (Normal tag) arg letrec
           | Types.Record_extension _ ->
               build_block cl (size + 1) (Normal 0) arg letrec
           | Types.Record_unboxed _ ->
               assert false
           | Types.Record_float ->
               build_block cl size Boxed_float arg letrec)
      | None -> dead_code lam letrec
    end
  | Lconst const ->
      (match current_let with
       | Some current_let ->
           { letrec with consts = (current_let.ident, const) :: letrec.consts }
       | None -> dead_code lam letrec)
  | Llet (Variable, _, _, _, _) ->
      (* This is not supposed to appear at this point *)
      assert false
  | Llet ((Strict | Alias | StrictOpt) as let_kind, value_kind, id, def, body)
    ->
      let letbound = Ident.Set.add id letrec.letbound in
      let letrec = { letrec with letbound } in
      let free_vars = Lambda.free_variables def in
      if Ident.Set.disjoint free_vars recursive_set then
        (* Non recursive let *)
        let letrec = prepare_letrec recursive_set current_let body letrec in
        let pre ~tail : Lambda.lambda =
          Llet (let_kind, value_kind, id, def, letrec.pre ~tail)
        in
        { letrec with pre }
      else
      let recursive_set = Ident.Set.add id recursive_set in
      let letrec = prepare_letrec recursive_set current_let body letrec in
      let let_def = { let_kind; value_kind; ident = id } in
      prepare_letrec recursive_set (Some let_def) def letrec
  | Lsequence (lam1, lam2) ->
      let letrec = prepare_letrec recursive_set current_let lam2 letrec in
      prepare_letrec recursive_set None lam1 letrec
  | Levent (body, event) ->
      let letrec = prepare_letrec recursive_set current_let body letrec in
      { letrec with effects = Levent (letrec.effects, event) }
  | Lletrec (bindings, body) ->
      (* Inner letrecs need some special care: We split between _outer_ bindings
         that are recursive with the current [recursive_set], and therefore need
         to be merged into it, and _inner_ bindings that are not (but can still
         be recursive between themselves). This corresponds to the [Recursive]
         case in [Rec_check.value_bindings].

         One solution would be to handle now the outer bindings, and lift the
         inner ones into [letrec.pre], to be handled in a second pass. That
         would change the evaluation order, though, so we instead descend
         recursively in a single pass. This requires separate accumulators that
         we re-integrate in the right place in [letrec] afterwards, so that the
         outer bindings can safely depend upon them. *)
      let deps =
        List.fold_left (fun acc (x, def) ->
            Ident.Map.add x (Lambda.free_variables def) acc)
          Ident.Map.empty
          bindings
      in
      let vars = Ident.Map.keys deps in
      let reverse_deps =
        (* Set.t Map.t to Set.t Map.t transposition to get reverse
           dependencies *)
        let add_opt x = function
          | None -> Some (Ident.Set.singleton x)
          | Some s -> Some (Ident.Set.add x s)
        in
        Ident.Map.fold (fun x deps reverse_deps ->
            Ident.Set.fold (fun d reverse_deps ->
                Ident.Map.update d (add_opt x) reverse_deps)
              deps reverse_deps)
          deps Ident.Map.empty
      in
      let recursive_set =
        (* and a fixpoint to get their transitive counterpart *)
        Ident.Set.fixpoint (fun x ->
            try Ident.Map.find x reverse_deps with Not_found -> Ident.Set.empty)
          recursive_set
      in
      let outer_vars = Ident.Set.inter vars recursive_set in

      if Ident.Set.is_empty outer_vars then
        (* Non recursive relative to top-level letrec, we can avoid dissecting
           it right now. Its turn will come later. *)
        let letrec = prepare_letrec recursive_set current_let body letrec in
        let pre ~tail : Lambda.lambda =
          Lletrec (bindings, letrec.pre ~tail)
        in
        { letrec with pre }
      else

      let letrec =
        { letrec with letbound =
                        Ident.Set.union letrec.letbound vars }
      in
      let letrec =
        prepare_letrec recursive_set current_let body letrec
      in
      let pre, letrec =
        (* extract the current [pre], so that definitions from the inner letrec
           can be re-inserted in the middle *)
        letrec.pre, { letrec with pre = fun ~tail -> tail }
      in
      let letrec, inner_effects, inner_functions =
        List.fold_right
          (fun (id, def) (letrec, inner_effects, inner_functions) ->
            let let_def = {
              let_kind = Strict;
              value_kind = Pgenval;
              ident = id;
            } in
            if Ident.Set.mem id outer_vars then
              prepare_letrec recursive_set (Some let_def) def letrec,
              inner_effects, inner_functions
            else
              let { blocks; consts; pre; effects;
                    functions; substitution; letbound } =
                letrec
              in
              let inner_letrec = {
                effects = Lconst (Const_pointer 0);
                functions = [];
                (* these can be safely handled in common *)
                blocks; consts; pre; substitution; letbound;
              } in
              let inner_letrec =
                prepare_letrec
                  (Ident.Set.diff vars outer_vars) (Some let_def)
                  def inner_letrec
              in
              { inner_letrec with effects; functions },
              inner_letrec.effects :: inner_effects,
              inner_letrec.functions @ inner_functions
          )
          bindings
          (letrec, [], [])
      in
      let pre =
        List.fold_left (fun pre -> function
            | Lconst (Const_pointer 0) -> pre
            | eff -> fun ~tail -> Lsequence (eff, pre ~tail))
          pre inner_effects
      in
      let pre =
        match inner_functions with
        | [] -> pre
        | _ :: _ ->
            let functions =
              List.map (fun (id, lfun) -> id, Lfunction lfun) inner_functions
            in
            fun ~tail -> Lletrec (functions, pre ~tail)
      in
      let pre ~tail = letrec.pre ~tail:(pre ~tail) in
      { letrec with pre }

  | Lvar id when Ident.Set.mem id letrec.letbound ->
      (* This cannot be a mutable variable: it is ok to copy it *)
      (match current_let with
       | Some cl ->
           let substitute_from =
             Ident.Map.fold (fun x y acc ->
                 if Ident.equal y cl.ident then Ident.Set.add x acc
                 else acc)
               letrec.substitution
               (Ident.Set.singleton cl.ident)
           in
           let substitution =
             Ident.Set.fold (fun from -> Ident.Map.add from id)
               substitute_from letrec.substitution
           in
           let letbound = Ident.Set.add cl.ident letrec.letbound in
           { letrec with substitution; letbound }
       | None -> dead_code lam letrec)

  | Lifused (_v, lam) ->
      prepare_letrec recursive_set current_let lam letrec

  | Lwhile (_, _)
  | Lfor (_, _, _, _, _)
  | Lassign (_, _) ->
      (* Effect expressions returning unit. The result can be
         pre-declared.  *)
      let consts = match current_let with
        | Some cl -> (cl.ident, Const_pointer 0) :: letrec.consts
        | None -> letrec.consts
      in
      { letrec with
        effects = Lsequence (lam, letrec.effects);
        consts; }

  | Lapply _
  | Lswitch (_, _, _)
  | Lstringswitch (_, _, _, _)
  | Lstaticraise (_, _)
  | Lstaticcatch (_, _, _)
  | Ltrywith (_, _, _)
  | Lifthenelse (_, _, _)
  | Lsend (_, _, _, _, _)
  | Lvar _
  | Lprim (_, _, _) ->
      (* This cannot be recursive, otherwise it should have been caught
         by the well formedness check. Hence it is ok to evaluate it
         before anything else. *)
      (* Check that no recursive variable appears in a position where it is
         inspected (appearances in guarded positions in other cases are OK) *)
      let no_recurse = match lam with
        | Lstaticcatch (_, _, _)
        | Ltrywith (_, _, _)
          -> None
        | Lswitch (lam1, _, _)
        | Lstringswitch (lam1, _, _, _)
        | Lifthenelse (lam1, _, _)
          -> Some lam1
        | Lapply _
        | Lstaticraise _
        | Lsend _
        | Lvar _
        | Lprim _
          -> Some lam
        | _ -> assert false
      in
      Option.iter (fun lam ->
          let free_vars = Lambda.free_variables lam in
          if not (Ident.Set.disjoint free_vars recursive_set)
          then
            Misc.fatal_errorf "Unallowed recursive access to %a in:@.%a@."
              Ident.Set.print (Ident.Set.inter free_vars recursive_set)
              Printlambda.lambda lam)
        no_recurse;
      let pre = match current_let with
        | Some cl -> fun ~tail : Lambda.lambda ->
            Llet (cl.let_kind, cl.value_kind, cl.ident, lam, letrec.pre ~tail)
        | None -> fun ~tail : Lambda.lambda ->
            Lsequence (lam, letrec.pre ~tail)
      in
      { letrec with pre }

let dissect_letrec ~bindings ~body =

  let letbound =
    Ident.Set.of_list (List.map fst bindings)
  in

  let letrec =
    List.fold_right (fun (id, def) letrec ->
        let let_def = {
          let_kind = Strict;
          value_kind = Pgenval;
          ident = id;
        } in
        prepare_letrec letbound (Some let_def) def letrec)
      bindings
      { blocks = [];
        consts = [];
        pre = (fun ~tail -> tail);
        effects = Lconst (Const_pointer 0);
        functions = [];
        substitution = Ident.Map.empty;
        letbound;
      }
  in

  let preallocations =
    let loc = Location.none in
    List.map (fun (id, { block_type; size }) ->
        let fn =
          match block_type with
          | Normal _tag -> "caml_alloc_dummy"
          | Boxed_float -> "caml_alloc_dummy_float"
        in
        let desc = Primitive.simple ~name:fn ~arity:1 ~alloc:true in
        let size : lambda = Lconst (Const_base (Const_int size)) in
        id, Lprim (Pccall desc, [size], loc))
      letrec.blocks
  in

  let effects_then_body = lsequence (letrec.effects, body) in
  let functions =
    match letrec.functions with
    | [] -> effects_then_body
    | _ :: _ ->
        let functions =
          List.map (fun (id, lfun) -> id, Lfunction lfun) letrec.functions
        in
        Lletrec (functions, effects_then_body)
  in
  let with_non_rec =
    letrec.pre ~tail:functions
  in
  let with_preallocations =
    List.fold_left
      (fun body (id, binding) ->
         Llet (Strict, Pgenval, id, binding, body))
      with_non_rec
      preallocations
  in
  let with_constants =
    List.fold_left
      (fun body (id, const) ->
         Llet (Strict, Pgenval, id, Lconst const, body))
      with_preallocations
      letrec.consts
  in
  let substituted =
    Lambda.rename
      letrec.substitution
      with_constants
  in

  substituted

type dissected =
  | Dissected of Lambda.lambda
  | Unchanged

let dissect_letrec ~bindings ~body =
  let is_a_function = function
    | _, Lfunction _ -> true
    | _, _ -> false
  in
  if List.for_all is_a_function bindings then
    Unchanged
  else
    try
      Dissected (dissect_letrec ~bindings ~body)
    with Bug ->
      Misc.fatal_errorf "let-rec@.%a@."
        Printlambda.lambda (Lletrec (bindings, body))

let preallocate_letrec ~bindings ~body =
  let bindings = List.rev bindings in
  let body_with_initialization =
  List.fold_left
    (fun body (id, def, _size) -> Lsequence (update_dummy id def, body))
    body bindings
  in
  List.fold_left
    (fun body (id, _def, size) ->
       let desc =
         Primitive.simple ~name:"caml_alloc_dummy" ~arity:1 ~alloc:true
       in
       let size : lambda = Lconst (Const_base (Const_int size)) in
       Llet (Strict, Pgenval, id,
             Lprim (Pccall desc, [size], Location.none), body))
    body_with_initialization bindings
