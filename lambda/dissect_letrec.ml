(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Vincent Laviron, OCamlPro                *)
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

let debug x =
  match Sys.getenv "DISSECT_DEBUG" with
  | "" | "0" | exception Not_found ->
      Format.ifprintf Format.std_formatter x
  | _ ->
      Format.fprintf Format.std_formatter x

(* Converts let-rec containing values into an initialization then
   assignment sequence.

   We assume that the typechecker correclty validated that the letrec
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
   known size: the makeblock primitive and function declarations.
   Makeblocks will be preallocated, while all the functions will
   be turned into a single letrec.

   Note that this does not exactly match the definition of Static
   and Dynamic size for is_valid_recursive_expression. It considers
   Static to be anything that can be preallocated or that can't
   contain the value of its dependencies. Neither containing, nor
   using (inspecting) a value means that its content is useless.
   Hence it can be replaced by anything.

   For instance

   let rec a =
     for i = 0 to 10 do let _ = () :: b in () done
   and b = a :: []

   The result of a 'for' expression cannot be preallocated, but also
   cannot contain any value, hence, cannot contain b, even if it
   depends on it. Since it also cannot inspect it, this means
   that the value of b is completely useless in the definition
   of a.

   The structure of the generated code will be:

   let p = expr in
   (* Values that do not depend on any other from the let-rec *)
   ...
   let v = caml_alloc_dummy n in
   (* With n the staticaly known size of blocks *)
   ...
   let n = dummy_value in
   (* The values that can't be used *)
   ...
   let x = expr in
   (* The values that can't be preallocated but can depend on
      others. The expression might refer to dummy values, but
      they will never be used. *)
   ...
   let rec f x = ...
   and g x = ...
   and ...
   in
   (* All the functions from the let-rec *)
   let v_contents = expr in
   (* The expressions with statically known size that have been
      preallocated *)
   ...
   caml_update_dummy v v_contents;
   ...


   The recursive values of a letrec are not only the one bound
   by the letrec expression itself. For instance

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
  immutables : Ident.Set.t;
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
  | Lfunction funct ->
      (match current_let with
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
       | None -> letrec) (* dead code *)
  | Lprim ((Pmakeblock _ | Pmakearray (_, _) | Pduprecord (_, _)) as prim, args, dbg)
    when not (List.for_all is_simple args) ->
      (* If there are some non-trivial expressions as arguments, we
         first extract the arguments (to let-bound variables) before
         deconstructing. Arguments could contain side effects and
         other blocks declarations *)
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
  | Lprim (Pmakearray ((Paddrarray|Pintarray), _), args, _) ->
      (match current_let with
       | Some cl -> build_block cl (List.length args) (Normal 0) lam letrec
       | None -> letrec)
  | Lprim (Pmakearray (Pfloatarray, _), args, _) ->
      (match current_let with
       | Some cl -> build_block cl (List.length args) Boxed_float lam letrec
       | None -> letrec)
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
      | None -> letrec
    end
  | Lconst const ->
      (match current_let with
       | Some current_let ->
           { letrec with consts = (current_let.ident, const) :: letrec.consts }
       | None -> letrec)
  | Llet (Variable, k, id, def, body) ->
      let letrec = prepare_letrec recursive_set current_let body letrec in
      (* Variable let comes from mutable values, and reading from it is
         considered as inspections by Typecore.check_recursive_expression.
         This means that either:
         - the value does not depend on any recursive value,
         - or it is not read in the let-rec
      *)

      (* TODO: binder dans une variable temporaire *)

      let free_vars_def = Lambda.free_variables def in
      if Ident.Set.disjoint free_vars_def recursive_set then
        let pre ~tail : Lambda.lambda =
          Llet (Variable, k, id, def, letrec.pre ~tail)
        in
        { letrec with pre }
      else begin
        let free_vars_body = Lambda.free_variables body in
        (* This is infrequent enought for not caring
           about performances *)
        assert(not (Ident.Set.mem id free_vars_body));
        (* It is not used, we only keep the effect *)
        { letrec with effects = Lsequence (def, letrec.effects) }
      end
  | Llet ((Strict | Alias | StrictOpt) as let_kind, value_kind, id, def, body) ->
      let immutables = Ident.Set.add id letrec.immutables in
      let letrec = { letrec with immutables } in
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
      let free_vars = Lambda.free_variables lam1 in
      let letrec = prepare_letrec recursive_set current_let lam2 letrec in
      if Ident.Set.disjoint free_vars recursive_set then
        { letrec with pre = fun ~tail -> Lsequence (lam1, letrec.pre ~tail) }
      else
        (* Note that it is important not to handle this the same way as lets.
           XXX: TODO think about an explanation *)
        let letrec = prepare_letrec recursive_set None lam1 letrec in
        { letrec with effects = Lsequence (lam1, letrec.effects) }
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

      (* TODO lg: it's probably better to have this, but commented out atm so
         that it doesn't hide bugs in the more complex path
       * if Ident.Set.is_empty outer_vars then
       *   (\* Non recursive relative to top-level letrec, we can avoid dissecting it right now.
       *      Its turn will come later. *\)
       *   let letrec = prepare_letrec recursive_set current_let body letrec in
       *   let pre ~tail : Lambda.lambda =
       *     Lletrec (bindings, letrec.pre ~tail)
       *   in
       *   { letrec with pre }
       * else *)

      let letrec =
        { letrec with immutables =
                        Ident.Set.union letrec.immutables vars }
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
            debug "    [31m%a = %a[m %b@."
              Ident.print id
              Printlambda.lambda def
              (Ident.Set.mem id outer_vars);
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
                    functions; substitution; immutables } =
                letrec
              in
              let inner_letrec = {
                effects = Lconst (Const_pointer 0);
                functions = [];
                (* these can be safely handled in common *)
                blocks; consts; pre; substitution; immutables;
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

     (* let outer_bindings, non_rec_bindings =
      *   List.partition (fun (x, _) -> Ident.Set.mem x outer_vars) bindings
      * in
      * let letrec =
      *   List.fold_right (fun (id, def) letrec ->
      *       let let_def = {
      *         let_kind = Strict;
      *         value_kind = Pgenval;
      *         ident = id;
      *       } in
      *       prepare_letrec recursive_set (Some let_def) def letrec)
      *     outer_bindings
      *     letrec
      * in
      * let pre =
      *   match non_rec_bindings with
      *   | [] -> letrec.pre
      *   | bnd -> fun ~tail : Lambda.lambda -> Lletrec (bnd, letrec.pre ~tail)
      * in
      * { letrec with pre } *)



  | Lvar id when Ident.Set.mem id letrec.immutables ->
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
           debug "ADD SUBST: %a => %a@."
             Ident.print cl.ident Ident.print id;
           let immutables = Ident.Set.add cl.ident letrec.immutables in
           { letrec with substitution; immutables }
       | None -> letrec)

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
      (* CR vlaviron: This invariant is false when this expression is the result
         of a previously dissected term, which can contain calls to
         caml_update_dummy *)
      let free_vars = Lambda.free_variables lam in
      if not (Ident.Set.disjoint free_vars recursive_set) then begin
        debug "case %a@.%a@."
          Ident.Set.print
          (Ident.Set.inter free_vars recursive_set)
          Printlambda.lambda lam;
        (* raise Bug; *)
      end;
      (* assert(Ident.Set.is_empty (Ident.Set.inter free_vars recursive_set)); *)
      let pre = match current_let with
        | Some cl -> fun ~tail : Lambda.lambda ->
            Llet (cl.let_kind, cl.value_kind, cl.ident, lam, letrec.pre ~tail)
        | None -> fun ~tail : Lambda.lambda ->
            Lsequence (lam, letrec.pre ~tail)
      in
      { letrec with pre }

let dissect_letrec ~bindings ~body =

  debug "dissect@ %a@.@."
    Printlambda.lambda (Lletrec (bindings, Lconst (Const_pointer 0)));

  let recursive_set =
    Ident.Set.of_list (List.map fst bindings)
  in

  let letrec =
    List.fold_right (fun (id, def) letrec ->
        let let_def = {
          let_kind = Strict;
          value_kind = Pgenval;
          ident = id;
        } in
        prepare_letrec recursive_set (Some let_def) def letrec)
      bindings
      { blocks = [];
        consts = [];
        pre = (fun ~tail -> tail);
        effects = Lconst (Const_pointer 0);
        functions = [];
        substitution = Ident.Map.empty;
        immutables = recursive_set;
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

  (* let substituted =
   *   List.fold_left (fun body (id, _) ->
   *       Llet (Strict, Pgenval, id, Lconst (Const_pointer 99999), body))
   *     substituted bindings
   * in *)

  debug "dissected@ %a@.@."
    Printlambda.lambda substituted;
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


