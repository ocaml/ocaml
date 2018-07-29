(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Jeremy Yallop, University of Cambridge                   *)
(*                                                                        *)
(*   Copyright 2017 Jeremy Yallop                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Asttypes
open Typedtree
open Types

exception Illegal_expr

module Rec_context =
struct

  (** For an expression in a program, its "usage mode" represents
     static information about how the value produced by the expression
     will be used by the context around it. *)
  type mode =
    | Dereference
    (** A [Dereference] context consumes, inspects and uses the value
        in arbitrary ways. Such a value must be fully defined at the point
        of usage, it cannot be defined mutually-recursively with its context. *)

    | Return
    (** A [Return] context returns its value without further inspection.
        This value cannot be defined mutually-recursively with its context,
        as there is a risk of self-loop: in [let rec x = y and y = x], the
        two definitions use a single variable in [Return] context. *)

    | Guard
    (** A [Guard] context returns the value as a member of a data structure,
        for example a variant constructor or record. The value can safely be
        defined mutually-recursively with their context, for example in
        [let rec li = 1 :: li].
        When these subexpressions participate in a cyclic definition,
        this definition is productive/guarded.

        The [Guard] mode is also used when a value is not dereferenced,
        it is returned by a sub-expression, but the result of this
        sub-expression is discarded instead of being returned.
        For example, the subterm [?] is in a [Guard] context
        in [let _ = ? in e] and in [?; e].
        When these subexpressions participate in a cyclic definition,
        they cannot create a self-loop.
    *)

    | Delay
    (** A [Delay] context can be fully evaluated without evaluting its argument,
        which will only be needed at a later point of program execution. For
        example, [fun x -> ?] or [lazy ?] are [Delay] contexts. *)

    | Unused
    (** [Unused] is for subexpressions that are not used at all during
       the evaluation of the whole program. This is the mode of
       a variable in an expression in which it does not occur. *)

  (* Returns the most conservative mode of the two arguments.

     Deref < Return < Guard < Delay < Unused
  *)
  let prec m m' =
    match m, m' with
    | Dereference, _
    | _, Dereference -> Dereference
    | Return, _
    | _, Return -> Return
    | Guard, _
    | _, Guard -> Guard
    | Delay, _
    | _, Delay -> Delay
    | _ -> Unused

  (* If x is used with the mode m in e[x], and e[x] is used with mode
     m' in e'[e[x]], then x is used with mode m'[m] in e'[e[x]].

     Return is neutral for composition: m[Return] = m = Return[m].
  *)
  let compos m' m = match m', m with
    | Unused, _
    | _, Unused -> Unused
    | Dereference, _ -> Dereference
    | Delay, _ -> Delay
    | m', Return -> m'
    | Return, m
    | Guard, m -> m

  module Env :
  sig
    type t

    val single : Ident.t -> mode -> t
    (** Create an environment with a single identifier used with a given mode.
    *)

    val empty : t
    (** An environment with no used identifiers. *)

    val find : Ident.t -> t -> mode
    (** Find the mode of an indentifier in an environment.  The default mode is
        Unused. *)

    val unguarded : t -> Ident.t list -> Ident.t list
    (** unguarded e l: the list of all identifiers in e that are unguarded of
        dereferenced in the environment e. *)

    val dependent : t -> Ident.t list -> Ident.t list
    (** unguarded e l: the list of all identifiers in e that are used in e. *)

    val join : t -> t -> t
    val join_list : t list -> t

    val remove : Ident.t -> t -> t
    (* Remove an identifier from an environment. *)

    val take: Ident.t -> t -> mode * t
    (* Remove an identifier from an environment, and return its mode *)

    val remove_list : Ident.t list -> t -> t
    (* Remove all the identifiers of a list from an environment. *)
  end =
  struct
    module M = Map.Make(Ident)

    (** A "t" maps each rec-bound variable to an access status *)
    type t = mode M.t

    let find (id: Ident.t) (tbl: t) =
      try M.find id tbl with Not_found -> Unused

    let empty = M.empty

    let join (x: t) (y: t) =
      M.fold
        (fun (id: Ident.t) (v: mode) (tbl: t) ->
           let v' = find id tbl in
           M.add id (prec v v') tbl)
        x y

    let join_list li = List.fold_left join empty li

    let single id mode = M.add id mode empty

    let unguarded env li =
      let not_guarded = function
        | Dereference | Return -> true
        | Guard | Delay | Unused -> false
      in
      List.filter (fun id -> not_guarded (find id env)) li

    let dependent env li =
      let used = function
        | Dereference | Return | Guard | Delay -> true
        | Unused -> false
      in
      List.filter (fun id -> used (find id env)) li

    let remove = M.remove

    let take id env = (find id env, remove id env)

    let remove_list l env =
      List.fold_left (fun env id -> M.remove id env) env l
  end
end

open Rec_context

let is_ref : Types.value_description -> bool = function
  | { Types.val_kind =
        Types.Val_prim { Primitive.prim_name = "%makemutable";
                          prim_arity = 1 } } ->
        true
  | _ -> false

(* See the note on abstracted arguments in the documentation for
    Typedtree.Texp_apply *)
let is_abstracted_arg : arg_label * expression option -> bool = function
  | (_, None) -> true
  | (_, Some _) -> false

type sd = Static | Dynamic

let classify_expression : Typedtree.expression -> sd =
  (* We need to keep track of the size of expressions
      bound by local declarations, to be able to predict
      the size of variables. Compare:

        let rec r =
          let y = fun () -> r ()
          in y

      and

        let rec r =
          let y = if Random.bool () then ignore else fun () -> r ()
          in y

    In both cases the final adress of `r` must be known before `y` is compiled,
    and this is only possible if `r` has a statically-known size.

    The first definition can be allowed (`y` has a statically-known
    size) but the second one is unsound (`y` has no statically-known size).
  *)
  let rec classify_expression env e = match e.exp_desc with
    (* binding and variable cases *)
    | Texp_let (rec_flag, vb, e) ->
        let env = classify_value_bindings rec_flag env vb in
        classify_expression env e
    | Texp_ident (path, _, _) ->
        classify_path env path

    (* non-binding cases *)
    | Texp_letmodule (_, _, _, e)
    | Texp_sequence (_, e)
    | Texp_letexception (_, e) ->
        classify_expression env e

    | Texp_construct (_, {cstr_tag = Cstr_unboxed}, [e]) ->
        classify_expression env e
    | Texp_construct _ ->
        Static

    | Texp_record { representation = Record_unboxed _;
                    fields = [| _, Overridden (_,e) |] } ->
        classify_expression env e
    | Texp_record _ ->
        Static

    | Texp_apply ({exp_desc = Texp_ident (_, _, vd)}, _)
      when is_ref vd ->
        Static
    | Texp_apply (_,args)
      when List.exists is_abstracted_arg args ->
        Static
    | Texp_apply _ ->
        Dynamic

    | Texp_for _
    | Texp_constant _
    | Texp_new _
    | Texp_instvar _
    | Texp_tuple _
    | Texp_array _
    | Texp_variant _
    | Texp_setfield _
    | Texp_while _
    | Texp_setinstvar _
    | Texp_pack _
    | Texp_object _
    | Texp_function _
    | Texp_lazy _
    | Texp_unreachable
    | Texp_extension_constructor _ ->
        Static

    | Texp_match _
    | Texp_ifthenelse _
    | Texp_send _
    | Texp_field _
    | Texp_assert _
    | Texp_try _
    | Texp_override _ ->
        Dynamic
  and classify_value_bindings rec_flag env bindings =
    (* We use a non-recursive classification, classifying each
        binding with respect to the old environment
        (before all definitions), even if the bindings are recursive.

        Note: computing a fixpoint in some way would be more
        precise, as the following could be allowed:

          let rec topdef =
            let rec x = y and y = fun () -> topdef ()
            in x
    *)
    ignore rec_flag;
    let old_env = env in
    let add_value_binding env vb =
      match vb.vb_pat.pat_desc with
      | Tpat_var (id, _loc) ->
          let size = classify_expression old_env vb.vb_expr in
          Ident.add id size env
      | _ ->
          (* Note: we don't try to compute any size for complex patterns *)
          env
    in
    List.fold_left add_value_binding env bindings
  and classify_path env = function
    | Path.Pident x ->
        begin
          try Ident.find_same x env
          with Not_found ->
            (* an identifier will be missing from the map if either:
                - it is a non-local identifier
                  (bound outside the letrec-binding we are analyzing)
                - or it is bound by a complex (let p = e in ...) local binding
                - or it is bound within a module (let module M = ... in ...)
                  that we are not traversing for size computation

                For non-local identifiers it might be reasonable (although
                not completely clear) to consider them Static (they have
                already been evaluated), but for the others we must
                under-approximate with Dynamic.

                This could be fixed by a more complete implementation.
            *)
            Dynamic
        end
    | Path.Pdot _ | Path.Papply _ ->
        (* local modules could have such paths to local definitions;
            classify_expression could be extend to compute module
            shapes more precisely *)
        Dynamic
  in classify_expression Ident.empty

let remove_pat pat env =
  Env.remove_list (pat_bound_idents pat) env

let remove_patlist pats env =
  List.fold_right remove_pat pats env

(* Usage mode judgments.

   There are two main groups of judgment functions:

   - Judgments of the form "Gamma |- ... : m"
     compute the environment of a subterm from its mode,
     so the corresponding function has type [... -> mode -> Env.t].

     We write ['a -> term_judg] in this case.

   - Judgments of the form "Gamma |- ... : m -| Gamma'"
     correspond to binding constructs that have both an
     exterior environment (the environment of the whole term)
     and an interior environment (the environment after the binding
     construct has introduced new names in scope).

     For example, a toplevel let-binding could be given
     the following rule:

       G |- e : m + m'
       -------------------------------
       G |- (let x = e : m) -| x:m', G

     In this case, the corresponding function has type
     [... -> mode -> Env.t -> Env.t]: it takes the mode
     and interior environment as inputs, and returns
     the exterior environment.

     We write ['a -> bind_judg] in this case.
*)
type term_judg = mode -> Env.t
type bind_judg = mode -> Env.t -> Env.t

let option : 'a. ('a -> term_judg) -> 'a option -> term_judg =
  fun f o m -> match o with
    | None -> Env.empty
    | Some v -> f v m
let list : 'a. ('a -> term_judg) -> 'a list -> term_judg =
  fun f li m ->
    List.fold_left (fun env item -> Env.join env (f item m)) Env.empty li
let array : 'a. ('a -> term_judg) -> 'a array -> term_judg =
  fun f ar m ->
    Array.fold_left (fun env item -> Env.join env (f item m)) Env.empty ar

let rec expression : Typedtree.expression -> term_judg =
  fun exp mode -> match exp.exp_desc with
    | Texp_ident (pth, _, _) ->
      path pth mode
    | Texp_let (rec_flag, bindings, body) ->
      value_bindings rec_flag bindings mode (expression body mode)
    | Texp_letmodule (x, _, mexp, e) ->
      module_binding (x, mexp) mode (expression e mode)
    | Texp_match (e, cases, _) ->
      let env_pat, m_e =
        List.fold_left
          (fun (env, m) c ->
            let (env', m') = case c mode in
            (Env.join env env'), (prec m m'))
          (Env.empty, Unused)
          cases
      in
      let env_e = expression e m_e in
      Env.join env_pat env_e
    | Texp_for (_, _, low, high, _, body) ->
      (*
        G1 |- low: m[Dereference]
        G2 |- high: m[Dereference]
        G3 |- body: m[Guard]
        ---
        G1 + G2 + G3 |- for _ = low to high do body done: m
      *)
      Env.join_list [
        expression low (compos mode Dereference);
        expression high (compos mode Dereference);
        expression body (compos mode Guard);
      ]
    | Texp_constant _ ->
      Env.empty
    | Texp_new (pth, _, _) ->
      (*
        G |- c: m[Dereference]
        -----------------------
        G |- new c: m
      *)
      path pth (compos mode Dereference)
    | Texp_instvar (self_path, pth, _inst_var) ->
        Env.join
          (path self_path (compos mode Dereference))
          (path pth mode)
    | Texp_apply ({exp_desc = Texp_ident (_, _, vd)}, [_, Some arg])
      when is_ref vd ->
      (*
        G |- e: m[Guard]
        ------------------
        G |- ref e: m
      *)
      expression arg (compos mode Guard)
    | Texp_apply (e, args)  ->
        let arg (_, eo) m = option expression eo m in
        let m' = if List.exists is_abstracted_arg args
          then (* see the comment on Texp_apply in typedtree.mli;
                  the non-abstracted arguments are bound to local
                  variables, which corresponds to a Guard mode. *)
            compos mode Guard
          else compos mode Dereference
        in
        Env.join (list arg args m') (expression e m')
    | Texp_tuple exprs ->
      list expression exprs (compos mode Guard)
    | Texp_array exprs ->
      let array_mode = match Typeopt.array_kind exp with
        | Lambda.Pfloatarray ->
            (* (flat) float arrays unbox their elements *)
            Dereference
        | Lambda.Pgenarray ->
            (* This is counted as a use, because constructing a generic array
               involves inspecting to decide whether to unbox (PR#6939). *)
            Dereference
        | Lambda.Paddrarray | Lambda.Pintarray ->
            (* non-generic, non-float arrays act as constructors *)
            Guard
      in
      list expression exprs (compos mode array_mode)
    | Texp_construct (_, desc, exprs) ->
      let access_constructor =
        match desc.cstr_tag with
        | Cstr_extension (pth, _) ->
          path pth (compos mode Dereference)
        | _ -> Env.empty
      in
      let m' = match desc.cstr_tag with
        | Cstr_unboxed ->
          compos mode Return
        | Cstr_constant _ | Cstr_block _ | Cstr_extension _ ->
          compos mode Guard
      in
      Env.join access_constructor (list expression exprs m')
    | Texp_variant (_, eo) ->
      (*
        G |- e: m[Guard]
        ------------------   -----------
        G |- `A e: m         [] |- `A: m
      *)
      option expression eo (compos mode Guard)
    | Texp_record { fields = es; extended_expression = eo;
                    representation = rep } ->
        let mfield = match rep with
          | Record_float -> Dereference
          | Record_unboxed _ -> Return
          | Record_regular | Record_inlined _
          | Record_extension -> Guard
        in
        let field (_label, field_def) m = match field_def with
            Kept _ -> Env.empty
          | Overridden (_, e) -> expression e m
        in
        Env.join (array field es (compos mode mfield))
                 (option expression eo (compos mode Dereference))
    | Texp_ifthenelse (cond, ifso, ifnot) ->
      (*
        Gc |- c: m[Dereference]
        G1 |- e1: m
        G2 |- e2: m
        ---
        Gc + G1 + G2 |- if c then e1 else e2: m

      Note: `if c then e1 else e2` is treated in the same way as
      `match c with true -> e1 | false -> e2`
      *)
      Env.join_list [
        expression cond (compos mode Dereference);
        expression ifso mode;
        option expression ifnot mode;
      ]
    | Texp_setfield (e1, _, _, e2) ->
      (*
        G1 |- e1: m[Dereference]
        G2 |- e2: m[Dereference] (*TODO: why is this a dereference?*)
        ---
        G1 + G2 |- e1.x <- e2: m
      *)
      let env_1 = expression e1 (compos mode Dereference) in
      let env_2 = expression e2 (compos mode Dereference) in
      Env.join env_1 env_2
    | Texp_sequence (e1, e2) ->
      (*
        G1 |- e1: m[Guard]
        G2 |- e2: m
        --------------------
        G1 + G2 |- e1; e2: m

        Note: `e1; e2` is treated in the same way as `let _ = e1 in e2`
      *)
      let env1 = expression e1 (compos mode Guard) in
      let env2 = expression e2 mode in
      Env.join env1 env2
    | Texp_while (e1, e2) ->
      (*
        G1 |- cond: m[Dereference]
        G2 |- body: m[Guard]
        ---------------------------------
        G1 + G2 |- while cond do body done: m
      *)
      let env_1 = expression e1 (compos mode Dereference) in
      let env_2 = expression e2 (compos mode Guard) in
      Env.join env_1 env_2
    | Texp_send (e1, _, eo) ->
      (*
        G |- e: m[Dereference]
        ---------------------- (plus weird 'eo' option)
        G |- e#x: m
      *)
      Env.join (expression e1 (compos mode Dereference))
               (option expression eo (compos mode Dereference))
    | Texp_field (e, _, _) ->
      (*
        G |- e: m[Dereference]
        -----------------------
        G |- e.x: m
      *)
      expression e (compos mode Dereference)
    | Texp_setinstvar (pth,_,_,e) ->
      (*
        G |- e: m[Dereference]  (*TODO: why is this a dereference?*)
        -----------------------
        G |- x <- e: m
      *)
      Env.join
        (path pth (compos mode Dereference))
        (expression e (compos mode Dereference))
    | Texp_letexception ({ext_id}, e) ->
      (* G |- e: m
         ----------------------------
         G |- let exception A in e: m
      *)
      Env.remove ext_id (expression e mode)
    | Texp_assert e ->
      (*
        G |- e: m[Dereference]
        -----------------------
        G |- assert e: m

        Note: `assert e` is treated just as if `assert` was a function.
      *)
      expression e (compos mode Dereference)
    | Texp_pack mexp ->
      (*
        G |- M: m
        ----------------
        G |- module M: m
      *)
      modexp mexp mode
    | Texp_object (clsstrct, _) ->
      class_structure clsstrct mode
    | Texp_try (e, cases) ->
      (*
        G |- e: m
        G1 |- e1: m
        ...
        Gn |- en: m
        ---
        G + G1 + ... + Gn |- try e with p1 -> e1 | ... | pn -> en: m
      *)
      let case {Typedtree.c_rhs} m = expression c_rhs m in
      Env.join (expression e mode) (list case cases mode)
    | Texp_override (pth, fields) ->
      let field (_, _, arg) m = expression arg m in
      let m' = compos mode Dereference in
      Env.join (path pth m') (list field fields m')
    | Texp_function { cases } ->
      (* Approximation: function `p1 -> e1, ..., pn -> en` is the same as
         `fun x -> match x with p1 -> e1, ..., pn -> en`.

         The typing of this expression is nearly the same as the typing of
         a `match` expression, the differences are:
         - e1, ..., en are evaluated in the m[Delay] mode instead of m
         - we don't care about the mode returned by the `case` function.
      *)
      list (fun c m -> fst (case c m)) cases (compos mode Delay)
    | Texp_lazy e ->
      (*
        G |- e:
        ---
        ... |- lazy e: m
      *)
      let m' = begin match Typeopt.classify_lazy_argument e with
        | `Constant_or_function
        | `Identifier _
        | `Float_that_cannot_be_shortcut ->
          mode
        | `Other ->
          compos mode Delay
        end
      in
      expression e m'
    | Texp_unreachable ->
      (*
        ----------
        [] |- .: m
      *)
      Env.empty
    | Texp_extension_constructor (_lid, pth) ->
      path pth (compos mode Dereference)

and class_structure : Typedtree.class_structure -> term_judg =
  fun cs m -> list class_field cs.cstr_fields m

and class_field : Typedtree.class_field -> term_judg =
  fun cf m -> match cf.cf_desc with
    | Tcf_inherit (_, ce, _super, _inh_vars, _inh_meths) ->
      class_expr ce (compos m Dereference)
    | Tcf_val (_lab, _mut, _, cfk, _) ->
      class_field_kind cfk m
    | Tcf_method (_, _, cfk) ->
      class_field_kind cfk m
    | Tcf_constraint _ ->
      Env.empty
    | Tcf_initializer e ->
      expression e (compos m Dereference)
    | Tcf_attribute _ ->
      Env.empty

and class_field_kind : Typedtree.class_field_kind -> term_judg =
  fun cfk m -> match cfk with
    | Tcfk_virtual _ ->
      Env.empty
    | Tcfk_concrete (_, e) ->
      expression e (compos m Dereference)

and modexp : Typedtree.module_expr -> term_judg =
  fun mexp mode -> match mexp.mod_desc with
    | Tmod_ident (pth, _) ->
      path pth mode
    | Tmod_structure s ->
      structure s mode
    | Tmod_functor (_, _, _, e) ->
      modexp e (compos mode Delay)
    | Tmod_apply (f, p, _) ->
      let m' = compos mode Dereference in
      Env.join (modexp f m') (modexp p m')
    | Tmod_constraint (mexp, _, _, coe) ->
      let rec coercion k = function
        | Tcoerce_none ->
          k Return
        | Tcoerce_structure _
        | Tcoerce_functor _ ->
          (* These coercions perform a shallow copy of the input module,
             by creating a new module with fields obtained by accessing
             the same fields in the input module. *)
           k Dereference
        | Tcoerce_primitive _ ->
          (* This corresponds to 'external' declarations,
             and the coercion ignores its argument *)
          k Unused
        | Tcoerce_alias (pth, coe) ->
          (* Alias coercions ignore their arguments, but they evaluate
             their alias module 'pth' under another coercion. *)
          coercion (fun m -> path pth (compos mode m)) coe
      in
      coercion (fun m -> modexp mexp (compos mode m)) coe
    | Tmod_unpack (e, _) ->
      expression e mode

and path : Path.t -> term_judg =
  (*
    ------------
    x: m |- x: m

    G |- A: m[Dereference]
    -----------------------
    G |- A.x: m

    G1 |- A: m[Dereference]
    G2 |- B: m[Dereference]
    ------------------------ (as for term application)
    G1 + G2 |- A(B): m
  *)
  fun pth mode -> match pth with
    | Path.Pident x ->
        Env.single x mode
    | Path.Pdot (t, _, _) ->
        path t (compos mode Dereference)
    | Path.Papply (f, p) ->
        let m = compos mode Dereference in
        Env.join (path f m) (path p m)

and structure : Typedtree.structure -> term_judg =
  (*
    G1, {x: _, x in vars(G1)} |- item1: G2 + ... + Gn in m
    G2, {x: _, x in vars(G2)} |- item2: G3 + ... + Gn in m
    ...
    Gn, {x: _, x in vars(Gn)} |- itemn: [] in m
    ---
    (G1 + ... + Gn) - V |- struct item1 ... itemn end: m
  *)
  fun s m ->
    List.fold_right (fun it env -> structure_item it m env)
      s.str_items Env.empty

and structure_item : Typedtree.structure_item -> bind_judg =
  fun s m env -> match s.str_desc with
    | Tstr_eval (e, _) ->
      (*
        G |- e: m[Guard]
        G' |- struct {items} end: m
        ------------------------------------
        G + G' |- struct e: m {items} end: m

        The expression `e` is treated in the same way as let _ = e
      *)
      Env.join env (expression e (compos m Guard))
    | Tstr_value (rec_flag, bindings) ->
      value_bindings rec_flag bindings m env
    | Tstr_module {mb_id; mb_expr} ->
      module_binding (mb_id, mb_expr) m env
    | Tstr_recmodule mbs ->
      let bindings = List.map (fun {mb_id; mb_expr} -> (mb_id, mb_expr)) mbs in
      recursive_module_bindings bindings m env
    | Tstr_primitive _ ->
      env
    | Tstr_type _ ->
      (*
        ---------------
        [] |- type t: m
      *)
      env
    | Tstr_typext {tyext_constructors = exts; _} ->
      let ext_ids = List.map (fun {ext_id = id; _} -> id) exts in
      Env.join
        (list extension_constructor exts m)
        (Env.remove_list ext_ids env)
    | Tstr_exception {tyexn_constructor = ext; _} ->
      Env.join
        (extension_constructor ext m)
        (Env.remove ext.ext_id env)
    | Tstr_modtype _
    | Tstr_class_type _
    | Tstr_attribute _ ->
      env
    | Tstr_open _ ->
      (* TODO: open introduces term/module variables in scope,
         we could/should remove them from the environment.

         See also Texp_open (in exp_extra, outside the normal matching path)
         and Tcl_open. *)
      env
    | Tstr_class classes ->
        let class_ids =
          let class_id ({ci_id_class = id; _}, _) = id in
          List.map class_id classes in
        let class_declaration ({ci_expr; _}, _) m =
          Env.remove_list class_ids (class_expr ci_expr m) in
        Env.join
          (list class_declaration classes m)
          (Env.remove_list class_ids env)
    | Tstr_include { incl_mod = mexp; incl_type = mty; _ } ->
      let included_ids =
        let sigitem_id = function
          | Sig_value (id, _)
          | Sig_type (id, _, _)
          | Sig_typext (id, _, _)
          | Sig_module (id, _, _)
          | Sig_modtype (id, _)
          | Sig_class (id, _, _)
          | Sig_class_type (id, _, _)
            -> id
        in
        List.map sigitem_id mty in
      Env.join (modexp mexp m) (Env.remove_list included_ids env)

and module_binding : (Ident.t * Typedtree.module_expr) -> bind_judg =
  fun (id, mexp) m env ->
      (*
        GE |- E: m[mM + Guard]
        -------------------------------------
        GE + G |- module M = E : m -| M:mM, G
      *)
      let mM, env = Env.take id env in
      Env.join (modexp mexp (compos m (prec mM Guard))) env

and recursive_module_bindings : (Ident.t * Typedtree.module_expr) list -> bind_judg =
  fun m_bindings m env ->
    let mids = List.map fst m_bindings in
    let binding (mid, mexp) m =
      let mM = Env.find mid env in
      Env.remove_list mids (modexp mexp (compos m (prec mM Guard)))
    in
    Env.join (list binding m_bindings m) (Env.remove_list mids env)

and class_expr : Typedtree.class_expr -> term_judg =
  fun ce m -> match ce.cl_desc with
    | Tcl_ident (pth, _, _) ->
        path pth (compos m Dereference)
    | Tcl_structure cs ->
        class_structure cs m
    | Tcl_fun (_, _, args, ce, _) ->
        let ids = List.map fst args in
        Env.remove_list ids (class_expr ce (compos m Delay))
    | Tcl_apply (ce, args) ->
        let m' = compos m Dereference in
        let arg (_label, eo) m = option expression eo m in
        Env.join (class_expr ce m') (list arg args m')
    | Tcl_let (rec_flag, bindings, _, ce) ->
      value_bindings rec_flag bindings m (class_expr ce m)
    | Tcl_constraint (ce, _, _, _, _) ->
        class_expr ce m
    | Tcl_open (_, _, _, _, ce) ->
        class_expr ce m

and extension_constructor : Typedtree.extension_constructor -> term_judg =
  fun ec m -> match ec.ext_kind with
    | Text_decl _ ->
      Env.empty
    | Text_rebind (pth, _lid) ->
      path pth m

and value_bindings : rec_flag -> Typedtree.value_binding list -> bind_judg =
  fun rec_flag bindings mode bound_env ->
    (*
       (Gi |- ei : m[mi])^i       (pi : mi -| D)^i
       G := sum(Gi - if (rec) then (pj)^j else pi)^i
       -------------------------------------------------
       G + (D - (pj)^j) |- let (rec)? (pi=ei)^i : m -| D
    *)
    let all_bound_pats = List.map (fun vb -> vb.vb_pat) bindings in
    let binding_env {vb_pat; vb_expr; _} m =
      let bound_pats = match rec_flag with
        | Recursive -> all_bound_pats
        | Nonrecursive -> [vb_pat] in
      let m' = compos m (pattern vb_pat bound_env) in
      remove_patlist bound_pats (expression vb_expr m') in
    Env.join
      (list binding_env bindings mode)
      (remove_patlist all_bound_pats bound_env)

and case : Typedtree.case -> mode -> Env.t * mode =
  fun { Typedtree.c_lhs; c_guard; c_rhs } m ->
    (*
       Ge |- e : m    Gg |- g : m[Dereference]
       G := Ge+Gg     p : mp -| G
       ----------------------------------------
       G - p; m[mp] |- (p (when g)? -> e) : m
    *)
    let env_guard = option expression c_guard (compos m Dereference) in
    let env_rhs = expression c_rhs m in
    let env = Env.join env_guard env_rhs in
    (remove_pat c_lhs env), compos m (pattern c_lhs env)

and pattern : pattern -> Env.t -> mode = fun pat env ->
  (*
    mp := | Dereference if p is destructuring
          | Guard       otherwise
    me := sum{G(x), x in vars(p)}
    --------------------------------------------
    p : (mp + me) -| G
  *)
  let m_pat = if is_destructuring_pattern pat
              then Dereference
              else Guard
  in
  let m_env =
    pat_bound_idents pat
    |> List.map (fun id -> Env.find id env)
    |> List.fold_left prec Unused
  in
  prec m_pat m_env

and is_destructuring_pattern : Typedtree.pattern -> bool =
  fun pat -> match pat.pat_desc with
    | Tpat_any -> false
    | Tpat_var (_id, _) -> false
    | Tpat_alias (pat, _, _) ->
        is_destructuring_pattern pat
    | Tpat_constant _ -> true
    | Tpat_tuple _ -> true
    | Tpat_construct (_, _, _) -> true
    | Tpat_variant _ -> true
    | Tpat_record (_, _) -> true
    | Tpat_array _ -> true
    | Tpat_or (l,r,_) ->
        is_destructuring_pattern l || is_destructuring_pattern r
    | Tpat_lazy _ -> true
    | Tpat_exception _ -> false

let is_valid_recursive_expression idlist expr =
  let ty = expression expr Return in
  match Env.unguarded ty idlist, Env.dependent ty idlist,
        classify_expression expr with
  | _ :: _, _, _ (* The expression inspects rec-bound variables *)
  | _, _ :: _, Dynamic -> (* The expression depends on rec-bound variables
                              and its size is unknown *)
      false
  | [], _, Static (* The expression has known size *)
  | [], [], Dynamic -> (* The expression has unknown size,
                          but does not depend on rec-bound variables *)
      true

let is_valid_class_expr idlist ce =
  let rec class_expr : mode -> Typedtree.class_expr -> Env.t =
    fun mode ce -> match ce.cl_desc with
      | Tcl_ident (_, _, _) ->
        (*
          ----------
          [] |- a: m
        *)
        Env.empty
      | Tcl_structure _ ->
        (*
          -----------------------
          [] |- struct ... end: m
        *)
        Env.empty
      | Tcl_fun (_, _, _, _, _) -> Env.empty
        (*
          ---------------------------
          [] |- fun x1 ... xn -> C: m
        *)
      | Tcl_apply (_, _) -> Env.empty
      | Tcl_let (rec_flag, bindings, _, ce) ->
        value_bindings rec_flag bindings mode (class_expr mode ce)
      | Tcl_constraint (ce, _, _, _, _) ->
        class_expr mode ce
      | Tcl_open (_, _, _, _, ce) ->
        class_expr mode ce
  in
  match Env.unguarded (class_expr Return ce) idlist with
  | [] -> true
  | _ :: _ -> false
