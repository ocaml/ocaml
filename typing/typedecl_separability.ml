(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*   Gabriel Scherer, projet Parsifal, INRIA Saclay                       *)
(*   Rodolphe Lepigre, projet Deducteam, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type type_definition = type_declaration
(* We should use 'declaration' for interfaces, and 'definition' for
   implementations. The name type_declaration in types.ml is improper
   for our usage -- although for OCaml types the declaration and
   definition languages are the same. *)

(** assuming that a datatype has a single constructor/label with
   a single argument, [argument_to_unbox] represents the
   information we need to check the argument for separability. *)
type argument_to_unbox = {
  argument_type: type_expr;
  result_type_parameter_instances: type_expr list;
  (** result_type_parameter_instances represents the domain of the
     constructor; usually it is just a list of the datatype parameter
     ('a, 'b, ...), but when using GADTs or constraints it could
     contain arbitrary type expressions.

     For example, [type 'a t = 'b constraint 'a = 'b * int] has
     [['b * int]] as [result_type_parameter_instances], and so does
     [type _ t = T : 'b -> ('b * int) t]. *)
}

(** Summarize the right-hand-side of a type declaration,
    for separability-checking purposes. See {!structure} below. *)
type type_structure =
  | Synonym of type_expr
  | Abstract
  | Open
  | Algebraic
  | Unboxed of argument_to_unbox

let structure : type_definition -> type_structure = fun def ->
  match def.type_kind with
  | Type_open -> Open
  | Type_abstract ->
      begin match def.type_manifest with
      | None -> Abstract
      | Some type_expr -> Synonym type_expr
      end

  | ( Type_record ([{ld_type = ty; _}], Record_unboxed _)
    | Type_variant ([{cd_args = Cstr_tuple [ty]; _}], Variant_unboxed)
    | Type_variant ([{cd_args = Cstr_record [{ld_type = ty; _}]; _}],
                    Variant_unboxed)) ->
     let params =
       match def.type_kind with
       | Type_variant ([{cd_res = Some ret_type}], _) ->
          begin match get_desc ret_type with
          | Tconstr (_, tyl, _) -> tyl
          | _ -> assert false
          end
       | _ -> def.type_params
     in
     Unboxed { argument_type = ty; result_type_parameter_instances = params }

  | Type_record _ | Type_variant _ -> Algebraic

type error =
  | Non_separable_evar of string option

exception Error of Location.t * error

(* see the .mli file for explanations on the modes *)
module Sep = Types.Separability
type mode = Sep.t = Ind | Sep | Deepsep

let rank = Sep.rank
let max_mode = Sep.max

(** If the type context [e(_)] imposes the mode [m] on its hole [_],
    and the type context [e'(_)] imposes the mode [m'] on its hole [_],
    then the mode on [_] imposed by the context composition [e(e'(_))]
    is [compose m m'].

    This operation differs from [max_mode]: [max_mode Ind Sep] is [Sep],
    but [compose Ind Sep] is [Ind]. *)
let compose
  : mode -> mode -> mode
  = fun m1 m2 ->
  match m1 with
  | Deepsep -> Deepsep
  | Sep -> m2
  | Ind -> Ind

type type_var = {
  text: string option; (** the user name of the type variable, None for '_' *)
  id: int; (** the identifier of the type node (type_expr.id) of the variable *)
}

module TVarMap = Map.Make(struct
    type t = type_var
    let compare v1 v2 = compare v1.id v2.id
  end)
type context = mode TVarMap.t
let (++) = TVarMap.union (fun _ m1 m2 -> Some(max_mode m1 m2))
let empty = TVarMap.empty


(** [immediate_subtypes ty] returns the list of all the
   immediate sub-type-expressions of [ty]. They represent the biggest
   sub-components that may be extracted using a constraint. For
   example, the immediate sub-type-expressions of [int * (bool * 'a)]
   are [int] and [bool * 'a].

   Smaller components are extracted recursively in [check_type]. *)
let rec immediate_subtypes : type_expr -> type_expr list = fun ty ->
  (* Note: Btype.fold_type_expr is not suitable here:
     - it does not do the right thing on Tpoly, iterating on type
       parameters as well as the subtype
     - it performs a shallow traversal of object types,
       while our implementation collects all method types *)
  match get_desc ty with
  (* these are the important cases,
     on which immediate_subtypes is called from [check_type] *)
  | Tarrow(_,ty1,ty2,_) ->
      [ty1; ty2]
  | Ttuple(tys) -> tys
  | Tpackage(_, fl) -> (snd (List.split fl))
  | Tobject(row,class_ty) ->
      let class_subtys =
        match !class_ty with
        | None        -> []
        | Some(_,tys) -> tys
      in
      immediate_subtypes_object_row class_subtys row
  | Tvariant(row) ->
      immediate_subtypes_variant_row [] row

  (* the cases below are not called from [check_type],
     they are here for completeness *)
  | Tnil | Tfield _ ->
      (* these should only occur under Tobject and not at the toplevel,
         but "better safe than sorry" *)
      immediate_subtypes_object_row [] ty
  | Tlink _ | Tsubst _ -> assert false (* impossible due to Ctype.repr *)
  | Tvar _ | Tunivar _ -> []
  | Tpoly (pty, _) -> [pty]
  | Tconstr (_path, tys, _) -> tys

and immediate_subtypes_object_row acc ty = match get_desc ty with
  | Tnil -> acc
  | Tfield (_label, _kind, ty, rest) ->
      let acc = ty :: acc in
      immediate_subtypes_object_row acc rest
  | _ -> ty :: acc

and immediate_subtypes_variant_row acc desc =
  let add_subtypes acc =
    let add_subtype acc (_l, rf) =
      immediate_subtypes_variant_row_field acc rf in
    List.fold_left add_subtype acc (row_fields desc) in
  let add_row acc =
    let row = row_more desc in
    match get_desc row with
    | Tvariant more -> immediate_subtypes_variant_row acc more
    | _ -> row :: acc
  in
  add_row (add_subtypes acc)

and immediate_subtypes_variant_row_field acc f =
  match row_field_repr f with
  | Rpresent(None)
  | Rabsent            -> acc
  | Rpresent(Some(ty)) -> ty :: acc
  | Reither(_,field_types,_) ->
      List.rev_append field_types acc

let free_variables ty =
  Ctype.free_variables ty
  |> List.map (fun ty ->
      match get_desc ty with
        Tvar text -> {text; id = get_id ty}
      | _ ->
          (* Ctype.free_variables only returns Tvar nodes *)
          assert false)

(** Coinductive hypotheses to handle equi-recursive types

    OCaml allows infinite/cyclic types, such as
      (int * 'a) as 'a
    whose infinite unfolding is (int * (int * (int * (int * ...)))).

    Remark: this specific type is only accepted if the -rectypes option
    is passed, but such "equi-recursive types" are accepted by
    default if the cycle goes through an object type or polymorphic
    variant type:
      [ `int | `other of 'a ] as 'a
      < head : int; rest : 'a > as 'a

    We have to take those infinite types in account in our
    separability-checking program: a naive implementation would loop
    infinitely when trying to prove that one of them is Deepsep.

    After type-checking, the cycle-introducing form (... as 'a) does
    not appear explicitly in the syntax of types: types are graphs/trees
    with cycles in them, and we have to use the type_expr.id field,
    an identifier for each node in the graph/tree, to detect cycles.

    We avoid looping by remembering the set of separability queries
    that we have already asked ourselves (in the current
    search branch). For example, if we are asked to check

      (int * 'a) : Deepsep

    our algorithm will check both (int : Deepsep) and ('a : Deepsep),
    but it will remember in these sub-checks that it is in the process
    of checking (int * 'a) : Deepsep, adding it to a list of "active
    goals", or "coinductive hypotheses".

    Each new sub-query will start by checking whether the query
    already appears as a coinductive hypothesis; in our example, this
    can happen if 'a and (int * 'a) are in fact the same node in the
    cyclic tree. In that case, we return immediately (instead of looping):
    we reason that, assuming that 'a is indeed Deepsep, then it is
    the case that (int * 'a) is also Deepsep.

    This kind of cyclic reasoning can be dangerous: it would be wrong
    to argue that an arbitrary 'a type is Deepsep by saying:
    "assuming that 'a is Deepsep, then it is the case that 'a is
    also Deepsep". In the first case, we made an assumption on 'a,
    and used it on a type (int * 'a) which has 'a as a strict sub-component;
    in the second, we use it on the same type 'a directly, which is invalid.

    Now consider a type of the form (('a t) as 'a): while 'a is a sub-component
    of ('a t), it may still be wrong to reason coinductively about it,
    as ('a t) may be defined as (type 'a t = 'a).

    When moving from (int * 'a) to a subcomponent (int) or ('a), we
    say that the coinductive hypothesis on (int * 'a : m) is "safe":
    it can be used immediately to prove the subcomponents, because we
    made progress moving to a strict subcomponent (we are guarded
    under a computational type constructor). On the other hand, when
    moving from ('a t) to ('a), we say that the coinductive hypothesis
    ('a t : m) is "unsafe" for the subgoal, as we don't know whether
    we have made strict progress. In the general case, we keep track
    of a set of safe and unsafe hypotheses made in the past, and we
    use them to terminate checking if we encounter them again,
    ensuring termination.

    If we encounter a (ty : m) goal that is exactly a safe hypothesis,
    we terminate with a success. In fact, we can use mode subtyping here:
    if (ty : m') appears as a hypothesis with (m' >= m), then we would
    succeed for (ty : m'), so (ty : m) should succeed as well.

    On the other hand, if we encounter a (ty : m) goal that is an
    *unsafe* hypothesis, we terminate the check with a failure. In this case,
    we cannot work modulo mode subtyping: if (ty : m') appears with
    (m' >= m), then the check (ty : m') would have failed, but it is still
    possible that the weaker current query (ty : m) would succeed.

    In usual coinductive-reasoning systems, unsafe hypotheses are turned
    into safe hypotheses each time strict progress is made (for each
    guarded sub-goal). Consider ((int * 'a) t as 'a : deepsep) for example:
    the idea is that the ((int * 'a) t : deepsep) hypothesis would be
    unsafe when checking ((int * 'a) : deepsep), but that the progress
    step from (int * 'a : deepsep) to ('a : deepsep) would turn all
    past unsafe hypotheses into safe hypotheses. There is a problem
    with this, though, due to constraints: what if (_ t) is defined as

      type 'b t = 'a constraint 'b = (int * 'a)

    ?

    In that case, then 'a is precisely the one-step unfolding
    of the ((int * 'a) t) definition, and it would be an invalid,
    cyclic reasoning to prove ('a : deepsep) from the now-safe
    hypothesis ((int * 'a) t : deepsep).

    Surprisingly-fortunately, we have exactly the information we need
    to know whether (_ t) may or may not pull a constraint trick of
    this nature: we can look at its mode signature, where constraints
    are marked by a Deepsep mode. If we see Deepsep, we know that a
    constraint exists, but we don't know what the constraint is:
    we cannot tell at which point, when decomposing the parameter type,
    a sub-component can be considered safe again. To model this,
    we add a third category of co-inductive hypotheses: to "safe" and
    "unsafe" we add the category of "poison" hypotheses, which remain
    poisonous during the remaining of the type decomposition,
    even in presence of safe, computational types constructors:

    - when going under a computational constructor,
      "unsafe" hypotheses become "safe"
    - when going under a constraining type (more precisely, under
      a type parameter that is marked Deepsep in the mode signature),
      "unsafe" hypotheses become "poison"

    The mode signature tells us even a bit more: if a parameter
    is marked "Ind", we know that the type constructor cannot unfold
    to this parameter (otherwise it would be Sep), so going under
    this parameter can be considered a safe/guarded move: if
    we have to check (foo t : m) with ((_ : Ind) t) in the signature,
    we can recursively check (foo : Ind) with (foo t : m) marked
    as "safe", rather than "unsafe".
*)
module TypeMap = Btype.TypeMap
module ModeSet = Set.Make(Types.Separability)

type coinductive_hyps = {
  safe: ModeSet.t TypeMap.t;
  unsafe: ModeSet.t TypeMap.t;
  poison: ModeSet.t TypeMap.t;
}

module Hyps : sig
  type t = coinductive_hyps
  val empty : t
  val add : type_expr -> mode -> t -> t
  val guard : t -> t
  val poison : t -> t
  val safe : type_expr -> mode -> t -> bool
  val unsafe : type_expr -> mode -> t -> bool
end = struct
  type t = coinductive_hyps

  let empty = {
    safe = TypeMap.empty;
    unsafe = TypeMap.empty;
    poison = TypeMap.empty;
  }

  let of_opt = function
    | Some ms -> ms
    | None -> ModeSet.empty

  let merge map1 map2 =
    TypeMap.merge (fun _k ms1 ms2 ->
        Some (ModeSet.union (of_opt ms1) (of_opt ms2))
      ) map1 map2

  let guard {safe; unsafe; poison;} = {
    safe = merge safe unsafe;
    unsafe = TypeMap.empty;
    poison;
  }

  let poison {safe; unsafe; poison;} = {
    safe;
    unsafe = TypeMap.empty;
    poison = merge poison unsafe;
  }

  let add ty m hyps =
    let m_map = TypeMap.singleton ty (ModeSet.singleton m) in
    { hyps with unsafe = merge m_map hyps.unsafe; }

  let find ty map = try TypeMap.find ty map with Not_found -> ModeSet.empty

  let safe ty m hyps =
    match ModeSet.max_elt_opt (find ty hyps.safe) with
    | None -> false
    | Some best_safe -> rank best_safe >= rank m

  let unsafe ty m {safe = _; unsafe; poison} =
    let in_map s = ModeSet.mem m (find ty s) in
    List.exists in_map [unsafe; poison]
end

(** For a type expression [ty] (without constraints and existentials),
    any mode checking [ty : m] is satisfied in the "worse case" context
    that maps all free variables of [ty] to the most demanding mode,
    Deepsep. *)
let worst_case ty =
  let add ctx tvar = TVarMap.add tvar Deepsep ctx in
  List.fold_left add TVarMap.empty (free_variables ty)


(** [check_type env sigma ty m] returns the most permissive context [gamma]
    such that [ty] is separable at mode [m] in [gamma], under
    the signature [sigma]. *)
let check_type
  : Env.t -> type_expr -> mode -> context
  = fun env ty m ->
  let rec check_type hyps ty m =
    if Hyps.safe ty m hyps then empty
    else if Hyps.unsafe ty m hyps then worst_case ty
    else
    let hyps = Hyps.add ty m hyps in
    match (get_desc ty, m) with
    (* Impossible case due to the call to [Ctype.repr]. *)
    | (Tlink _            , _      ) -> assert false
    (* Impossible case (according to comment in [typing/types.mli]. *)
    | (Tsubst(_)          , _      ) -> assert false
    (* "Indifferent" case, the empty context is sufficient. *)
    | (_                  , Ind    ) -> empty
    (* Variable case, add constraint. *)
    | (Tvar(alpha)        , m      ) ->
        TVarMap.singleton {text = alpha; id = get_id ty} m
    (* "Separable" case for constructors with known memory representation. *)
    | (Tarrow _           , Sep    )
    | (Ttuple _           , Sep    )
    | (Tvariant(_)        , Sep    )
    | (Tobject(_,_)       , Sep    )
    | ((Tnil | Tfield _)  , Sep    )
    | (Tpackage(_,_)      , Sep    ) -> empty
    (* "Deeply separable" case for these same constructors. *)
    | (Tarrow _           , Deepsep)
    | (Ttuple _           , Deepsep)
    | (Tvariant(_)        , Deepsep)
    | (Tobject(_,_)       , Deepsep)
    | ((Tnil | Tfield _)  , Deepsep)
    | (Tpackage(_,_)      , Deepsep) ->
        let tys = immediate_subtypes ty in
        let on_subtype context ty =
          context ++ check_type (Hyps.guard hyps) ty Deepsep in
        List.fold_left on_subtype empty tys
    (* Polymorphic type, and corresponding polymorphic variable.

       In theory, [Tpoly] (forall alpha. tau) would add a new variable
       (alpha) in scope, check its body (tau) recursively, and then
       remove the new variable from the resulting context. Because the
       rule accepts any mode for this variable, the removal never
       fails.

       In practice the implementation is simplified by ignoring the
       new variable, and always returning the [empty] context
       (instead of (alpha : m) in the [Tunivar] case: the constraint
       on the variable is removed/ignored at the variable occurrence
       site, rather than at the variable-introduction site. *)
    (* Note: that we are semantically incomplete in the Deepsep case
       (following the syntactic typing rules): the semantics only
       requires that *closed* sub-type-expressions be (deeply)
       separable; sub-type-expressions containing the quantified
       variable cannot be extracted by constraints (this would be
       a scope violation), so they could be ignored if they occur
       under a separating type constructor. *)
    | (Tpoly(pty,_)       , m      ) ->
        check_type hyps pty m
    | (Tunivar(_)         , _      ) -> empty
    (* Type constructor case. *)
    | (Tconstr(path,tys,_), m      ) ->
        let msig = (Env.find_type path env).type_separability in
        let on_param context (ty, m_param) =
          let hyps = match m_param with
            | Ind -> Hyps.guard hyps
            | Sep -> hyps
            | Deepsep -> Hyps.poison hyps in
          context ++ check_type hyps ty (compose m m_param) in
        List.fold_left on_param empty (List.combine tys msig)
  in
  check_type Hyps.empty ty m

let best_msig decl = List.map (fun _ -> Ind) decl.type_params
let worst_msig decl = List.map (fun _ -> Deepsep) decl.type_params

(** [msig_of_external_type decl] infers the mode signature of an
    abstract/external type. We must assume the worst, namely that this
    type may be defined as an unboxed algebraic datatype imposing deep
    separability of its parameters.

    One exception is when the type is marked "immediate", which
    guarantees that its representation is only integers.  Immediate
    types are always separable, so [Ind] suffices for their
    parameters.

    Note: this differs from {!Types.Separability.default_signature},
    which does not have access to the declaration and its immediacy. *)
let msig_of_external_type decl =
  match decl.type_immediate with
  | Always | Always_on_64bits -> best_msig decl
  | Unknown -> worst_msig decl

(** [msig_of_context ~decl_loc constructor context] returns the
   separability signature of a single-constructor type whose
   definition is valid in the mode context [context].

   Note: A GADT constructor introduces existential type variables, and
   may also introduce some equalities between its return type
   parameters and type expressions containing universal and
   existential variables. In other words, it introduces new type
   variables in scope, and restricts existing variables by adding
   equality constraints.

   [msig_of_context] performs the reverse transformation: the context
   [ctx] computed from the argument of the constructor mentions
   existential variables, and the function returns a context over the
   (universal) type parameters only. (Type constraints do not
   introduce existential variables, but they do introduce equalities;
   they are handled as GADTs equalities by this function.)

   The transformation is separability-preserving in the following
   sense: for any valid instance of the result mode signature
   (replacing the universal type parameters with ground types
   respecting the variable's separability mode), any possible
   extension of this context instance with ground instances for the
   existential variables of [parameter] that respects the equation
   constraints will validate the separability requirements of the
   modes in the input context [ctx].

   Sometimes no such universal context exists, as an existential type
   cannot be safely introduced, then this function raises an [Error]
   exception with a [Non_separable_evar] payload.  *)
let msig_of_context : decl_loc:Location.t -> parameters:type_expr list
    -> context -> Sep.signature =
  fun ~decl_loc ~parameters context ->
    let handle_equation (acc, context) param_instance =
      (* In the theory, GADT equations are of the form
           ('a = <ty>)
         for each type parameter 'a of the type constructor. For each
         such equation, we should "strengthen" the current context in
         the following way:
         - if <ty> is another variable 'b,
           the mode of 'a is set to the mode of 'b,
           and 'b is set to Ind
         - if <ty> is a type expression whose variables are all Ind,
           set 'a to Ind and discard the equation
         - otherwise (one of the variable of 'b is not Ind),
           set 'a to Deepsep and set all variables of <ty> to Ind

         In practice, type parameters are determined by their position
         in a list, they do not necessarily have a corresponding type variable.
         Instead of "setting 'a" in the context as in the description above,
         we build a list of modes by repeated consing into
         an accumulator variable [acc], setting existential variables
         to Ind as we go. *)
      let get context var =
        try TVarMap.find var context with Not_found -> Ind in
      let set_ind context var =
        TVarMap.add var Ind context in
      let is_ind context var = match get context var with
        | Ind -> true
        | Sep | Deepsep -> false in
      match get_desc param_instance with
      | Tvar text ->
          let var = {text; id = get_id param_instance} in
          (get context var) :: acc, (set_ind context var)
      | _ ->
          let instance_exis = free_variables param_instance in
          if List.for_all (is_ind context) instance_exis then
            Ind :: acc, context
          else
            Deepsep :: acc, List.fold_left set_ind context instance_exis
    in
    let mode_signature, context =
      let (mode_signature_rev, ctx) =
        List.fold_left handle_equation ([], context) parameters in
      (* Note: our inference system is not principal, because the
         inference result depends on the order in which those
         equations are processed. (To our knowledge this is the only
         source of non-principality.) If two parameters ('a, 'b) are
         forced to be equal to each other, and also separable, then
         either modes (Sep, Ind) and (Ind, Sep) are correct, allow
         more declarations than (Sep, Sep), but (Ind, Ind) would be
         unsound.

         Such a non-principal example is the following:

           type ('a, 'b) almost_eq =
             | Almost_refl : 'c -> ('c, 'c) almost_eq

         (This example looks strange: GADT equations are typically
         either on only one parameter, or on two parameters that are
         not used to classify constructor arguments. Indeed, we have
         not found non-principal declarations in real-world code.)

         In a non-principal system, it is important the our choice of
         non-unique solution be at least predictable. We find it more
         natural, when either ('a : Sep, 'b : Ind) and ('a : Ind,
         'b : Sep) are correct because 'a = 'b, to choose to make the
         first/leftmost parameter more constrained. We read this as
         saying that 'a must be Sep, and 'b = 'a so 'b can be
         Ind. (We define the second parameter as equal of the first,
         already-seen parameter; instead of saying that the first
         parameter is equal to the not-yet-seen second one.)

         This is achieved by processing the equations from left to
         right with List.fold_left, instead of using
         List.fold_right. The code is slightly more awkward as it
         needs a List.rev on the accumulated modes, but it gives
         a more predictable/natural (non-principal) behavior.
  *)
      (List.rev mode_signature_rev, ctx) in
    (* After all variables determined by the parameters have been set to Ind
       by [handle_equation], all variables remaining in the context are
       purely existential and should not require a stronger mode than Ind. *)
    let check_existential evar mode =
      if rank mode > rank Ind then
        raise (Error (decl_loc, Non_separable_evar evar.text))
    in
    TVarMap.iter check_existential context;
    mode_signature

(** [check_def env def] returns the signature required
    for the type definition [def] in the typing environment [env].

    The exception [Error] is raised if we discover that
    no such signature exists -- the definition will always be invalid.
    This only happens when the definition is marked to be unboxed. *)

let check_def
  : Env.t -> type_definition -> Sep.signature
  = fun env def ->
  match structure def with
  | Abstract ->
      msig_of_external_type def
  | Synonym type_expr ->
      check_type env type_expr Sep
      |> msig_of_context ~decl_loc:def.type_loc ~parameters:def.type_params
  | Open | Algebraic ->
      best_msig def
  | Unboxed constructor ->
      check_type env constructor.argument_type Sep
      |> msig_of_context ~decl_loc:def.type_loc
           ~parameters:constructor.result_type_parameter_instances

let compute_decl env decl =
  if Config.flat_float_array then check_def env decl
  else
    (* Hack: in -no-flat-float-array mode, instead of always returning
       [best_msig], we first compute the separability signature --
       falling back to [best_msig] if it fails.

       This discipline is conservative: it never
       rejects -no-flat-float-array programs. At the same time it
       guarantees that, for any program that is also accepted
       in -flat-float-array mode, the same separability will be
       inferred in the two modes. In particular, the same .cmi files
       and digests will be produced.

       Before we introduced this hack, the production of different
       .cmi files would break the build system of the compiler itself,
       when trying to build a -no-flat-float-array system from
       a bootstrap compiler itself using -flat-float-array. See #9291.
       *)
    try check_def env decl with
    | Error _ ->
       (* It could be nice to emit a warning here, so that users know
          that their definition would be rejected in -flat-float-array mode *)
       best_msig decl

(** Separability as a generic property *)
type prop = Types.Separability.signature

let property : (prop, unit) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq ts1 ts2 =
    List.length ts1 = List.length ts2
    && List.for_all2 Sep.eq ts1 ts2 in
  let merge ~prop:_ ~new_prop =
    (* the update function is monotonous: ~new_prop is always
       more informative than ~prop, which can be ignored *)
    new_prop in
  let default decl = best_msig decl in
  let compute env decl () = compute_decl env decl in
  let update_decl decl type_separability = { decl with type_separability } in
  let check _env _id _decl () = () in (* FIXME run final check? *)
  { eq; merge; default; compute; update_decl; check; }

(* Definition using the fixpoint infrastructure. *)
let update_decls env decls =
  Typedecl_properties.compute_property_noreq property env decls
