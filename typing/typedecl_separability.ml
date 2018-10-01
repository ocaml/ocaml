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

[@@@warning "-32"] (* unused values *);;

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
  kind: parameter_kind; (* for error messages *)
  mutability: Asttypes.mutable_flag;
  argument_type: type_expr;
  result_type_parameter_instances: type_expr list;
  (** result_type_parameter_instances represents the domain of the
     constructor; usually it is just a list of the datatype parameter
     ('a, 'b, ...), but when using GADTs or constraints it could
     contain arbitrary type expressions.

     For example, [type 'a t = 'b constraint 'a = 'b * int] has
     [['b * int]] as [result_type_parameter_instances], and so does
     [type _ t = T : 'b -> ('b * int) t]. *)
  location : Location.t;
}
and parameter_kind =
  | Record_field
  | Constructor_parameter
  | Constructor_field (** inlined records *)

(** ['a multiplicity] counts the number of ['a] in
    a structure in which expect to see only one ['a]. *)
type 'a multiplicity =
  | Zero
  | One of 'a
  | Several

type arity = argument_to_unbox multiplicity (**how many parameters?*)

type branching = arity multiplicity (**how many constructors?*)

(** Summarize the right-hand-side of a type declaration,
    for separability-checking purposes. See {!structure} below. *)
type type_structure =
  | Synonym of type_expr
  | Abstract
  | Open
  | Algebraic of branching

let demultiply_list
  : type a b. a list -> (a -> b) -> b multiplicity
  = fun li f -> match li with
  | [] -> Zero
  | [v] -> One (f v)
  | _::_::_ -> Several

let structure : type_definition -> type_structure = fun def ->
  match def.type_kind with
  | Type_open -> Open
  | Type_abstract ->
      begin match def.type_manifest with
      | None -> Abstract
      | Some type_expr -> Synonym type_expr
      end
  | Type_record (labels, _) ->
      Algebraic (One (
        demultiply_list labels @@ fun ld -> {
          location = ld.ld_loc;
          kind = Record_field;
          mutability = ld.ld_mutable;
          argument_type = ld.ld_type;
          result_type_parameter_instances = def.type_params;
        }
      ))
  | Type_variant constructors ->
      Algebraic (demultiply_list constructors @@ fun cd ->
        let result_type_parameter_instances =
          match cd.cd_res with
          (* cd_res is the optional return type (in a GADT);
             if None, just use the type parameters *)
          | None -> def.type_params
          | Some ret_type ->
              begin match Ctype.repr ret_type with
              | {desc=Tconstr (_, tyl, _)} ->
                  List.map Ctype.repr tyl
              | _ -> assert false
              end
        in
        begin match cd.cd_args with
        | Cstr_tuple tys ->
            demultiply_list tys @@ fun argument_type -> {
              location = cd.cd_loc;
              kind = Constructor_parameter;
              mutability = Asttypes.Immutable;
              argument_type;
              result_type_parameter_instances;
            }
        | Cstr_record labels ->
            demultiply_list labels @@ fun ld ->
              let argument_type = ld.ld_type in
              {
                location = ld.ld_loc;
                kind = Constructor_field;
                mutability = ld.ld_mutable;
                argument_type;
                result_type_parameter_instances;
              }
        end)


type error =
  | Non_separable_evar of string option

exception Error of Location.t * error
(*
exception Not_separable of { loc: Location.t; evar: string option; }
*)

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
  match (Ctype.repr ty).desc with
  (* these are the important cases,
     on which immediate_subtypes is called from check_type_expr *)
  | Tarrow(_,ty1,ty2,_) ->
      [ty1; ty2]
  | Ttuple(tys)
  | Tpackage(_,_,tys) ->
      tys
  | Tobject(row,class_ty) ->
      let class_subtys =
        match !class_ty with
        | None        -> []
        | Some(_,tys) -> tys
      in
      immediate_subtypes_object_row class_subtys row
  | Tvariant(row) ->
      immediate_subtypes_variant_row [] row

  (* the cases below are not called from check_type_expr,
     they are here for completeness *)
  | Tnil | Tfield _ ->
      (* these should only occur under Tobject and not at the toplevel,
         but "better safe than sorry" *)
      immediate_subtypes_object_row [] ty
  | Tlink _ | Tsubst _ -> assert false (* impossible due to Ctype.repr *)
  | Tvar _ | Tunivar _ -> []
  | Tpoly (pty, _) -> [pty]
  | Tconstr (_path, tys, _) -> tys

and immediate_subtypes_object_row acc ty = match (Ctype.repr ty).desc with
  | Tnil -> acc
  | Tfield (_label, _kind, ty, rest) ->
      let acc = ty :: acc in
      immediate_subtypes_object_row acc rest
  | _ -> ty :: acc

and immediate_subtypes_variant_row acc desc =
  let add_subtypes acc =
    let add_subtype acc (_l, rf) =
      immediate_subtypes_variant_row_field acc rf in
    List.fold_left add_subtype acc desc.row_fields in
  let add_row acc =
    let row = Ctype.repr desc.row_more in
    match row.desc with
    | Tvariant more -> immediate_subtypes_variant_row acc more
    | _ -> row :: acc
  in
  add_row (add_subtypes acc)

and immediate_subtypes_variant_row_field acc = function
  | Rpresent(None)
  | Rabsent            -> acc
  | Rpresent(Some(ty)) -> ty :: acc
  | Reither(_,field_types,_,r) ->
      let acc = List.rev_append field_types acc in
      begin match !r with
      | None -> acc
      | Some rf -> immediate_subtypes_variant_row_field acc rf
      end


(** [check_type env sigma ty m] returns the most permissive context [gamma]
    such that [ty] is separable at mode [m] in [gamma], under
    the signature [sigma]. *)
let check_type
  : Env.t -> type_expr -> mode -> context
  = fun env ty m ->
  let rec check_type ty m =
    let ty = Ctype.repr ty in
    match (ty.desc, m) with
    (* Impossible case due to the call to [Ctype.repr]. *)
    | (Tlink _            , _      ) -> assert false
    (* Impossible case (according to comment in [typing/types.mli]. *)
    | (Tsubst(_)          , _      ) -> assert false
    (* "Indiferent" case, nothing to do. *)
    | (_                  , Ind    ) -> empty
    (* Variable case, add constraint. *)
    | (Tvar(alpha)        , m      ) ->
        TVarMap.singleton {text = alpha; id = ty.Types.id} m
    (* "Separable" case for constructors with known memory representation. *)
    | (Tarrow _           , Sep    )
    | (Ttuple _           , Sep    )
    | (Tvariant(_)        , Sep    )
    | (Tobject(_,_)       , Sep    )
    | ((Tnil | Tfield _)  , Sep    )
    | (Tpackage(_,_,_)    , Sep    ) -> empty
    (* "Deeply separable" case for these same constructors. *)
    | (Tarrow _           , Deepsep)
    | (Ttuple _           , Deepsep)
    | (Tvariant(_)        , Deepsep)
    | (Tobject(_,_)       , Deepsep)
    | ((Tnil | Tfield _)  , Deepsep)
    | (Tpackage(_,_,_)    , Deepsep) ->
        let tys = immediate_subtypes ty in
        let on_subtype context ty =
          context ++ check_type ty (compose m Ind) in
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
        check_type pty m
    | (Tunivar(_)         , _      ) -> empty
    (* Type constructor case. *)
    | (Tconstr(path,tys,_), m      ) ->
        let msig = (Env.find_type path env).type_separability in
        let on_param context (ty, m_param) =
          context ++ check_type ty (compose m m_param) in
        List.fold_left on_param empty (List.combine tys msig)
  in
  check_type ty m

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

(** [msig_of_context  context] returns the
   separability signature of a single-constructor type whose definition
   is valid in the mode context [context]. *)
let msig_of_context
  : type_expr list -> context -> Sep.signature
  = fun parameters context ->
    let handle_parameter param_instance (acc, context) =
      let param_instance = Ctype.repr param_instance in
      let get context var =
        try TVarMap.find var context with Not_found -> Ind in
      let remove context var =
        TVarMap.remove var context in
      match param_instance.desc with
      | Tvar text ->
          let var = {text; id = param_instance.Types.id} in
          (get context var) :: acc, remove context var
      | _ ->
          failwith "TODO: GADT case"
    in
    let mode_signature, context =
      (* fold_right here is necessary to get the mode
         consed to the accumulator in the right order *)
      List.fold_right handle_parameter parameters ([], context) in
    assert (TVarMap.is_empty context);
    mode_signature

(** [check_def env def] returns the signature required
    for the type definition [def] in the typing environment [env].

    The exception [Not_separable] is raised if we discover that
    no such signature exists -- the definition will always be invalid. This
    only happens when the definition is marked to be unboxed. *)
let check_def
  : Env.t -> type_definition -> Sep.signature
  = fun env def ->
  let boxed = not def.type_unboxed.unboxed in
  match structure def with
  | Abstract ->
      assert boxed;
      msig_of_external_type def
  | Synonym type_expr ->
      check_type env type_expr Sep
      |> msig_of_context def.type_params
  | Open | Algebraic (Zero | Several | One (Zero | Several)) ->
      assert boxed;
      best_msig def
  | Algebraic (One (One constructor)) ->
    if boxed then best_msig def
    else
      check_type env constructor.argument_type Sep
      |> msig_of_context def.type_params

let compute_decl env decl =
  if not Config.flat_float_array then best_msig decl
  else check_def env decl
