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




(** [check_type env sigma ty m] returns the most permissive context [gamma]
    such that [ty] is separable at mode [m] in [gamma], under
    the signature [sigma]. *)
let check_type
  : Env.t -> type_expr -> mode -> context
  = fun env ty m ->
    ignore (env, ty, m); failwith "TODO"

let best_msig def = List.map (fun _ -> Ind) def.type_params

(** [msig_of_context parameters context] returns the
   separability signature of a single-constructor type whose definition
   is valid in the mode context [context]. *)
let msig_of_context
  : type_expr list -> context -> Sep.signature
  = fun parameters context ->
    ignore (parameters, context); failwith "TODO"

(** [check_def env def] returns the signature required
    for the type definition [def] in the typing environment [env].

    The exception [Not_separable] is raised if we discover that
    no such signature exists -- the definition will always be invalid. This
    only happens when the definition is marked to be unboxed. *)
let check_def
  : Env.t -> type_definition -> Sep.signature
  = fun env def ->
    ignore (env, def); failwith "TODO"

let compute_decl env decl =
  if not Config.flat_float_array then best_msig decl
  else check_def env decl
