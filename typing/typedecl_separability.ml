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
