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

type t =
  | Unavailable
  | This of type_expr
  | Only_on_64_bits of type_expr

(* We use the Ctype.expand_head_opt version of expand_head to get access
   to the manifest type of private abbreviations. *)
let rec get_unboxed_type_representation env ty fuel =
  if fuel < 0 then Unavailable else
  let ty = Ctype.repr (Ctype.expand_head_opt env ty) in
  match ty.desc with
  | Tconstr (p, args, _) ->
    begin match Env.find_type p env with
    | exception Not_found -> This ty
    | {type_immediate = Always; _} ->
        This Predef.type_int
    | {type_immediate = Always_on_64bits; _} ->
        Only_on_64_bits Predef.type_int
    | {type_unboxed = {unboxed = false}} -> This ty
    | {type_params; type_kind =
         Type_record ([{ld_type = ty2; _}], _)
       | Type_variant [{cd_args = Cstr_tuple [ty2]; _}]
       | Type_variant [{cd_args = Cstr_record [{ld_type = ty2; _}]; _}]}

      ->
        let ty2 = match ty2.desc with Tpoly (t, _) -> t | _ -> ty2 in
        get_unboxed_type_representation env
          (Ctype.apply env type_params ty2 args) (fuel - 1)
    | {type_kind=Type_abstract} -> Unavailable
          (* This case can occur when checking a recursive unboxed type
             declaration. *)
    | _ -> assert false (* only the above can be unboxed *)
    end
  | _ -> This ty

let get_unboxed_type_representation env ty =
  (* Do not give too much fuel: PR#7424 *)
  get_unboxed_type_representation env ty 100
;;
