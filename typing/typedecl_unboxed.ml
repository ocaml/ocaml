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

(* We use the Ctype.expand_head_opt version of expand_head to get access
   to the manifest type of private abbreviations. *)
let rec get_unboxed_type_representation env ty fuel =
  if fuel < 0 then None else
  let ty = Ctype.expand_head_opt env ty in
  match get_desc ty with
  | Tconstr (p, args, _) ->
    begin match Env.find_type p env with
    | exception Not_found -> Some ty
    | {type_params; type_kind =
         Type_record ([{ld_type = ty2; _}], Record_unboxed _)
       | Type_variant ([{cd_args = Cstr_tuple [ty2]; _}], Variant_unboxed)
       | Type_variant ([{cd_args = Cstr_record [{ld_type = ty2; _}]; _}],
                       Variant_unboxed)}
      ->
        let ty2 = match get_desc ty2 with Tpoly (t, _) -> t | _ -> ty2 in
        get_unboxed_type_representation env
          (Ctype.apply env type_params ty2 args) (fuel - 1)
    | _ -> Some ty
    end
  | _ -> Some ty

let get_unboxed_type_representation env ty =
  (* Do not give too much fuel: PR#7424 *)
  get_unboxed_type_representation env ty 100
