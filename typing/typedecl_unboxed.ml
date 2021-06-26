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

module Callstack = struct
  type t = Path.t list

  module TypeMap = Btype.TypeMap
  type map = t TypeMap.t

  let visit p callstack : t =
    p :: callstack

  let visited p callstack =
    List.exists (Path.same p) callstack

  let head (callstacks : map) ty =
    TypeMap.find ty callstacks

  let rec extend callstacks ty new_callstack =
    if TypeMap.mem ty callstacks then callstacks
    else
      let callstacks = TypeMap.add ty new_callstack callstacks in
      Btype.fold_type_expr (fun callstacks ty' ->
        extend callstacks ty' new_callstack
      ) callstacks ty

  let fill ty callstack = extend TypeMap.empty ty callstack

  let expand_head_opt env ty callstacks =
    let head_callstack = head callstacks ty in
    let ty = Ctype.expand_head_opt env ty in
    ty, extend callstacks ty head_callstack

  let apply env params (ty, callstacks) args current_callstack =
    let ty = Ctype.apply env params ty args in
    (ty, extend callstacks ty current_callstack)
end

type t =
  | Unavailable
  | This of type_expr
  | Only_on_64_bits of type_expr

let check_annotated ty callstacks =
  let hash = Btype.TypeHash.create 42 in
  let rec loop ty =
    if Btype.TypeHash.mem hash ty then ()
    else begin
      Btype.TypeHash.add hash ty ();
      assert (Btype.TypeMap.mem ty callstacks);
      Btype.iter_type_expr loop ty;
    end
  in loop ty

(* We use the Ctype.expand_head_opt version of expand_head to get access
   to the manifest type of private abbreviations. *)
let rec get_unboxed_type_representation env ty callstacks =
  let ty, callstacks = Callstack.expand_head_opt env ty callstacks in
  check_annotated ty callstacks;
  match get_desc ty with
  | Tconstr (p, args, _) ->
    let head_callstack = Callstack.head callstacks ty in
    if Callstack.visited p head_callstack then
      Unavailable
    else
      let current_callstack = Callstack.visit p head_callstack in
      begin match Env.find_type p env with
      | exception Not_found -> This ty
      | {type_immediate = Always; _} ->
          This Predef.type_int
      | {type_immediate = Always_on_64bits; _} ->
          Only_on_64_bits Predef.type_int
      | {type_params; type_kind =
           Type_record ([{ld_type = ty2; _}], Record_unboxed _)
         | Type_variant ([{cd_args = Cstr_tuple [ty2]; _}], Variant_unboxed)
         | Type_variant ([{cd_args = Cstr_record [{ld_type = ty2; _}]; _}],
                         Variant_unboxed)}
        ->
          let ty2 = match get_desc ty2 with Tpoly (t, _) -> t | _ -> ty2 in
          let (ty2, callstacks) = Callstack.apply
            env type_params (ty2, callstacks) args current_callstack in
          get_unboxed_type_representation env ty2 callstacks
      | _ -> This ty
      end
  | _ -> This ty

let get_unboxed_type_representation env ty =
  let callstacks = Callstack.fill ty [] in
  get_unboxed_type_representation env ty callstacks
;;
