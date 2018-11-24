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

type error = Bad_immediate_attribute
exception Error of Location.t * error

let marked_as_immediate decl =
  Builtin_attributes.immediate decl.type_attributes

let compute_decl env tdecl =
  match (tdecl.type_kind, tdecl.type_manifest) with
  | (Type_variant [{cd_args = Cstr_tuple [arg]; _}], _)
    | (Type_variant [{cd_args = Cstr_record [{ld_type = arg; _}]; _}], _)
    | (Type_record ([{ld_type = arg; _}], _), _)
  when tdecl.type_unboxed.unboxed ->
    begin match Typedecl_unboxed.get_unboxed_type_representation env arg with
    | Some argrepr -> not (Ctype.maybe_pointer_type env argrepr)
    | None -> false
    end
  | (Type_variant (_ :: _ as cstrs), _) ->
    not (List.exists (fun c -> c.Types.cd_args <> Types.Cstr_tuple []) cstrs)
  | (Type_abstract, Some(typ)) ->
    not (Ctype.maybe_pointer_type env typ)
  | (Type_abstract, None) -> marked_as_immediate tdecl
  | _ -> false

let property : (bool, unit) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq = (=) in
  let merge ~prop:_ ~new_prop = new_prop in
  let default _decl = false in
  let compute env decl () = compute_decl env decl in
  let update_decl decl immediacy = { decl with type_immediate = immediacy } in
  let check _env _id decl () =
    if (marked_as_immediate decl) && (not decl.type_immediate) then
      raise (Error (decl.type_loc, Bad_immediate_attribute)) in
  {
    eq;
    merge;
    default;
    compute;
    update_decl;
    check;
  }

let update_decls env decls =
  Typedecl_properties.compute_property_noreq property env decls
