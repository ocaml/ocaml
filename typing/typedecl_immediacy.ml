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

type error = Bad_immediacy_attribute of Type_immediacy.Violation.t
exception Error of Location.t * error

let compute_decl env tdecl =
  match (tdecl.type_kind, tdecl.type_manifest) with
  | (Type_variant ([{cd_args = Cstr_tuple [arg]
                            | Cstr_record [{ld_type = arg; _}]; _}],
                   Variant_unboxed)
    | Type_record ([{ld_type = arg; _}], Record_unboxed _)), _ ->
    begin match Typedecl_unboxed.get_unboxed_type_representation env arg with
    | None -> Type_immediacy.Unknown
    | Some argrepr -> Ctype.immediacy env argrepr
    end
  | (Type_variant (_ :: _ as cstrs, _), _) ->
    if not (List.exists (fun c -> c.Types.cd_args <> Types.Cstr_tuple []) cstrs)
    then
      Type_immediacy.Always
    else
      Type_immediacy.Unknown
  | (Type_abstract, Some(typ)) -> Ctype.immediacy env typ
  | (Type_abstract, None) -> Type_immediacy.of_attributes tdecl.type_attributes
  | _ -> Type_immediacy.Unknown

let property : (Type_immediacy.t, unit) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq = (=) in
  let merge ~prop:_ ~new_prop = new_prop in
  let default _decl = Type_immediacy.Unknown in
  let compute env decl () = compute_decl env decl in
  let update_decl decl immediacy = { decl with type_immediate = immediacy } in
  let check _env _id decl () =
    let written_by_user = Type_immediacy.of_attributes decl.type_attributes in
    match Type_immediacy.coerce decl.type_immediate ~as_:written_by_user with
    | Ok () -> ()
    | Error violation ->
        raise (Error (decl.type_loc,
                      Bad_immediacy_attribute violation))
  in
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
