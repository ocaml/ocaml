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

type immediacy_kind =
  | Explicit of Type_immediacy.t
  | Of_type of Types.type_expr

let compute_imm env tdecl =
  match (tdecl.type_kind, tdecl.type_manifest) with
  | (Type_variant [{cd_args = Cstr_tuple [arg]; _}], _)
    | (Type_variant [{cd_args = Cstr_record [{ld_type = arg; _}]; _}], _)
    | (Type_record ([{ld_type = arg; _}], _), _)
  when tdecl.type_unboxed.unboxed ->
    begin match Typedecl_unboxed.get_unboxed_type_representation env arg with
    | Typedecl_unboxed.Unavailable -> Explicit Unknown
    | Typedecl_unboxed.This argrepr -> Of_type argrepr
    | Typedecl_unboxed.Only_on_64_bits argrepr ->
       (* FIXME *)
        match Ctype.approx_immediacy env argrepr with
        | Type_immediacy.Always -> Explicit Always_on_64bits
        | Type_immediacy.Always_on_64bits | Type_immediacy.Unknown as x -> Explicit x
    end
  | (Type_variant (_ :: _ as cstrs), _) ->
    if not (List.exists (fun c -> c.Types.cd_args <> Types.Cstr_tuple []) cstrs)
    then
      Explicit Always
    else
      Explicit Unknown
  | (Type_abstract, Some(typ)) -> Of_type typ
  | (Type_abstract, None) ->
     Explicit (Type_immediacy.of_attributes tdecl.type_attributes)
  | _ -> Explicit Unknown

let compute_decl env tdecl =
  match compute_imm env tdecl with
  | Explicit imm -> imm
  | Of_type ty -> Ctype.approx_immediacy env ty

let check_decl env tdecl ~as_ =
  match compute_imm env tdecl with
  | Explicit imm -> Type_immediacy.coerce imm ~as_
  | Of_type ty -> Ctype.check_immediacy env ~as_ ty

let property : (Type_immediacy.t, unit) Typedecl_properties.property =
  let open Typedecl_properties in
  let eq = (=) in
  let merge ~prop:_ ~new_prop = new_prop in
  let default _decl = Type_immediacy.Unknown in
  let compute env decl () = compute_decl env decl in
  let update_decl decl immediacy = { decl with type_immediate = immediacy } in
  let check env _id decl () =
    let written_by_user = Type_immediacy.of_attributes decl.type_attributes in
    match check_decl env decl ~as_:written_by_user with
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
