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

type decl = Types.type_declaration

type ('prop, 'req) property = {
  eq : 'prop -> 'prop -> bool;
  merge : prop:'prop -> new_prop:'prop -> 'prop;

  default : decl -> 'prop;
  compute : Env.t -> decl -> 'req -> 'prop;
  update_decl : decl -> 'prop -> decl;

  check : Env.t -> Ident.t -> decl -> 'req -> unit;
}

let add_type ~check id decl env =
  let open Types in
  Builtin_attributes.warning_scope ~ppwarning:false decl.type_attributes
    (fun () -> Env.add_type ~check id decl env)

let add_types_to_env decls env =
  List.fold_right
    (fun (id, decl) env -> add_type ~check:true id decl env)
    decls env

let compute_property
: ('prop, 'req) property -> Env.t ->
  (Ident.t * decl) list -> 'req list -> (Ident.t * decl) list
= fun property env decls required ->
  (* [decls] and [required] must be lists of the same size,
     with [required] containing the requirement for the corresponding
     declaration in [decls]. *)
  let props = List.map (fun (_id, decl) -> property.default decl) decls in
  let rec compute_fixpoint props =
    let new_decls =
      List.map2 (fun (id, decl) prop ->
          (id, property.update_decl decl prop))
        decls props in
    let new_env = add_types_to_env new_decls env in
    let new_props =
      List.map2
        (fun (_id, decl) (prop, req) ->
           let new_prop = property.compute new_env decl req in
           property.merge ~prop ~new_prop)
        new_decls (List.combine props required) in
    if not (List.for_all2 property.eq props new_props)
    then compute_fixpoint new_props
    else begin
      List.iter2
        (fun (id, decl) req -> property.check new_env id decl req)
        new_decls required;
      new_decls
    end
  in
  compute_fixpoint props

let compute_property_noreq property env decls =
  let req = List.map (fun _ -> ()) decls in
  compute_property property env decls req
