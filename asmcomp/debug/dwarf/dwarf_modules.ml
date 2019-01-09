(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DAH = Dwarf_attribute_helpers
module DS = Dwarf_state

let proto_die_for_module state ~module_path ~get_parent ~module_name =
  match DS.find_die_for_module_path state ~module_path with
  | None ->
    (* We don't give information about the code range for the module
       initialiser in this DIE.  The reason is that the standard says nothing
       about the treatment of local variables, etc., for such initialisers.
       Furthermore, we would not want such variables being in scope in the
       debugger whenever we are inside a function from the module. *)
    let proto_die =
      Proto_die.create ~parent:(Some (get_parent ()))
        ~tag:Module
        ~attribute_values:[
          DAH.create_name module_name;
        ]
        ()
    in
    DS.record_die_for_module_path state ~module_path proto_die;
    proto_die
  | Some proto_die -> proto_die

let dwarf state ~(module_path : Path.t) =
  let rec create_up_to_root ~(module_path : Path.t) =
    match module_path with
    | Pident module_name ->
      let module_name = Ident.name module_name in
      proto_die_for_module state ~module_path ~module_name
        ~get_parent:(fun () -> DS.compilation_unit_proto_die state)
    | Pdot (module_path_prefix, module_name, _) ->
      proto_die_for_module state ~module_path ~module_name
        ~get_parent:(fun () ->
          create_up_to_root ~module_path:module_path_prefix)
    | Papply _ -> Misc.fatal_error "[Papply] should not occur here"
  in
  create_up_to_root ~module_path
