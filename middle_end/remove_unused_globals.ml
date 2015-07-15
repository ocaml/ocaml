(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let used_globals id tree =
  let used = ref Ext_types.Int.Set.empty in
  Flambda_iterators.iter_named (function
      | Prim (Pgetglobalfield (modul, pos), _, _) when Ident.same id modul ->
        used := Ext_types.Int.Set.add pos !used
      | _ -> ())
    tree;
  !used

let remove_unused_globals tree =
  let id = Compilation_unit.get_current_id_exn () in
  let used = used_globals id tree in
  Flambda_iterators.map_named (function
      | Prim (Psetglobalfield(Not_exported, pos), arg, dbg)
        when not (Ext_types.Int.Set.mem pos used) -> Prim (Pignore, arg, dbg)
      | e -> e)
    tree
