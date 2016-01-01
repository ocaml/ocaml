(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

module Id : Id_types.Id = Id_types.Id (struct end)
module Unit_id = Id_types.UnitId (Id) (Compilation_unit)

type t = Unit_id.t

include Identifiable.Make (Unit_id)

let create = Unit_id.create
let get_compilation_unit = Unit_id.unit
