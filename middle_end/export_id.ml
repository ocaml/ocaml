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

(* CR mshinwell for pchambart: We should add a comment here describing
   exactly what is meant by an "export ID" (in terms independent of the
   asmcomp/ code). *)

module T = struct
  module Inner_id = Ext_types.Id (struct end)
  include Ext_types.UnitId (Inner_id) (Compilation_unit)
end

include T
include Ext_types.Identifiable.Make (T)
