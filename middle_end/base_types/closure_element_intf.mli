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

module type S = sig
  include Ext_types.Identifiable

  val wrap : Variable.t -> t
  val unwrap : t -> Variable.t

  val in_compilation_unit : Compilation_unit.t -> t -> bool
  val get_compilation_unit : t -> Compilation_unit.t

  val unique_name : t -> string

  val output_full : out_channel -> t -> unit
end
