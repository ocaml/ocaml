(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variables used in the backend, optionally equipped with "provenance"
    information, used for the emission of debugging information. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include module type of struct include Ident end

type backend_var = t

module Provenance : sig
  type t

  val create
     : module_path:Path.t
    -> debuginfo:Debuginfo.t
    -> ident_for_type:(Compilation_unit.t * Ident.t)
    -> Is_parameter.t
    -> t

  val module_path : t -> Path.t
  val debuginfo : t -> Debuginfo.t
  val ident_for_type : t -> Compilation_unit.t * Ident.t
  val is_parameter : t -> Is_parameter.t

  val replace_debuginfo : t -> Debuginfo.t -> t
  val replace_ident_for_type : t -> Compilation_unit.t * Ident.t -> t
  val replace_is_parameter : t -> Is_parameter.t -> t

  val print : Format.formatter -> t -> unit
end

module With_provenance : sig
  (** Values of type [t] should be used for variables in binding position. *)
  type t

  val print : Format.formatter -> t -> unit

  val create : ?provenance:Provenance.t -> backend_var -> t

  val var : t -> backend_var
  val provenance : t -> Provenance.t option

  val name : t -> string

  val rename : ?provenance:Provenance.t -> t -> t
end
