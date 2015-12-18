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

(** Modules about numbers that satisfy [Identifiable.S]. *)

module Int : sig
  include Identifiable.S with type t = int

  (** [zero_to_n n] is the set of numbers {0, ..., n} (inclusive). *)
  val zero_to_n : int -> Set.t
end

module Float : Identifiable.S with type t = float
