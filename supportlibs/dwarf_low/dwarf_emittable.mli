(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  (** Measure the size in bytes of the given entity. *)
  val size : t -> Int64.t

  (** Emit assembler directives to describe the given entity. *)
  val emit : t -> (module Asm_directives.S) -> unit
end

module type S1_ignore = sig
  type 'a t

  (** Measure the size in bytes of the given entity. *)
  val size : _ t -> Int64.t

  (** Emit assembler directives to describe the given entity. *)
  val emit : _ t -> (module Asm_directives.S) -> unit
end
