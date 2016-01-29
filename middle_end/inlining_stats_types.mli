(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* Types used for producing statistics about inlining. *)

module Inlined : sig
  type not_inlined_reason =
    | Unspecialized
    | Without_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t

  type inlined_reason =
    | Unconditionally
    | Decl_local_to_application
    | Stub
    | Without_subfunctions of Inlining_cost.Whether_sufficient_benefit.t
    | With_subfunctions of
        Inlining_cost.Whether_sufficient_benefit.t
        * Inlining_cost.Whether_sufficient_benefit.t

  type t =
    | Not_inlined of not_inlined_reason
    | Inlined of inlined_reason
end

module Unrolled : sig
  type t =
    | Unrolling_not_tried
    | Not_unrolled of Inlining_cost.Whether_sufficient_benefit.t
    | Unrolled of Inlining_cost.Whether_sufficient_benefit.t
end

module Specialised : sig
  type t =
    | Specialising_not_tried
    | Not_specialised of Inlining_cost.Whether_sufficient_benefit.t
    | Specialised of Inlining_cost.Whether_sufficient_benefit.t
end

module Nonrecursive : sig
  type t = Inlined.t
end

module Recursive : sig
  type t = Unrolled.t * Specialised.t
end

module Prevented : sig
  type t =
    | Function_obviously_too_large of int
    | Function_prevented_from_inlining
    | Level_exceeded
    | Classic_heuristic
end

module Decision : sig

  type t =
    | Prevented of Prevented.t
    | Nonrecursive of Nonrecursive.t
    | Recursive of Recursive.t

  val summary : Format.formatter -> t -> unit
  val calculation : depth:int -> Format.formatter -> t -> unit
end
