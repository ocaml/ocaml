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

(** A description of only what is being projected, not where it is
    being projected from. *)
type t =
  | Project_var of Var_within_closure.t
  | Closure of Closure_id.t
  | Field of int

include Identifiable.S with type t := t

type var_and_projectee = Variable.t * t

module Var_and_projectee : Identifiable.S
  with type t := var_and_projectee
