(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Assign numerical offsets, within closure blocks, for code pointers and
    environment entries. *)

module Project_closure_index : sig
  type t = private {
    arity_of_first_function : int;
    closure_index : int;
  }

  include Identifiable.S with type t := t
end

module Closure_index : sig
  type t = private {
    arity : int;
    closure_index : int;
  }

  include Identifiable.S with type t := t
end

type result = private {
  project_closure_indexes : Project_closure_index.t Closure_id.Map.t;
  move_within_set_of_closures_indexes : Closure_index.t Closure_id.Map.t;
  free_variable_offsets : int Var_within_closure.Map.t Closure_id.Map.t;
}

val compute : Flambda.program -> result
