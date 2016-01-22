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

type projection =
  | Project_var of Flambda.project_var
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of Flambda.move_within_set_of_closures
  | Field of int * Variable.t

include Identifiable.Make (...)

module Projectee = struct
  type projection =
    | Project_var of Var_within_closure.t
    | Closure of Closure_id.t
    | Field of int
end

module Var_and_projectee = struct
  type t = Variable.t * Projectee.t
end
