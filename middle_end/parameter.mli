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

(** [Parameter.t] carries a unique [Variable.t] used as function parameter.
    It can also carries annotation on the usage of the variable *)

type t

(** Make a parameter from a variable with default attributes *)
val wrap : Variable.t -> t

val var : t -> Variable.t
val vars : t list -> Variable.t list
val var_set : t list -> Variable.Set.t

(** Rename the inner variable of the parameter *)
val rename
   : ?current_compilation_unit:Compilation_unit.t
  -> ?append:string
  -> t
  -> t

val map_var : (Variable.t -> Variable.t) -> t -> t

include Identifiable.S with type t := t
