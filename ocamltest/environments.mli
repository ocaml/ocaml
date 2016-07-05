(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Sébastien Hinderer, projet Gallium, INRIA Paris           *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Definition of environments, used to pass parameters to tests and actions *)

exception Empty_environment_name

exception Variable_already_defined of Variables.t

exception Environment_already_registered of string

exception Environment_not_found of string

type t

val empty : t

val from_bindings : (Variables.t * string) list -> t

val lookup : Variables.t -> t -> string option
val safe_lookup : Variables.t -> t -> string

val add : Variables.t -> string -> t -> t
val add_bindings : (Variables.t * string) list -> t -> t

val register : string -> t -> unit

val include_ : string -> t -> t

val dump : out_channel -> t -> unit
