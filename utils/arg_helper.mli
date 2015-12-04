(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

(** Decipher command line arguments of the form
        <value> | <key>=<value>[,...]
    (as used for example for the specification of inlining parameters
    varying by simplification round).
*)

module Make (S : sig
  module Key : sig
    type t

    (** The textual representation of a key must not contain '=' or ','. *)
    val of_string : string -> t

    module Map : Map.S with type key = t
  end

  module Value : sig
    type t

    (** The textual representation of a value must not contain '=' or ','. *)
    val of_string : string -> t
  end
end) : sig
  type parsed =
    | Always of S.Value.t
    | Variable of S.Value.t S.Key.Map.t

  val parse : string -> help_text:string -> parsed

  val get : key:S.Key.t -> S.Value.t -> parsed -> S.Value.t
end
