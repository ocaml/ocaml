(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                  Mark Shinwell, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright and licence information to be added.                     *)
(*                                                                     *)
(*                                                                     *)
(*                                                                     *)
(***********************************************************************)

type t

include Emittable.S with type t := t

val create : abbreviation_code:Abbreviation_code.t
  -> tag:Tag.t
  -> attributes:Attribute.t list
  -> t
