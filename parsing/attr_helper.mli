(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                 Jeremie Dimino, Jane Street Europe                  *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers for attributes *)

open Asttypes
open Parsetree

type error =
  | Multiple_attributes of string
  | No_payload_expected of string

type identifier = { names: string list; context: string list }

val std_namespace: string -> string list
val create: ?context:string list -> string -> identifier

(** The [identifier] argument of the following functions is a
    list of alternative names for the attribute we are looking for and a list of
    possible other related names that could appear in this context. For instance:

    {[
      { names=["foo"; "ocaml.foo"]; context=[ "fooo"; "ocaml.fooo"] }
    ]}

    During attribute identification, a warning is emitted if the nearest
    name in the union of names and context is in the list of possible
    names and is at a distance inferior to [attribute_max_distance] *)
val attribute_max_distance: int ref
val is_attribute: ?warn:bool -> identifier -> attribute -> bool
val get_no_payload_attribute:
  ?warn:bool -> identifier -> attributes -> string loc option
val has_no_payload_attribute :
  ?warn:bool -> identifier -> attributes -> bool

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit
