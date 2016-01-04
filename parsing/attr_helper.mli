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

type identifier =
  { names: string list; neighbouring_names: string list; max_distance:int }
(** The identifier type represents the information needed to recognize
   a given attribute even in presence of spelling mistakes
   - The field [names] lists all possible names for the attribute
   - The field [neighbouring_names] enumerates similar names that might appear,
    potentially mispelled, in the same context as the attribute and should not
    be misidentified as the attribute.
    - The field [max_distance] is the maximal distance beyond which words are
    always considered as distinct words rather than misspelling. The choice of
    [max_distance] should be a compromise between false positive and false negative
    rates. In doubt, 1 or 2 are good default values.

    For instance, for an attribute [foo] that might appear in the same
    context than an [fooo] attribute, the identifier could be

    {[
      {
        names=["foo"; "ocaml.foo"];
        neighbouring_names=[ "fooo"; "ocaml.fooo"];
        max_distance=2
      }
    ]}
    More concrete examples can be found in {!Builtin_attributes} and
    {!Translattribute}.

    A name which does not belong to the [names] list is considered to be a
    misspelled version of the identifier if:
     - the minimal distance [d] between the name and the list of attributes name
    is less than [max_distance]
    - no name in the [neighbouring_names] list is at a distance strictly inferior
    to [d].

*)

(** [std_namespace] creates standard aliases [[ short_name; "ocaml." ^ short_name]]
    for a given short name *)
val std_namespace: string -> string list

(** Create a standard identifier from a short name *)
val create:
  ?neighbouring_names:string list -> ?max_distance:int -> string -> identifier

(** In the three following functions, a warning is emitted if a misspelled
    version of the identifier is detected *)
val is_attribute: ?warn:bool -> identifier -> attribute -> bool
val get_no_payload_attribute:
  ?warn:bool -> identifier -> attributes -> string loc option
val has_no_payload_attribute :
  ?warn:bool -> identifier -> attributes -> bool

exception Error of Location.t * error

val report_error: Format.formatter -> error -> unit
