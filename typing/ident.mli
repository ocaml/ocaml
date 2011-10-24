(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* Identifiers (unique names) *)

type t

val create: string -> t
val create_idents: string -> int -> t list
val create_persistent: string -> t
val create_predef_exn: string -> t
val rename: t -> t
val name: t -> string
val unique_name: t -> string
val unique_toplevel_name: t -> string
val persistent: t -> bool
val equal: t -> t -> bool
        (* Compare identifiers by name. *)
val same: t -> t -> bool
        (* Compare identifiers by binding location.
           Two identifiers are the same either if they are both
           non-persistent and have been created by the same call to
           [new], or if they are both persistent and have the same
           name. *)
val hide: t -> t
        (* Return an identifier with same name as the given identifier,
           but stamp different from any stamp returned by new.
           When put in a 'a tbl, this identifier can only be looked
           up by name. *)

val make_global: t -> unit
val global: t -> bool
val is_predef_exn: t -> bool

val binding_time: t -> int
val current_time: unit -> int
val set_current_time: int -> unit
val reinit: unit -> unit

val print: Format.formatter -> t -> unit

type 'a tbl = (* Association tables from identifiers to type 'a. *)
    Empty
  | Node of 'a tbl * 'a data * 'a tbl * int

and 'a data =
  { ident: t;
    data: 'a;
    previous: 'a data option }

val empty: 'a tbl
val tbl_data: 'a data -> 'a
val tbl_ident: 'a data -> t
val add: t -> 'a -> 'a tbl -> 'a tbl
val find_same: t -> 'a tbl -> 'a
val find_name: string -> 'a tbl -> 'a
val keys: 'a tbl -> t list

val get_known_new_name: t -> t list -> t

val merge: 'a tbl -> 'a tbl -> 'a tbl
val raw_keys: t tbl -> t list
(* val find: Path.t -> 'a tbl -> 'a *)
