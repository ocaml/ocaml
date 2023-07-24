(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation of pattern-matching *)

open Typedtree
open Lambda
open Debuginfo.Scoped_location

(* Entry points to match compiler *)
val for_function:
        scopes:scopes -> Location.t ->
        int ref option -> lambda -> (pattern * lambda) list -> partial ->
        lambda
val for_trywith:
        scopes:scopes -> Location.t ->
        lambda -> (pattern * lambda) list ->
        lambda
val for_let:
        scopes:scopes -> Location.t ->
        lambda -> pattern -> lambda ->
        lambda
val for_multiple_match:
        scopes:scopes -> Location.t ->
        lambda list -> (pattern * lambda) list -> partial ->
        lambda

val for_tupled_function:
        scopes:scopes -> Location.t ->
        Ident.t list -> (pattern list * lambda) list -> partial ->
        lambda

(** [for_optional_arg_default pat body ~default_arg ~param] is:

   {[
     let $pat =
       match $param with
       | Some x -> x
       | None -> $default_arg
     in
     $body
   ]}
*)
val for_optional_arg_default:
  scopes:scopes -> Location.t ->
  pattern -> default_arg:lambda -> param:Ident.t -> lambda ->
  lambda

exception Cannot_flatten

val flatten_pattern: int -> pattern -> pattern list

(* Expand stringswitch to  string test tree *)
val expand_stringswitch:
    scoped_location -> lambda -> (string * lambda) list ->
    lambda option -> lambda

val inline_lazy_force : lambda -> scoped_location -> lambda
