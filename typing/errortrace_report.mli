(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Florian Angeletti, projet Cambium, INRIA Paris                        *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions for reporting core level type errors. *)

open Format_doc

val ambiguous_type:
    formatter -> Env.t -> (Path.t * Path.t) -> (Path.t * Path.t) list ->
    Format_doc.t -> Format_doc.t -> Format_doc.t -> unit

val unification :
  formatter ->
  Env.t -> Errortrace.unification_error ->
  ?type_expected_explanation:Format_doc.t -> Format_doc.t -> Format_doc.t ->
  unit

val equality :
  formatter ->
  Out_type.type_or_scheme ->
  Env.t -> Errortrace.equality_error ->
   Format_doc.t -> Format_doc.t ->
  unit

val moregen :
  formatter ->
  Out_type.type_or_scheme ->
  Env.t -> Errortrace.moregen_error ->
  Format_doc.t -> Format_doc.t ->
  unit

val comparison :
  formatter ->
  Out_type.type_or_scheme ->
  Env.t -> Errortrace.comparison_error ->
  Format_doc.t -> Format_doc.t  ->
  unit

val subtype :
  formatter ->
  Env.t ->
  Errortrace.Subtype.error ->
  string ->
  unit
