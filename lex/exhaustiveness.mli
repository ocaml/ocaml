(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                             Martin Jambon                              *)
(*                                                                        *)
(*   Copyright 2025 Martin Jambon                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(**
   Check whether a rule may fail to match on some input
*)

(** Report rules that may fail on some input.
    A warning is printed on stderr.

    @param fatal changes the error messages to indicate that they're fatal
*)
val check :
  ?fatal:bool ->
  Lexgen.automata array ->
  (string list, Syntax.location) Lexgen.automata_entry list ->
  (unit, unit) result
