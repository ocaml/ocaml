(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2016 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Check a number of continuation-related invariants *)

(* Currently, this checks that :
   - Every use of a continuation occurs within the scope of its handler
   (this includes the Poptrap and Pushtrap instructions, which must
   refer to a continuation in scope; however, exception stack invariants
   are checked by Trap_analysis later, not here).
   - In every function declaration, a given continuation can only be
   declared in a single handler.
   - A continuation cannot be declared/used both as an exception continuation
   and a normal one.
   - The number of arguments given to continuations makes sense :
   exception continuation handlers take exactly one argument, and
   Exit instructions take the same number of arguments as their handler.

   This pass is intended to document what invariants the backend can rely
   upon, with hope that future changes in earlier passes that break these
   invariants will document what the new invariants are and update the checks.
*)

(** [run ppf fundecl] analyses the given function, and returns whether
    any errors were encountered (with corresponding error messages printed
    on the given formatter). *)

val run : Format.formatter -> Cmm.fundecl -> bool
