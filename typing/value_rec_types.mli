(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         Vincent Laviron, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2023 OCamlPro, SAS                                         *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Types related to the compilation of value let-recs (non-functional
     recursive definitions) *)

(** The kind of recursive bindings, as computed by
    [Value_rec_check.classify_expression] *)
type recursive_binding_kind =
| Static
  (** Bindings for which some kind of pre-allocation scheme is possible.
      The expression is allowed to be recursive, as long as its definition does
      not inspect recursively defined values. *)
| Dynamic
  (** Bindings for which pre-allocation is not possible.
      The expression is not allowed to refer to any recursive variable. *)
