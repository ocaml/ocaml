(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Coalesce labels that point at the same source location.  This maximises
    the chance that two labels which actually point at the same location can
    be identified as doing so by using a simple equality check.  This helps
    in particular with constructing the DWARF-5 .debug_addr section.

    This pass also rewrites all labels that are defined by [Llabel]
    constructs in the code so that they are in increasing order by code
    address.  (So in particular this excludes [call_labels] and so forth.)
    This rewriting is necessary so that the code used to construct DWARF-4
    location lists can use the ordering between labels as a proxy for
    the ordering between the corresponding virtual memory addresses.
*)
(* CR mshinwell: Also update comment to note that we need labels to be
   deduped so we can avoid emitting empty location list entries, which produce
   "hole in location list" warnings in objdump when such an entry is at the
   start of an object file (top of first function). *)
(* CR mshinwell: It's not clear the second paragraph above is true, since
   maybe that code only looks at the labels inserted by [Compute_ranges],
   which are in order. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val fundecl
   : Linearize.fundecl
  -> int Numbers.Int.Map.t * Linearize.fundecl
