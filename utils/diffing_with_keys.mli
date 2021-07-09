(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Florian Angeletti, projet Cambium, Inria Paris             *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(**

   When diffing lists where each element has a distinct key, we can refine
   the diffing patch by introducing two composite edit moves: swaps and moves.

   [Swap]s exchange the position of two elements. [Swap] cost is set to
   [2 * change - epsilon].
   [Move]s change the position of one element. [Move] cost is set to
   [delete + addition - epsilon].

   When the cost [delete + addition] is greater than [change] and with those
   specific weights, the optimal patch with [Swap]s and [Move]s can be computed
   directly and cheaply from the original optimal patch.

*)

type 'a with_pos = {pos: int; data:'a}
val with_pos: 'a list -> 'a with_pos list

type ('l,'r,'diff) mismatch =
  | Name of {pos:int; got:string; expected:string; types_match:bool}
  | Type of {pos:int; got:'l; expected:'r; reason:'diff}

(** This specialized version of changes introduces two composite
    changes: [Move] and [Swap]
*)
type ('l,'r,'diff) change =
  | Change of ('l,'r,'diff) mismatch
  | Swap of { pos: int * int; first: string; last: string }
  | Move of {name:string; got:int; expected:int}
  | Insert of {pos:int; insert:'r}
  | Delete of {pos:int; delete:'l}

val prefix: Format.formatter -> ('l,'r,'diff) change -> unit

module Define(D:Diffing.Defs with type eq := unit): sig

  type diff = (D.left, D.right, D.diff) mismatch
  type left = D.left with_pos
  type right = D.right with_pos

  (** Composite changes and patches *)
  type composite_change = (D.left,D.right,D.diff) change
  type patch = composite_change list

  (** Atomic changes *)
  type change = (left,right,unit,diff) Diffing.change

  module type Parameters = sig
    val weight: change -> int
    val test: D.state -> left -> right -> (unit, diff) result
    val update: change -> D.state -> D.state

    val key_left: D.left -> string
    val key_right: D.right -> string
  end

  module Simple: Parameters -> sig
      val diff: D.state -> D.left list -> D.right list -> patch
    end

end
