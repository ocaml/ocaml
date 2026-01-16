(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Malo Monin, projet Cambium, Inria Paris                 *)
(*                                                                        *)
(*   Copyright 2024 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Item: sig
  type ('v, 'k) t = {
    name: string;
    item : 'v;
    kind : 'k;
  }
  val item: ('v,'k) t -> 'v
end

type left_index = int
type right_index = int
type rank = int

type ('a,'v) matches = {
  left: 'a list;
  pairs:('v * 'v) list;
  right:'a list;
}
type ('v,'k) item_matches =  (('v,'k) Item.t, 'v) matches

type unstable_matching = {
  first:left_index * right_index;
  second: left_index * right_index;
  current_rank: rank * rank;
  optimal: rank * rank
}

val stable_matches:
  distance:(int -> int -> int) -> (_,int) matches ->
  (unit, unstable_matching) Result.t

val strong_stable_matches:
  distance:(int -> int -> int) -> (_,int) matches ->
  (unit, unstable_matching) Result.t

(** [matches ~compatible ~preferences ~size:(ls,rs)] computes a matching between
    a set of [ls] left items and [rs] right items favoring the right side. The
    matches are compatible and weakly stable according to the [preferences]
    matrix. The size of the matching is at least 2/3 of the optimal matching
    size (computing optimal matching with partial preferences and ties is in
    NP).
*)
val matches:
    compatible:(left_index -> right_index -> bool)
    -> preferences:(right_index -> (left_index * rank) array)
    -> size: (int * int)
    -> (int, int) matches

(** [fuzzy_match_names ~compatibility ~max_right_item ~cutoff left right] calls
    the {!matches} function using the OSA edit distance to compute preferences
    with a cutoff function. To avoid quadratic complexity on large module size
    we limit the right side to the first [max_right_item] items *)
val fuzzy_match_names: compatibility:('k -> 'k -> bool) ->
  max_right_items:int -> cutoff:(string -> int) -> ('v,'k) Item.t list ->
  ('v,'k) Item.t list -> ('v,'k) item_matches
