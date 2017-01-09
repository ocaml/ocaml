(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Simon Cruanes                                          *)
(*                                                                        *)
(*   Copyright 2017 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Iter]: functional iterators *)

type ('a, 's) step =
  | Done
  | Skip of 's
  | Yield of 'a * 's

type +_ t =
  | Sequence : 's * ('s -> ('a,'s) step) -> 'a t

let empty = Sequence ((), (fun _ -> Done))

let return x =
  let next = function
    | true -> Done
    | false -> Yield (x, true)
  in
  Sequence (false, next)

let map f (Sequence (state, next)) =
  let next' s = match next s with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (x, s') -> Yield (f x, s')
  in
  Sequence (state, next')

let filter_map f (Sequence (state, next)) =
  let next' s = match next s with
    | Done -> Done
    | Skip s -> Skip s
    | Yield (x, s') ->
        match f x with
          | None -> Skip s'
          | Some y -> Yield (y, s')
  in
  Sequence (state, next')

let filter f (Sequence (state, next)) =
  let next' s = match next s with
    | Done -> Done
    | Skip _ as res -> res
    | Yield (x, _) as res when f x -> res
    | Yield (_, s') -> Skip s'
  in
  Sequence (state, next')

type ('a, 'b, 'top_st) flat_map_state =
    FMS :
      { top: 'top_st;
        sub: 'sub_st;
        sub_next: 'sub_st -> ('b, 'sub_st) step
      } -> ('a, 'b, 'top_st) flat_map_state

let flat_map f (Sequence (state, next)) =
  let next' (FMS { top; sub; sub_next}) =
    match sub_next sub with
      | Skip sub' -> Skip (FMS {top; sub=sub'; sub_next})
      | Done ->
          begin match next top with
            | Done -> Done
            | Skip top' -> Skip (FMS {top=top'; sub; sub_next})
            | Yield (x, top') ->
                let Sequence (sub, sub_next) = f x in
                Skip (FMS { top=top'; sub; sub_next; })
          end
      | Yield (x, sub') ->
          Yield (x, FMS {top; sub=sub'; sub_next})
  in
  let s0 = FMS {top=state; sub=(); sub_next=(fun _ -> Done) } in
  Sequence (s0, next')

let fold_left f acc (Sequence (state,next)) =
  let rec aux f acc state = match next state with
    | Done -> acc
    | Skip state' -> aux f acc state'
    | Yield (x, state') ->
        let acc = f acc x in
        aux f acc state'
  in
  aux f acc state

let iter f (Sequence (state,next)) =
  let rec aux state = match next state with
    | Done -> ()
    | Skip state' -> aux state'
    | Yield (x, state') ->
        f x;
        aux state'
  in
  aux state
