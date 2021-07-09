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


type 'a with_pos = {pos:int; data:'a}
let with_pos l = List.mapi (fun n data -> {pos=n+1; data}) l

(** Composite change and mismatches *)
type ('l,'r,'diff) mismatch =
  | Name of {pos:int; got:string; expected:string; types_match:bool}
  | Type of {pos:int; got:'l; expected:'r; reason:'diff}

type ('l,'r,'diff) change =
  | Change of ('l,'r,'diff) mismatch
  | Swap of { pos: int * int; first: string; last: string }
  | Move of {name:string; got:int; expected:int}
  | Insert of {pos:int; insert:'r}
  | Delete of {pos:int; delete:'l}

let prefix ppf x =
  let kind = match x with
    | Change _ | Swap _ | Move _ -> Diffing.Modification
    | Insert _ -> Diffing.Insertion
    | Delete _ -> Diffing.Deletion
  in
  let style k ppf inner =
    let sty = Diffing.style k in
    Format.pp_open_stag ppf (Misc.Color.Style sty);
    Format.kfprintf (fun ppf -> Format.pp_close_stag ppf () ) ppf inner
  in
  match x with
  | Change (Name {pos; _ } | Type {pos; _})
  | Insert { pos; _ } | Delete { pos; _ } ->
      style kind ppf "%i. " pos
  | Swap { pos = left, right; _ } ->
      style kind ppf "%i<->%i. " left right
  | Move { got; expected; _ } ->
      style kind ppf "%i->%i. " expected got



(** To detect [move] and [swaps], we are using the fact that
    there are 2-cycles in the graph of name renaming.
    - [Change (x,y,_) is then an edge from
      [key_left x] to [key_right y].
    - [Insert x] is an edge between the special node epsilon and
      [key_left x]
    - [Delete x] is an edge between [key_right] and the epsilon node
      Since for 2-cycle, knowing one edge is enough to identify the cycle
      it might belong to, we are using maps of partial 2-cycles.
*)
module Two_cycle: sig
  type t = private (string * string)
  val create: string -> string -> t
end = struct
  type t = string * string
  let create kx ky =
    if kx <= ky then kx, ky else ky, kx
end
module Swap = Map.Make(struct
    type t = Two_cycle.t
    let compare: t -> t -> int = Stdlib.compare
  end)
module Move = Misc.Stdlib.String.Map


module Define(D:Diffing.Defs with type eq := unit) = struct

  module Internal_defs = struct
    type left = D.left with_pos
    type right = D.right with_pos
    type diff =  (D.left, D.right, D.diff) mismatch
    type eq = unit
    type state = D.state
  end
  module Diff = Diffing.Define(Internal_defs)

  type left = Internal_defs.left
  type right = Internal_defs.right
  type diff = (D.left, D.right, D.diff) mismatch
  type composite_change = (D.left,D.right,D.diff) change
  type nonrec change = (left, right, unit, diff) Diffing.change
  type patch = composite_change list

  module type Parameters = sig
    include Diff.Parameters with type update_result := D.state
    val key_left: D.left -> string
    val key_right: D.right -> string
  end

  module Simple(Impl:Parameters) = struct
    open Impl

    (** Partial 2-cycles *)
    type ('l,'r) partial_cycle =
      | Left of int * D.state * 'l
      | Right of int * D.state * 'r
      | Both of D.state * 'l * 'r

    (** Compute the partial cycle and edge associated to an edge *)
    let edge state (x:left) (y:right) =
      let kx, ky = key_left x.data, key_right y.data in
      let edge =
        if kx <= ky then
          Left (x.pos, state, (x,y))
        else
          Right (x.pos,state, (x,y))
      in
      Two_cycle.create kx ky, edge

    let merge_edge ex ey = match ex, ey with
      | ex, None -> Some ex
      | Left (lpos, lstate, l), Some Right (rpos, rstate,r)
      | Right (rpos, rstate,r), Some Left (lpos, lstate, l) ->
          let state = if lpos < rpos then rstate else lstate in
          Some (Both (state,l,r))
      | Both _ as b, _ | _, Some (Both _ as b)  -> Some b
      | l, _ -> Some l

    let two_cycles state changes =
      let add (state,(swaps,moves)) (d:change) =
        update d state,
        match d with
        | Change (x,y,_) ->
            let k, edge = edge state x y in
            Swap.update k (merge_edge edge) swaps, moves
        | Insert nx ->
            let k = key_right nx.data in
            let edge = Right (nx.pos, state,nx) in
            swaps, Move.update k (merge_edge edge) moves
        | Delete nx ->
            let k, edge = key_left nx.data, Left (nx.pos, state, nx) in
            swaps, Move.update k (merge_edge edge) moves
        | _ -> swaps, moves
      in
      List.fold_left add (state,(Swap.empty,Move.empty)) changes

    (** Check if an edge belongs to a known 2-cycle *)
    let swap swaps x y =
      let kx, ky = key_left x.data, key_right y.data in
      let key = Two_cycle.create kx ky in
      match Swap.find_opt key swaps with
      | None | Some (Left _ | Right _)-> None
      | Some Both (state, (ll,lr),(rl,rr)) ->
          match test state ll rr,  test state rl lr with
          | Ok _, Ok _ ->
              Some ({pos=ll.pos; data=kx}, {pos=rl.pos; data=ky})
          | Error _, _ | _, Error _ -> None

    let move moves x =
      let name =
        match x with
        | Either.Left x -> key_left x.data
        | Either.Right x -> key_right x.data
      in
      match Move.find_opt name moves with
      | None | Some (Left _ | Right _)-> None
      | Some Both (state,got,expected) ->
          match test state got expected with
          | Ok _ ->
              Some (Move {name; got=got.pos; expected=expected.pos})
          | Error _ -> None

    let refine state patch =
      let _, (swaps, moves) = two_cycles state patch in
      let filter: change -> composite_change option = function
        | Keep _ -> None
        | Insert x ->
            begin match move moves (Either.Right x) with
            | Some _ as move -> move
            | None -> Some (Insert {pos=x.pos;insert=x.data})
            end
        | Delete x ->
            begin match move moves (Either.Left x) with
            | Some _ -> None
            | None -> Some (Delete {pos=x.pos; delete=x.data})
            end
        | Change(x,y, reason) ->
            match swap swaps x y with
            | Some ({pos=pos1; data=first}, {pos=pos2; data=last}) ->
                if x.pos = pos1 then
                  Some (Swap { pos = pos1, pos2; first; last})
                else None
            | None -> Some (Change reason)
      in
      List.filter_map filter patch

    let diff state left right =
      let left = with_pos left in
      let right = with_pos right in
      let module Raw = Diff.Simple(Impl) in
      let raw = Raw.diff state (Array.of_list left) (Array.of_list right) in
      refine state raw

  end
end
