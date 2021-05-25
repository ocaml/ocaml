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


type 'a with_pos = int * 'a
let with_pos l = List.mapi (fun n x -> n+1,x) l
let pos (x,_) = x
let data (_,x) = x
let mk_pos pos data = pos, data

type ('a,'b) mismatch =
  | Name of {pos:int; got:string; expected:string; types_match:bool}
  | Type of {pos:int; got:'a; expected:'a; reason:'b}

type ('a,'b) change =
  | Change of ('a,'b) mismatch
  | Swap of { pos: int * int; first: string; last: string }
  | Move of {name:string; got:int; expected:int}
  | Insert of {pos:int; insert:'a}
  | Delete of {pos:int; delete:'a}

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

module Swap = Map.Make(struct
    type t = string * string
    let compare: t -> t -> int = Stdlib.compare
  end)
module Move = Misc.Stdlib.String.Map

type ('a,'state) partial_edge =
  | Left of int * 'state * 'a
  | Right of int * 'state * 'a
  | Both of 'state * 'a * 'a

let edge key state x y =
  let kx, ky = key (data x), key (data y) in
  if kx <= ky then
    (kx,ky), Left (pos x, state, (x,y))
  else
    (ky,kx), Right(pos x,state, (x,y))

let add_edge ex ey = match ex, ey with
  | ex, None -> Some ex
  | Left (lpos, lstate, l), Some Right (rpos, rstate,r)
  | Right (rpos, rstate,r), Some Left (lpos, lstate, l) ->
      let state = if lpos < rpos then rstate else lstate in
      Some (Both (state,l,r))
  | Both _ as b, _ | _, Some (Both _ as b)  -> Some b
  | l, _ -> Some l

let exchanges ~update ~key state changes =
  let add (state,(swaps,moves)) d =
    update d state,
    match d with
    | Diffing.Change (x,y,_) ->
        let k, edge = edge key state x y in
        Swap.update k (add_edge edge) swaps, moves
    | Diffing.Insert nx ->
        let k = key (data nx) in
        let edge = Right (pos nx, state,nx) in
        swaps, Move.update k (add_edge edge) moves
    | Diffing.Delete nx ->
        let k, edge = key (data nx), Left (pos nx, state, nx) in
        swaps, Move.update k (add_edge edge) moves
    | _ -> swaps, moves
  in
  List.fold_left add (state,(Swap.empty,Move.empty)) changes


let swap key test swaps x y =
  let kx, ky = key (data x), key (data y) in
  let key = if kx <= ky then kx, ky else ky, kx in
  match Swap.find_opt key swaps with
  | None | Some (Left _ | Right _)-> None
  | Some Both (state, (ll,lr),(rl,rr)) ->
      match test state ll rr,  test state lr rl with
      | Ok _, Ok _ ->
          Some (mk_pos (pos ll) kx, mk_pos (pos rl) ky)
      | Error _, _ | _, Error _ -> None

let move key test moves x =
  let name = key (data x) in
  match Move.find_opt name moves with
  | None | Some (Left _ | Right _)-> None
  | Some Both (state,got,expected) ->
      match test state got expected with
      | Ok _ ->
          Some (Move {name; got=pos got; expected=pos expected})
      | Error _ -> None

let refine ~key ~update ~test state patch =
  let _, (swaps, moves) = exchanges ~key ~update state patch in
  let filter = function
    | Diffing.Keep _ -> None
    | Diffing.Insert x ->
        begin match move key test moves x with
        | Some _ as move -> move
        | None -> Some (Insert {pos=pos x;insert=data x})
        end
    | Diffing.Delete x ->
        begin match move key test moves x with
        | Some _ -> None
        | None -> Some (Delete {pos=pos x;delete=data x})
        end
    | Diffing.Change(x,y, reason) ->
        match swap key test swaps x y with
        | Some ((pos1,first),(pos2,last)) ->
            if pos x = pos1 then
              Some (Swap { pos = pos1, pos2; first; last})
            else None
        | None -> Some (Change reason)
  in
  List.filter_map filter patch
