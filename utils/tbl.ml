(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

type ('a, 'b) t =
    Empty
  | Node of ('a, 'b) t * 'a * 'b * ('a, 'b) t * int

let empty = Empty

let height = function
    Empty -> 0
  | Node(_,_,_,_,h) -> h

let create l x d r =
  let hl = height l and hr = height r in
  Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

let bal l x d r =
  let hl = height l and hr = height r in
  if hl > hr + 1 then
    match l with
    | Node (ll, lv, ld, lr, _) when height ll >= height lr ->
        create ll lv ld (create lr x d r)
    | Node (ll, lv, ld, Node (lrl, lrv, lrd, lrr, _), _) ->
        create (create ll lv ld lrl) lrv lrd (create lrr x d r)
    | _ -> assert false
  else if hr > hl + 1 then
    match r with
    | Node (rl, rv, rd, rr, _) when height rr >= height rl ->
        create (create l x d rl) rv rd rr
    | Node (Node (rll, rlv, rld, rlr, _), rv, rd, rr, _) ->
        create (create l x d rll) rlv rld (create rlr rv rd rr)
    | _ -> assert false
  else
    create l x d r

let rec add x data = function
    Empty ->
      Node(Empty, x, data, Empty, 1)
  | Node(l, v, d, r, h) as t ->
      let c = compare x v in
      if c = 0 then
        Node(l, x, data, r, h)
      else if c < 0 then
        bal (add x data l) v d r
      else
        bal l v d (add x data r)

let rec find x = function
    Empty ->
      raise Not_found
  | Node(l, v, d, r, _) ->
      let c = compare x v in
      if c = 0 then d
      else find x (if c < 0 then l else r)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, d1, r1, h1), Node(l2, v2, d2, r2, h2)) ->
      bal l1 v1 d1 (bal (merge r1 l2) v2 d2 r2)

let rec remove x = function
    Empty ->
      Empty
  | Node(l, v, d, r, h) as t ->
      let c = compare x v in
      if c = 0 then
        merge l r
      else if c < 0 then
        bal (remove x l) v d r
      else
        bal l v d (remove x r)

let rec iter f = function
    Empty -> ()
  | Node(l, v, d, r, _) ->
      iter f l; f v d; iter f r

open Format

let print print_key print_data tbl =
  open_hvbox 2;
  print_string "[[";
  iter (fun k d ->
          open_box 2;
          print_key k; print_string " ->"; print_space();
          print_data d; print_string ";";
          close_box(); print_space())
        tbl;
  print_string "]]";
  close_box()
