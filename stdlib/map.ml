(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val find: key -> 'a t -> 'a
    val iter: (key -> 'a -> 'b) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let empty = Empty

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let new l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Set.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              new ll lv ld (new lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Set.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  new (new ll lv ld lrl) lrv lrd (new lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Set.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              new (new l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Set.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  new (new l x d rll) rlv rld (new rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) as t ->
          let c = Ord.compare x v in
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
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f l (f v d (fold f r accu))

end
