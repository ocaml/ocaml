(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val merge:
          (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val min_binding: 'a t -> (key * 'a)
    val min_binding_opt: 'a t -> (key * 'a) option
    val max_binding: 'a t -> (key * 'a)
    val max_binding_opt: 'a t -> (key * 'a) option
    val choose: 'a t -> (key * 'a)
    val choose_opt: 'a t -> (key * 'a) option
    val split: key -> 'a t -> 'a t * 'a option * 'a t
    val find: key -> 'a t -> 'a
    val find_opt: key -> 'a t -> 'a option
    val find_first: (key -> bool) -> 'a t -> key * 'a
    val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
    val find_last: (key -> bool) -> 'a t -> key * 'a
    val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of 'a t * key * 'a * 'a t * int

    let height = function
        Empty -> 0
      | Node(_,_,_,_,h) -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let singleton x d = Node(Empty, x, d, Empty, 1)

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node(_,_,_,_,h) -> h in
      let hr = match r with Empty -> 0 | Node(_,_,_,_,h) -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node(ll, lv, ld, lr, _) ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node(lrl, lrv, lrd, lrr, _)->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node(rl, rv, rd, rr, _) ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node(rll, rlv, rld, rlr, _) ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node(l, x, d, r, (if hl >= hr then hl + 1 else hr + 1))

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node(Empty, x, data, Empty, 1)
      | Node(l, v, d, r, h) as m ->
          let c = Ord.compare x v in
          if c = 0 then
            if d == data then m else Node(l, x, data, r, h)
          else if c < 0 then
            let ll = add x data l in
            if l == ll then m else bal ll v d r
          else
            let rr = add x data r in
            if r == rr then m else bal l v d rr

    let rec find x = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec find_first_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node(l, v, d, r, _) ->
          if f v then
            find_first_aux v d f l
          else
            find_first_aux v0 d0 f r

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          if f v then
            find_first_aux v d f l
          else
            find_first f r

    let rec find_first_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node(l, v, d, r, _) ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt_aux v0 d0 f r

    let rec find_first_opt f = function
        Empty ->
          None
      | Node(l, v, d, r, _) ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt f r

    let rec find_last_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node(l, v, d, r, _) ->
          if f v then
            find_last_aux v d f r
          else
            find_last_aux v0 d0 f l

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node(l, v, d, r, _) ->
          if f v then
            find_last_aux v d f r
          else
            find_last f l

    let rec find_last_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node(l, v, d, r, _) ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt_aux v0 d0 f l

    let rec find_last_opt f = function
        Empty ->
          None
      | Node(l, v, d, r, _) ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt f l

    let rec find_opt x = function
        Empty ->
          None
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then Some d
          else find_opt x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node(l, v, _, r, _) ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node(Empty, x, d, _, _) -> (x, d)
      | Node(l, _, _, _, _) -> min_binding l

    let rec min_binding_opt = function
        Empty -> None
      | Node(Empty, x, d, _, _) -> Some (x, d)
      | Node(l, _, _, _, _) -> min_binding_opt l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node(_, x, d, Empty, _) -> (x, d)
      | Node(_, _, _, r, _) -> max_binding r

    let rec max_binding_opt = function
        Empty -> None
      | Node(_, x, d, Empty, _) -> Some (x, d)
      | Node(_, _, _, r, _) -> max_binding_opt r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node(Empty, _, _, r, _) -> r
      | Node(l, x, d, r, _) -> bal (remove_min_binding l) x d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | (Node(l, v, d, r, _) as t) ->
          let c = Ord.compare x v in
          if c = 0 then merge l r
          else if c < 0 then
            let ll = remove x l in if l == ll then t else bal ll v d r
          else
            let rr = remove x r in if r == rr then t else bal l v d rr

    let rec iter f = function
        Empty -> ()
      | Node(l, v, d, r, _) ->
          iter f l; f v d; iter f r

    let rec map f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node(l', v, d', r', h)

    let rec mapi f = function
        Empty ->
          Empty
      | Node(l, v, d, r, h) ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node(l', v, d', r', h)

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node(l, v, d, r, _) ->
          fold f r (f v d (fold f l accu))

    let rec for_all p = function
        Empty -> true
      | Node(l, v, d, r, _) -> p v d && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node(l, v, d, r, _) -> p v d || exists p l || exists p r

    (* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_binding k v = function
      | Empty -> singleton k v
      | Node (l, x, d, r, _) ->
        bal (add_min_binding k v l) x d r

    let rec add_max_binding k v = function
      | Empty -> singleton k v
      | Node (l, x, d, r, _) ->
        bal l x d (add_max_binding k v r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add_min_binding v d r
      | (_, Empty) -> add_max_binding v d l
      | (Node(ll, lv, ld, lr, lh), Node(rl, rv, rd, rr, rh)) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node(l, v, d, r, _) ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node (l1, v1, d1, r1, h1), _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node (l2, v2, d2, r2, _)) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    let rec union f s1 s2 =
      match (s1, s2) with
      | (Empty, s) | (s, Empty) -> s
      | (Node (l1, v1, d1, r1, h1), Node (l2, v2, d2, r2, h2)) ->
          if h1 >= h2 then
            let (l2, d2, r2) = split v1 s2 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d2 with
            | None -> join l v1 d1 r
            | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
          else
            let (l1, d1, r1) = split v2 s1 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d1 with
            | None -> join l v2 d2 r
            | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r

    let rec filter p = function
        Empty -> Empty
      | Node(l, v, d, r, _) as t ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pvd = p v d in
          let r' = filter p r in
          if pvd then if l==l' && r==r' then t else join l' v d r'
          else concat l' r'

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node(l, v, d, r, _) ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pvd = p v d in
          let (rt, rf) = partition p r in
          if pvd
          then (join lt v d rt, concat lf rf)
          else (concat lt rt, join lf v d rf)

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node(l, v, d, r, _) -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal = function
        Empty -> 0
      | Node(l, _, _, r, _) -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node(l, v, d, r, _) -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding

    let choose_opt = min_binding_opt

end
