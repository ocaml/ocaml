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
      | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}

    let height = function
        Empty -> 0
      | Node t -> t.h

    let create l x d r =
      let hl = height l and hr = height r in
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let singleton x d = Node{l=Empty; v=x; d; r=Empty; h=1}

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node t -> t.h in
      let hr = match r with Empty -> 0 | Node t -> t.h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node{l=ll; v=lv; d=ld; r=lr; _} ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node{l=lrl; v=lrv; d=lrd; r=lrr; _}->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node{l=rl; v=rv; d=rd; r=rr; _} ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node{l=rll; v=rlv; d=rld; r=rlr; _} ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node{l=Empty; v=x; d=data; r=Empty; h=1}
      | Node t as m ->
          let c = Ord.compare x t.v in
          if c = 0 then
            if t.d == data then m else Node{l=t.l; v=x; d=data; r=t.r; h=t.h}
          else if c < 0 then
            let ll = add x data t.l in
            if t.l == ll then m else bal ll t.v t.d t.r
          else
            let rr = add x data t.r in
            if t.r == rr then m else bal t.l t.v t.d rr

    let rec find x = function
        Empty ->
          raise Not_found
      | Node t ->
          let c = Ord.compare x t.v in
          if c = 0 then t.d
          else find x (if c < 0 then t.l else t.r)

    let rec find_first_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node t ->
          if f t.v then
            find_first_aux t.v t.d f t.l
          else
            find_first_aux v0 d0 f t.r

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node t ->
          if f t.v then
            find_first_aux t.v t.d f t.l
          else
            find_first f t.r

    let rec find_first_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node t ->
          if f t.v then
            find_first_opt_aux t.v t.d f t.l
          else
            find_first_opt_aux v0 d0 f t.r

    let rec find_first_opt f = function
        Empty ->
          None
      | Node t ->
          if f t.v then
            find_first_opt_aux t.v t.d f t.l
          else
            find_first_opt f t.r

    let rec find_last_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node t ->
          if f t.v then
            find_last_aux t.v t.d f t.r
          else
            find_last_aux v0 d0 f t.l

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node t ->
          if f t.v then
            find_last_aux t.v t.d f t.r
          else
            find_last f t.l

    let rec find_last_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node t ->
          if f t.v then
            find_last_opt_aux t.v t.d f t.r
          else
            find_last_opt_aux v0 d0 f t.l

    let rec find_last_opt f = function
        Empty ->
          None
      | Node t ->
          if f t.v then
            find_last_opt_aux t.v t.d f t.r
          else
            find_last_opt f t.l

    let rec find_opt x = function
        Empty ->
          None
      | Node t ->
          let c = Ord.compare x t.v in
          if c = 0 then Some t.d
          else find_opt x (if c < 0 then t.l else t.r)

    let rec mem x = function
        Empty ->
          false
      | Node t ->
          let c = Ord.compare x t.v in
          c = 0 || mem x (if c < 0 then t.l else t.r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node t when t.l = Empty -> (t.v, t.d)
      | Node t -> min_binding t.l

    let rec min_binding_opt = function
        Empty -> None
      | Node t when t.l = Empty -> Some (t.v, t.d)
      | Node t-> min_binding_opt t.l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node t when t.r = Empty -> (t.v, t.d)
      | Node t -> max_binding t.r

    let rec max_binding_opt = function
        Empty -> None
      | Node t when t.r = Empty -> Some (t.v, t.d)
      | Node t -> max_binding_opt t.r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node t when t.l = Empty -> t.r
      | Node t -> bal (remove_min_binding t.l) t.v t.d t.r

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
      | (Node t as m) ->
          let c = Ord.compare x t.v in
          if c = 0 then merge t.l t.r
          else if c < 0 then
            let ll = remove x t.l in if t.l == ll then m else bal ll t.v t.d t.r
          else
            let rr = remove x t.r in if t.r == rr then m else bal t.l t.v t.d rr

    let rec iter f = function
        Empty -> ()
      | Node t ->
          iter f t.l; f t.v t.d; iter f t.r

    let rec map f = function
        Empty ->
          Empty
      | Node t ->
          let l' = map f t.l in
          let d' = f t.d in
          let r' = map f t.r in
          Node{l=l'; v=t.v; d=d'; r=r'; h=t.h}

    let rec mapi f = function
        Empty ->
          Empty
      | Node t ->
          let l' = mapi f t.l in
          let d' = f t.v t.d in
          let r' = mapi f t.r in
          Node{l=l'; v=t.v; d=d'; r=r'; h=t.h}

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node t ->
          fold f t.r (f t.v t.d (fold f t.l accu))

    let rec for_all p = function
        Empty -> true
      | Node t -> p t.v t.d && for_all p t.l && for_all p t.r

    let rec exists p = function
        Empty -> false
      | Node t -> p t.v t.d || exists p t.l || exists p t.r

    (* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_binding k v = function
      | Empty -> singleton k v
      | Node t ->
        bal (add_min_binding k v t.l) t.v t.d t.r

    let rec add_max_binding k v = function
      | Empty -> singleton k v
      | Node t ->
        bal t.l t.v t.d (add_max_binding k v t.r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add_min_binding v d r
      | (_, Empty) -> add_max_binding v d l
      | (Node{l=ll; v=lv; d=ld; r=lr; h=lh}, Node{l=rl; v=rv; d=rd; r=rr; h=rh}) ->
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
      | Node t ->
          let c = Ord.compare x t.v in
          if c = 0 then (t.l, Some t.d, t.r)
          else if c < 0 then
            let (ll, pres, rl) = split x t.l in (ll, pres, join rl t.v t.d t.r)
          else
            let (lr, pres, rr) = split x t.r in (join t.l t.v t.d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node {l=l2; v=v2; d=d2; r=r2}) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    let rec union f s1 s2 =
      match (s1, s2) with
      | (Empty, s) | (s, Empty) -> s
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, Node {l=l2; v=v2; d=d2; r=r2; h=h2}) ->
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
      | Node t as m ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p t.l in
          let pvd = p t.v t.d in
          let r' = filter p t.r in
          if pvd then if t.l==l' && t.r==r' then m else join l' t.v t.d r'
          else concat l' r'

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node t ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p t.l in
          let pvd = p t.v t.d in
          let (rt, rf) = partition p t.r in
          if pvd
          then (join lt t.v t.d rt, concat lf rf)
          else (concat lt rt, join lf t.v t.d rf)

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node t -> cons_enum t.l (More(t.v, t.d, t.r, e))

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
      | Node t -> cardinal t.l + 1 + cardinal t.r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node t -> bindings_aux ((t.v, t.d) :: bindings_aux accu t.r) t.l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding

    let choose_opt = min_binding_opt

end
