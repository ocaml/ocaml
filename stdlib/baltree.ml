(* Weight-balanced binary trees.
   These are binary trees such that one child of a node has at most N times
   as many elements as the other child. We take N=3. *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * int
        (* The type of trees containing elements of type ['a].
           [Empty] is the empty tree (containing no elements). *)

type 'a contents = Nothing | Something of 'a
        (* Used with the functions [modify] and [List.split], to represent
           the presence or the absence of an element in a tree. *)

(* Compute the size (number of nodes and leaves) of a tree. *)

let size = function
    Empty -> 1
  | Node(_, _, _, s) -> s

(* Creates a new node with left son l, value x and right son r.
   l and r must be balanced and size l / size r must be between 1/N and N.
   Inline expansion of size for better speed. *)

let new l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  Node(l, x, r, sl + sr + 1)

(* Same as new, but performs rebalancing if necessary.
   Assumes l and r balanced, and size l / size r "reasonable"
   (between 1/N^2 and N^2 ???).
   Inline expansion of new for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  if sl > 3 * sr then begin
    match l with
      Empty -> invalid_arg "Baltree.bal"
    | Node(ll, lv, lr, _) ->
        if size ll >= size lr then
          new ll lv (new lr x r)
        else begin
          match lr with
            Empty -> invalid_arg "Baltree.bal"
          | Node(lrl, lrv, lrr, _)->
              new (new ll lv lrl) lrv (new lrr x r)
        end
  end else if sr > 3 * sl then begin
    match r with
      Empty -> invalid_arg "Baltree.bal"
    | Node(rl, rv, rr, _) ->
        if size rr >= size rl then
          new (new l x rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Baltree.bal"
          | Node(rll, rlv, rlr, _) ->
              new (new l x rll) rlv (new rlr rv rr)
        end
  end else
    Node(l, x, r, sl + sr + 1)

(* Same as bal, but rebalance regardless of the original ratio
   size l / size r *)

let rec join l x r =
  match bal l x r with
    Empty -> invalid_arg "Baltree.join"
  | Node(l', x', r', _) as t' ->
      let sl = size l' and sr = size r' in
      if sl > 3 * sr or sr > 3 * sl then join l' x' r' else t'

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assumes size l / size r between 1/N and N. *)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      bal l1 v1 (bal (merge r1 l2) v2 r2)

(* Same as merge, but does not assume anything about l and r. *)

let rec concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      join l1 v1 (join (concat r1 l2) v2 r2)

(* Insertion *)

let add searchpred x t =
  let rec add = function
    Empty ->
      Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = searchpred v in
      if c == 0 then t else
      if c < 0 then bal (add l) v r else bal l v (add r)
  in add t

(* Membership *)

let contains searchpred t =
  let rec contains = function
    Empty -> false
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then true else
      if c < 0 then contains l else contains r
  in contains t

(* Search *)

let find searchpred t =
  let rec find = function
    Empty ->
      raise Not_found
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then v else
      if c < 0 then find l else find r
  in find t

(* Deletion *)

let remove searchpred t =
  let rec remove = function
    Empty ->
      Empty
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then merge l r else
      if c < 0 then bal (remove l) v r else bal l v (remove r)
  in remove t

(* Modification *)

let modify searchpred modifier t =
  let rec modify = function
    Empty ->
      begin match modifier Nothing with
        Nothing -> Empty
      | Something v -> Node(Empty, v, Empty, 1)
      end
  | Node(l, v, r, s) ->
      let c = searchpred v in
      if c == 0 then
        begin match modifier(Something v) with
          Nothing -> merge l r
        | Something v' -> Node(l, v', r, s)
        end
      else if c < 0 then bal (modify l) v r else bal l v (modify r)
  in modify t

(* Splitting *)

let split searchpred =
  let rec split = function
    Empty ->
      (Empty, Nothing, Empty)
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then (l, Something v, r)
      else if c < 0 then
        let (ll, vl, rl) = split l in (ll, vl, join rl v r)
      else
        let (lr, vr, rr) = split r in (join l v lr, vr, rr)
  in split

(* Comparison (by lexicographic ordering of the fringes of the two trees). *)

let compare cmp s1 s2 =
  let rec compare_aux l1 l2 =
      match (l1, l2) with
      ([], []) -> 0
    | ([], _)  -> -1
    | (_, []) -> 1
    | (Empty::t1, Empty::t2) ->
        compare_aux t1 t2
    | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
        let c = cmp v1 v2 in
        if c != 0 then c else compare_aux (r1::t1) (r2::t2)
    | (Node(l1, v1, r1, _) :: t1, t2) ->
        compare_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
    | (t1, Node(l2, v2, r2, _) :: t2) ->
        compare_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)
  in
    compare_aux [s1] [s2]
