(* Sets over ordered types *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * int

let empty = Empty

(* Compute the size (number of nodes and leaves) of a tree. *)

let size = function
    Empty -> 1
  | Node(_, _, _, s) -> s

(* Creates a new node with left son l, val x and right son r.
   l and r must be balanced and size l / size r must be between 1/N and N.
   Inline expansion of size for better speed. *)

let new l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  Node(l, x, r, sl + sr + 1)

(* Same as new, but performs rebalancing if necessary.
   Assumes l and r balanced, and size l / size r "reasonable".
   Inline expansion of new for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  if sl > 3 * sr then begin
    match l with
      Empty -> invalid_arg "Cset.bal"
    | Node(ll, lv, lr, _) ->
        if size ll >= size lr then
          new ll lv (new lr x r)
        else begin
          match lr with
            Empty -> invalid_arg "Cset.bal"
          | Node(lrl, lrv, lrr, _)->
              new (new ll lv lrl) lrv (new lrr x r)
        end
  end else if sr > 3 * sl then begin
    match r with
      Empty -> invalid_arg "Cset.bal"
    | Node(rl, rv, rr, _) ->
        if size rr >= size rl then
          new (new l x rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "Cset.bal"
          | Node(rll, rlv, rlr, _) ->
              new (new l x rll) rlv (new rlr rv rr)
        end
  end else
    Node(l, x, r, sl + sr + 1)

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assumes size l / size r between 1/N and N. *)

let rec merge l r =
  match (l, r) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      bal l1 v1 (bal (merge r1 l2) v2 r2)

(* Insertion *)

let rec add x = function
    Empty ->
      Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = compare x v in
      if c = 0 then t else
      if c < 0 then bal (add x l) v r else bal l v (add x r)

(* Membership *)

let rec mem x = function
    Empty ->
      false
  | Node(l, v, r, _) ->
      let c = compare x v in
      c = 0 or mem x (if c < 0 then l else r)

(* Removal *)

let rec remove x = function
    Empty ->
      Empty
  | Node(l, v, r, _) ->
      let c = compare x v in
      if c = 0 then merge l r else
      if c < 0 then bal (remove x l) v r else bal l v (remove x r)

(* Contents *)

let elements s =
  let rec elements accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements (v :: elements accu r) l
  in elements [] s
