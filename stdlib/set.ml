(* Sets over ordered types *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty: t
    val is_empty: t -> bool
    val mem: elt -> t -> bool
    val add: elt -> t -> t
    val remove: elt -> t -> t
    val union: t -> t -> t
    val inter: t -> t -> t
    val diff: t -> t -> t
    val compare: t -> t -> int
    val equal: t -> t -> bool
    val iter: (elt -> 'a) -> t -> unit
    val fold: (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val elements: t -> elt list
  end

module Make(Ord: OrderedType): (S with elt = Ord.t) =
  struct
    open Baltree
    type elt = Ord.t
    type t = elt Baltree.t

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let mem x s =
      Baltree.contains (Ord.compare x) s

    let add x s =
      Baltree.add (Ord.compare x) x s

    let remove x s =
      Baltree.remove (Ord.compare x) s

    let rec union s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, _), t2) ->
          let (l2, _, r2) = Baltree.split (Ord.compare v1) t2 in
          Baltree.join (union l1 l2) v1 (union r1 r2)

    let rec inter s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> Empty
      | (Node(l1, v1, r1, _), t2) ->
          match Baltree.split (Ord.compare v1) t2 with
            (l2, Nothing, r2) ->
              Baltree.concat (inter l1 l2) (inter r1 r2)
          | (l2, Something _, r2) ->
              Baltree.join (inter l1 l2) v1 (inter r1 r2)

    let rec diff s1 s2 =
      match (s1, s2) with
        (Empty, t2) -> Empty
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, _), t2) ->
          match Baltree.split (Ord.compare v1) t2 with
            (l2, Nothing, r2) ->
              Baltree.join (diff l1 l2) v1 (diff r1 r2)
          | (l2, Something _, r2) ->
              Baltree.concat (diff l1 l2) (diff r1 r2)

    let compare s1 s2 =
      Baltree.compare Ord.compare s1 s2

    let equal s1 s2 =
      compare s1 s2 = 0

    let rec iter f = function
        Empty -> ()
      | Node(l, v, r, _) -> iter f l; f v; iter f r

    let rec fold f s accu =
      match s with
        Empty -> accu
      | Node(l, v, r, _) -> fold f l (f v (fold f r accu))

    let rec elements_aux accu = function
        Empty -> accu
      | Node(l, v, r, _) -> elements_aux (v :: elements_aux accu r) l

    let elements s =
      elements_aux [] s

  end
