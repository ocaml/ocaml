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

(* Hash tables *)

(* We do dynamic hashing, and resize the table and rehash the elements
   when the load factor becomes too high. *)

type ('a, 'b) t = {
  mutable size: int;                        (* number of entries *)
  mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
  seed: int;                                (* for randomization *)
  mutable initial_size: int;                (* initial array size *)
}

and ('a, 'b) bucketlist =
  | Empty
  | Cons of {
    mutable key: 'a;
    mutable data: 'b;
    mutable next: ('a, 'b) bucketlist;
  }

(* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in-place implementation of resizing. *)

let is_old_hashtbl h =
  Obj.size (Obj.repr h) < 4

let ongoing_traversal h =
  is_old_hashtbl h (* compatibility with old hash tables *)
  || h.initial_size < 0

let flip_ongoing_traversal h =
  h.initial_size <- - h.initial_size

let[@inline] protect_traversal h f =
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let result = f () in
    if not old_trav then flip_ongoing_traversal h;
    result
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

(* To pick random seeds if requested *)

let randomized_default =
  let params =
    try Sys.getenv "OCAMLRUNPARAM" with Not_found ->
    try Sys.getenv "CAMLRUNPARAM" with Not_found -> "" in
  String.contains params 'R'

let randomized = Atomic.make randomized_default

let randomize () = Atomic.set randomized true
let is_randomized () = Atomic.get randomized

let prng_key = Domain.DLS.new_key Random.State.make_self_init

(* Functions which appear before the functorial interface must either be
   independent of the hash function or take it as a parameter (see #2202 and
   code below the functor definitions. *)

(* Creating a fresh, empty table *)

let rec power_2_above x n =
  if x >= n then x
  else if x * 2 > Sys.max_array_length then x
  else power_2_above (x * 2) n

let create ?(random = Atomic.get randomized) initial_size =
  let s = power_2_above 16 initial_size in
  let seed =
    if random then Random.State.bits (Domain.DLS.get prng_key) else 0
  in
  { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

let clear h =
  if h.size > 0 then begin
    h.size <- 0;
    Array.fill h.data 0 (Array.length h.data) Empty
  end

let reset h =
  let len = Array.length h.data in
  if is_old_hashtbl h (* compatibility with old hash tables *)
  || len = abs h.initial_size
  then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {key; data; next} ->
    let rec loop prev = function
      | Empty -> ()
      | Cons {key; data; next} ->
        let r = Cons {key; data; next} in
        begin match prev with
        | Empty -> assert false
        | Cons prev ->  prev.next <- r
        end;
        loop r next
    in
    let r = Cons {key; data; next} in
    loop r next;
    r

let copy h = { h with data = Array.map copy_bucketlist h.data }

let length h = h.size

let insert_all_buckets indexfun inplace odata ndata =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons {key; data; next} as cell ->
      let cell = if inplace then cell else Cons {key; data; next = Empty} in
      let nidx = indexfun key in
      begin match ndata_tail.(nidx) with
      | Empty -> ndata.(nidx) <- cell;
      | Cons tail -> tail.next <- cell;
      end;
      ndata_tail.(nidx) <- cell;
      insert_bucket next
  in
  for i = 0 to Array.length odata - 1 do
    insert_bucket odata.(i)
  done;
  if inplace then
    for i = 0 to nsize - 1 do
      match ndata_tail.(i) with
      | Empty -> ()
      | Cons tail -> tail.next <- Empty
    done

let resize indexfun h =
  let odata = h.data in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.data <- ndata;  (* so that indexfun sees the new bucket count *)
    insert_all_buckets (indexfun h) inplace odata ndata
  end

let iter f h =
  let rec do_bucket = function
    | Empty -> ()
    | Cons {key; data; next} -> f key data; do_bucket next
  in
  protect_traversal h (fun () ->
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done
  )

let rec filter_map_inplace_bucket f h i prev = function
  | Empty ->
    begin match prev with
    | Empty -> h.data.(i) <- Empty
    | Cons c -> c.next <- Empty
    end
  | (Cons ({key; data; next} as c)) as slot ->
    begin match f key data with
    | None ->
      h.size <- h.size - 1;
      filter_map_inplace_bucket f h i prev next
    | Some data ->
      begin match prev with
      | Empty -> h.data.(i) <- slot
      | Cons c -> c.next <- slot
      end;
      c.data <- data;
      filter_map_inplace_bucket f h i slot next
    end

let filter_map_inplace f h =
  protect_traversal h (fun () ->
    let n = Array.length h.data in
    for i = 0 to n - 1 do
      filter_map_inplace_bucket f h i Empty h.data.(i)
    done
  )

let fold f h init =
  let rec do_bucket acc = function
    | Empty -> acc
    | Cons {key; data; next} -> do_bucket (f key data acc) next
  in
  protect_traversal h (fun () ->
    Array.fold_left do_bucket init h.data
  )

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array;
}

let bucket_length b =
  let rec loop acc = function
    | Empty -> acc
    | Cons {next} -> loop (acc + 1) next
  in
  loop 0 b

let stats h =
  let mbl =
    Array.fold_left (fun m b -> Int.max m (bucket_length b)) 0 h.data
  in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter (fun b ->
    let l = bucket_length b in
    histo.(l) <- histo.(l) + 1
  ) h.data;
  { num_bindings = h.size;
    num_buckets = Array.length h.data;
    max_bucket_length = mbl;
    bucket_histogram = histo }

(** {1 Iterators} *)

let to_seq tbl =
  (* capture current array, so that even if the table is resized we
     keep iterating on the same array *)
  let tbl_data = tbl.data in
  (* state: index * next bucket to traverse *)
  let rec aux i buck () = match buck with
    | Empty ->
      if i = Array.length tbl_data
      then Seq.Nil
      else aux (i+1) tbl_data.(i) ()
    | Cons {key; data; next} ->
      Seq.Cons ((key, data), aux i next)
  in
  aux 0 Empty

let to_seq_keys m = Seq.map fst (to_seq m)

let to_seq_values m = Seq.map snd (to_seq m)

(* Implementation of functions dependent on key hash and equality *)
module Impl (M: sig
  type 'a key
  val key_index : ('a key, 'b) t -> 'a key -> int
  val key_equal : 'a key -> 'a key -> bool
end) = struct
  let key_index = M.key_index
  let key_equal = M.key_equal

  let maybe_resize h =
    if h.size > Array.length h.data lsl 1 then resize key_index h

  (* Find slot of key `k`, if it exists. Then, call continuation
     `f h k x i prev curr`, where `h` and `k` and `x` are as passed, `i` is
     the index of the bucket for `k`, `curr` is the first slot with key `k`
     (or Empty if no such slot exists), and `prev` is the previous slot (or
     Empty if `curr` is the head of the bucket). *)
  let[@inline] find_slot h k d f =
    let rec loop h k d i prev curr f =
      match curr with
      | Empty ->
        f h k d i prev curr
      | Cons {key; data=_; next} ->
        if key_equal key k
        then f h k d i prev curr
        else (loop[@tailcall]) h k d i curr next f
    in
    let i = key_index h k in
    let curr = h.data.(i) in
    loop h k d i Empty curr f

  let add h k d =
    let i = key_index h k in
    let bucket = Cons{key=k; data=d; next=h.data.(i)} in
    h.data.(i) <- bucket;
    h.size <- h.size + 1;
    maybe_resize h

  let replace h k d =
    find_slot h k d (fun h k d i _ curr ->
      match curr with
      | Empty ->
        h.data.(i) <- Cons{key=k; data=d; next=h.data.(i)};
        h.size <- h.size + 1;
        maybe_resize h
      | Cons curr ->
        curr.key <- k;
        curr.data <- d
    )

  let remove h k =
    find_slot h k () (fun h _ _ i prev curr ->
      match curr with
      | Empty -> ()
      | Cons curr ->
        begin match prev with
        | Empty -> h.data.(i) <- curr.next;
        | Cons prev -> prev.next <- curr.next;
        end;
        h.size <- h.size - 1
    )

  let find h k =
    find_slot h k () (fun _ _ _ _ _ curr ->
      match curr with
      | Empty -> raise Not_found
      | Cons curr -> curr.data
    )

  let find_opt h k =
    find_slot h k () (fun _ _ _ _ _ curr ->
      match curr with
      | Empty -> None
      | Cons curr -> Some curr.data
    )

  let find_all h k =
    let[@tail_mod_cons] rec find_in_bucket = function
      | Empty -> []
      | Cons {key; data; next} ->
        if key_equal key k
        then data :: find_in_bucket next
        else find_in_bucket next
    in
    find_in_bucket h.data.(key_index h k)

  let mem h k =
    find_slot h k () (fun _ _ _ _ _ curr ->
      match curr with
      | Empty -> false
      | Cons _ -> true
    )

  let add_seq tbl i =
    Seq.iter (fun (k,v) -> add tbl k v) i

  let replace_seq tbl i =
    Seq.iter (fun (k,v) -> replace tbl k v) i

  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl
end

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type SeededHashedType =
  sig
    type t
    val equal: t -> t -> bool
    val seeded_hash: int -> t -> int
  end

module type S = sig
  type key
  type !'a t
  val create: int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val find: 'a t -> key -> 'a
  val find_opt: 'a t -> key -> 'a option
  val find_all: 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
  val stats: 'a t -> statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : _ t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module type SeededS = sig
  type key
  type !'a t
  val create : ?random:bool -> int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy: 'a t -> 'a t
  val add: 'a t -> key -> 'a -> unit
  val remove: 'a t -> key -> unit
  val find: 'a t -> key -> 'a
  val find_opt: 'a t -> key -> 'a option
  val find_all: 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length: 'a t -> int
  val stats: 'a t -> statistics
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_keys : _ t -> key Seq.t
  val to_seq_values : 'a t -> 'a Seq.t
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
end

module MakeSeeded (H: SeededHashedType): (SeededS with type key = H.t) = struct
  type key = H.t
  type 'a hashtbl = (key, 'a) t
  type 'a t = 'a hashtbl
  let create = create
  let clear = clear
  let reset = reset
  let copy = copy

  include Impl(struct
    type 'a key = H.t

    let key_index h k =
      (H.seeded_hash h.seed k) land (Array.length h.data - 1)

    let key_equal = H.equal
  end)

  let iter = iter
  let filter_map_inplace = filter_map_inplace
  let fold = fold
  let length = length
  let stats = stats
  let to_seq = to_seq
  let to_seq_keys = to_seq_keys
  let to_seq_values = to_seq_values
end

module Make (H: HashedType): (S with type key = H.t) = struct
  include MakeSeeded(struct
      type t = H.t
      let equal = H.equal
      let seeded_hash (_seed: int) x = H.hash x
    end)
  let create sz = create ~random:false sz
  let of_seq i =
    let tbl = create 16 in
    replace_seq tbl i;
    tbl
end

(* Polymorphic hash function-based tables *)
(* Code included below the functorial interface to guard against accidental
   use - see #2202 *)

external seeded_hash_param :
  int -> int -> int -> 'a -> int = "caml_hash" [@@noalloc]

let hash x = seeded_hash_param 10 100 0 x
let hash_param n1 n2 x = seeded_hash_param n1 n2 0 x
let seeded_hash seed x = seeded_hash_param 10 100 seed x

include Impl(struct
  type 'a key = 'a

  let key_index h key =
    if not (is_old_hashtbl h)
    then (seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)
    else invalid_arg "Hashtbl: unsupported hash table format"

  let key_equal x y =
    compare x y = 0
end)

let rebuild ?(random = Atomic.get randomized) h =
  let s = power_2_above 16 (Array.length h.data) in
  let seed =
    if random then Random.State.bits (Domain.DLS.get prng_key)
    else if not (is_old_hashtbl h) then h.seed
    else 0 in
  let h' = {
    size = h.size;
    data = Array.make s Empty;
    seed = seed;
    initial_size = if not (is_old_hashtbl h) then h.initial_size else s
  } in
  insert_all_buckets (key_index h') false h.data h'.data;
  h'
