(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Sacha-Élie Ayoun, Soteria Tools Ltd.                  *)
(*                                                                        *)
(*   Copyright 2026, Soteria Tools Ltd.                                   *) 
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This file is largely inspired / copied from the implementation of Hashtbl.
   While hashsets can be implemented as ('a, unit) Hashtbl.t, this implementation
   is more efficient as it avoids the need to store the dummy value.
   
   Its interface is also adapted to the use case of sets. *)

type 'a bucketlist =
    Empty
  | Cons of { mutable key: 'a;
              mutable next: 'a bucketlist }   
   
type 'a t =
  { mutable size: int; 
    mutable data: 'a bucketlist array;
    seed: int;
    mutable initial_size: int;
  }

(* The sign of initial_size encodes the fact that a traversal is
   ongoing or not.

   This disables the efficient in place implementation of resizing.
*)

let ongoing_traversal h =
  Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
  || h.initial_size < 0

let flip_ongoing_traversal h =
  h.initial_size <- - h.initial_size

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
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = abs h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.data <- Array.make (abs h.initial_size) Empty
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {key; next} ->
      let rec loop prec = function
        | Empty -> ()
        | Cons {key; next} ->
            let r = Cons {key; next} in
            begin match prec with
            | Empty -> assert false
            | Cons prec ->  prec.next <- r
            end;
            loop r next
      in
      let r = Cons {key; next} in
      loop r next;
      r

let copy h = { h with data = Array.map copy_bucketlist h.data }

let length h = h.size

let insert_all_buckets indexfun inplace odata ndata =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons {key; next} as cell ->
        let cell =
          if inplace then cell
          else Cons {key; next = Empty}
        in
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
    h.data <- ndata;          (* so that indexfun sees the new bucket count *)
    insert_all_buckets (indexfun h) inplace odata ndata
  end

let iter f h =
  let rec do_bucket = function
    | Empty ->
        ()
    | Cons{key; next} ->
        f key; do_bucket next in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    for i = 0 to Array.length d - 1 do
      do_bucket d.(i)
    done;
    if not old_trav then flip_ongoing_traversal h;
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec filter_inplace_bucket f h i prec = function
  | Empty ->
      begin match prec with
      | Empty -> h.data.(i) <- Empty
      | Cons c -> c.next <- Empty
      end
  | (Cons {key; next}) as slot ->
      if f key then begin
        begin match prec with
        | Empty -> h.data.(i) <- slot
        | Cons c -> c.next <- slot
        end;
        filter_inplace_bucket f h i slot next
      end else begin
        h.size <- h.size - 1;
        filter_inplace_bucket f h i prec next
      end

let filter_inplace f h =
  let d = h.data in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to Array.length d - 1 do
      filter_inplace_bucket f h i Empty h.data.(i)
    done;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons{key; next} ->
        do_bucket next (f key accu) in
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    let d = h.data in
    let accu = ref init in
    for i = 0 to Array.length d - 1 do
      accu := do_bucket d.(i) !accu
    done;
    if not old_trav then flip_ongoing_traversal h;
    !accu
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let rec bucket_length accu = function
  | Empty -> accu
  | Cons{next} -> bucket_length (accu + 1) next

let stats h =
  let mbl =
    Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.data in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.data;
  Hashtbl.{ num_bindings = h.size;
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
        else aux(i+1) tbl_data.(i) ()
    | Cons {key; next} ->
        Seq.Cons (key, aux i next)
  in
  aux 0 Empty

(* Functorial interface *)

module type S =
  sig
    type elt
    type t
    val create: int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy: t -> t
    val add: t -> elt -> unit
    val remove: t -> elt -> unit
    val mem : t -> elt -> bool
    val iter: (elt -> unit) -> t -> unit
    val filter_inplace: (elt -> bool) -> t -> unit
    val fold: (elt -> 'b -> 'b) -> t -> 'b -> 'b
    val length: t -> int
    val stats: t -> Hashtbl.statistics
    val to_seq : t -> elt Seq.t
    val add_seq : t -> elt Seq.t -> unit
    val of_seq : elt Seq.t -> t
  end

module type SeededS =
  sig
    type elt
    type t
    val create : ?random:bool -> int -> t
    val clear : t -> unit
    val reset : t -> unit
    val copy : t -> t
    val add : t -> elt -> unit
    val remove : t -> elt -> unit
    val mem : t -> elt -> bool
    val iter : (elt -> unit) -> t -> unit
    val filter_inplace: (elt -> bool) -> t -> unit
    val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
    val length : t -> int
    val stats: t -> Hashtbl.statistics
    val to_seq : t -> elt Seq.t
    val add_seq : t -> elt Seq.t -> unit
    val of_seq : elt Seq.t -> t
  end

module MakeSeeded(H: Hashtbl.SeededHashedType): (SeededS with type elt = H.t) =
  struct
    type elt = H.t
    type hashset = elt t
    type t = hashset
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.seeded_hash h.seed key) land (Array.length h.data - 1)

    let rec remove_bucket h i key prec bucket =
      match bucket with
      | Empty ->
          bucket
      | Cons {key=k; next; _} ->
          if H.equal k key
          then begin
            h.size <- h.size - 1;
            begin match prec with
            | Empty -> h.data.(i) <- next
            | Cons c -> c.next <- next
            end;
            bucket
          end
          else remove_bucket h i key bucket next

    let remove h key =
      let i = key_index h key in
      ignore (remove_bucket h i key Empty h.data.(i))

    let rec retrieve_bucket key bucket =
      match bucket with
      | Empty ->
          bucket
      | Cons {key=k; next} ->
          if H.equal k key
          then bucket
          else retrieve_bucket key next

    let add_bucket h key i l = function
      | Empty ->
        h.data.(i) <- Cons{key; next=l};
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize key_index h
      | Cons slot -> slot.key <- key

    let add h key =
      let i = key_index h key in
      let l = h.data.(i) in
      let bucket = retrieve_bucket key l in
      add_bucket h key i l bucket

    (* Iterators *)

    let rec mem_in_bucket key = function
      | Empty ->
          false
      | Cons{key=k; next} ->
          H.equal k key || mem_in_bucket key next

    let mem h key =
      mem_in_bucket key h.data.(key_index h key)

    let add_seq tbl i =
      Seq.iter (fun k -> add tbl k) i

    let of_seq i =
      let tbl = create 16 in
      add_seq tbl i;
      tbl

    let iter = iter
    let filter_inplace = filter_inplace
    let fold = fold
    let length = length
    let stats = stats
    let to_seq = to_seq
  end

module Make(H: Hashtbl.HashedType): (S with type elt = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let seeded_hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      add_seq tbl i;
      tbl
  end

let key_index h key =
  if Obj.size (Obj.repr h) >= 4
  then (Hashtbl.seeded_hash_param 10 100 h.seed key) land (Array.length h.data - 1)
  else invalid_arg "Hashset: unsupported hash table format"

let rec remove_bucket h i key prec bucket =
  match bucket with
  | Empty ->
      bucket
  | Cons {key=k; next; _} ->
      if compare k key = 0
      then begin
        h.size <- h.size - 1;
        begin match prec with
        | Empty -> h.data.(i) <- next
        | Cons c -> c.next <- next
        end;
        bucket
      end
      else remove_bucket h i key bucket next

let remove h key =
  let i = key_index h key in
  ignore (remove_bucket h i key Empty h.data.(i))

let rec retrieve_bucket key bucket =
  match bucket with
  | Empty ->
      bucket
  | Cons {key=k; next} ->
      if compare k key = 0
      then bucket
      else retrieve_bucket key next

let add_bucket h key i l bucket =
  match bucket with
  | Empty ->
    h.data.(i) <- Cons{key; next=l};
    h.size <- h.size + 1;
    if h.size > Array.length h.data lsl 1 then resize key_index h
  | Cons (_ as slot) -> slot.key <- key

let add h key =
  let i = key_index h key in
  let l = h.data.(i) in
  let bucket = retrieve_bucket key l in
  add_bucket h key i l bucket

let rec mem_in_bucket key = function
  | Empty ->
      false
  | Cons{key=k; next} ->
      compare k key = 0 || mem_in_bucket key next

let mem h key =
  mem_in_bucket key h.data.(key_index h key)

let add_seq tbl i =
  Seq.iter (fun k -> add tbl k) i

let of_seq i =
  let tbl = create 16 in
  add_seq tbl i;
  tbl

let rebuild ?(random = Atomic.get randomized) h =
  let s = power_2_above 16 (Array.length h.data) in
  let seed =
    if random then Random.State.bits (Domain.DLS.get prng_key)
    else if Obj.size (Obj.repr h) >= 4 then h.seed
    else 0 in
  let h' = {
    size = h.size;
    data = Array.make s Empty;
    seed = seed;
    initial_size = if Obj.size (Obj.repr h) >= 4 then h.initial_size else s
  } in
  insert_all_buckets (key_index h') false h.data h'.data;
  h'
