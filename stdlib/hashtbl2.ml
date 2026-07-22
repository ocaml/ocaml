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

type ('a, 'b) t =
  { mutable size: int;                  (* number of entries *)
    mutable buckets: bucketlist array;  (* the buckets *)
    keys: 'a Dynarray.t;
    data: 'b Dynarray.t;
    seed: int;                          (* for randomization *)
    mutable initial_size: int;          (* initial array size *)
  }

and bucketlist =
    Empty
  | Cons of { mutable id: int;          (* unique identifier *)
              mutable next: bucketlist }

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

(* The runtime stores the initial value of "R" in
   caml_runtime_hashtbl_randomized. We choose to copy this initial value here
   and then keep then in sync in order to avoid adding a C call to every call to
   Hashtbl.create. *)
external randomized : unit -> bool =
  "caml_runtime_hashtbl_is_randomized" [@@noalloc]
let randomized = Atomic.make (randomized ())

external randomize : unit -> unit = "caml_runtime_hashtbl_randomize" [@@noalloc]
let randomize () =
  Atomic.set randomized true;
  (* Update the runtime's value so that the result from Sys.runtime_parameters
     includes "R". There is technically a race here where Hashtbl.create ()
     creates randomized hash tables, but Sys.runtime_parameters doesn't yet
     return R=1. We choose not to care - Hashtbl.is_randomized will always
     return the correct value, and making Sys.runtime_parameters always be in
     sync would either add a C call to every Hashtbl.create call or would
     introduce a complicated dependency cycle between Sys and Hashtbl *)
  randomize ()

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
  { initial_size = s; size = 0; seed = seed; buckets = Array.make s Empty;
    data = Dynarray.create (); keys = Dynarray.create () }

let clear h =
  if h.size > 0 then begin
    h.size <- 0;
    Array.fill h.buckets 0 (Array.length h.buckets) Empty;
    Dynarray.clear h.data; Dynarray.clear h.keys
  end

let reset h =
  let len = Array.length h.buckets in
  if Obj.size (Obj.repr h) < 4 (* compatibility with old hash tables *)
    || len = abs h.initial_size then
    clear h
  else begin
    h.size <- 0;
    h.buckets <- Array.make (abs h.initial_size) Empty;
    Dynarray.reset h.data; Dynarray.reset h.keys
  end

let copy_bucketlist = function
  | Empty -> Empty
  | Cons {id; next} ->
      let rec loop prec = function
        | Empty -> ()
        | Cons {id; next} ->
            let r = Cons {id; next} in
            begin match prec with
            | Empty -> assert false
            | Cons prec ->  prec.next <- r
            end;
            loop r next
      in
      let r = Cons {id; next} in
      loop r next;
      r

let copy h = { h with buckets = Array.map copy_bucketlist h.buckets;
                data = Dynarray.copy h.data; keys = Dynarray.copy h.keys }

let length h = h.size

let insert_all_buckets indexfun inplace odata ndata keys =
  let nsize = Array.length ndata in
  let ndata_tail = Array.make nsize Empty in
  let rec insert_bucket = function
    | Empty -> ()
    | Cons {id; next} as cell ->
        let cell =
          if inplace then cell
          else Cons {id; next = Empty}
        in
        let nidx = indexfun (Dynarray.get keys id) in
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
  let odata = h.buckets in
  let osize = Array.length odata in
  let nsize = osize * 2 in
  if nsize < Sys.max_array_length then begin
    let ndata = Array.make nsize Empty in
    let inplace = not (ongoing_traversal h) in
    h.buckets <- ndata;          (* so that indexfun sees the new bucket count *)
    insert_all_buckets (indexfun h) inplace odata ndata h.keys
  end

let iter f h =
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    for i = 0 to h.size - 1 do
      f (Dynarray.get h.keys i) (Dynarray.get h.data i)
    done;
    if not old_trav then flip_ongoing_traversal h;
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let replace_bucket_id ~key_index h ~key ~prev_id ~new_id =
  let rec find_bucket = function
  | Empty -> ()
  | Cons c ->
    if c.id = prev_id then c.id <- new_id
    else find_bucket c.next
  in find_bucket h.buckets.(key_index h key)

(* removes the bucket containing id *)
let remove_bucket ~key_index h ~key ~id =
  let i = key_index h key in
  let rec find_bucket prec = function
  | Empty -> ()
  | (Cons {id = prev; next}) as slot ->
    if prev = id then
      match prec with
      | Empty -> h.buckets.(i) <- Empty
      | Cons c -> c.next <- next
    else find_bucket slot next
  in find_bucket Empty h.buckets.(i)

(* function that iterates on ids *)
let rec filter_map_inplace_id f ~key_index h ~read ~write =
  if read = h.size then begin
    h.size <- write;
    if write <> read then
      for i = read - 1 to write do
        remove_bucket ~key_index h ~key:(Dynarray.get h.keys i) ~id:i;
        Dynarray.remove_last h.keys;
        Dynarray.remove_last h.data
      done
  end
  else
    match f (Dynarray.get h.keys read) (Dynarray.get h.data read) with
    | None ->
      remove_bucket ~key_index h ~key:(Dynarray.get h.keys write) ~id:write;
      filter_map_inplace_id f ~key_index h ~read:(read + 1) ~write
    | Some data ->
      if write <> read then begin
        Dynarray.set h.keys write (Dynarray.get h.keys read);
        replace_bucket_id ~key_index h ~key:(Dynarray.get h.keys read)
        ~prev_id:read ~new_id:write
      end;
      Dynarray.set h.data write data;
      filter_map_inplace_id f ~key_index h ~read:(read + 1) ~write:(write + 1)

let filter_map_inplace f ~key_index h =
  let old_trav = ongoing_traversal h in
  if not old_trav then flip_ongoing_traversal h;
  try
    filter_map_inplace_id f ~key_index h ~read:0 ~write:0;
    if not old_trav then flip_ongoing_traversal h
  with exn when not old_trav ->
    flip_ongoing_traversal h;
    raise exn

let fold f h init =
  let rec fold_aux i accu =
    if i = h.size then accu else
      fold_aux (i + 1)
        (f (Dynarray.get h.keys i) (Dynarray.get h.data i) accu) in
    let old_trav = ongoing_traversal h in
    if not old_trav then flip_ongoing_traversal h;
    try
      let accu = ref init in
      accu := fold_aux 0 !accu;
      if not old_trav then flip_ongoing_traversal h;
      !accu
    with exn when not old_trav -> flip_ongoing_traversal h;
    raise exn

type statistics = {
  num_bindings: int;
  num_buckets: int;
  max_bucket_length: int;
  bucket_histogram: int array
}

let rec bucket_length accu = function
  | Empty -> accu
  | Cons {next} -> bucket_length (accu + 1) next

let stats h =
  let mbl =
    Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.buckets in
  let histo = Array.make (mbl + 1) 0 in
  Array.iter
    (fun b ->
      let l = bucket_length 0 b in
      histo.(l) <- histo.(l) + 1)
    h.buckets;
  { num_bindings = h.size;
    num_buckets = Array.length h.buckets;
    max_bucket_length = mbl;
    bucket_histogram = histo }

(** {1 Iterators} *)

let to_seq tbl =
  let rec aux i () =
    if i = Dynarray.length tbl.data then Seq.Nil
    else Seq.Cons (((Dynarray.get tbl.keys i), (Dynarray.get tbl.data i)),
                    aux (i+1))
  in aux 0

let to_seq_keys m = Seq.map fst (to_seq m)

let to_seq_values m = Seq.map snd (to_seq m)

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

module type S =
  sig
    type key
    type !'a t
    val create: int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find_and_remove: 'a t -> key -> 'a option
    val find: 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val find_and_replace : 'a t -> key -> 'a -> 'a option
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

module type SeededS =
  sig
    type key
    type !'a t
    val create : ?random:bool -> int -> 'a t
    val clear : 'a t -> unit
    val reset : 'a t -> unit
    val copy : 'a t -> 'a t
    val add : 'a t -> key -> 'a -> unit
    val remove : 'a t -> key -> unit
    val find_and_remove : 'a t -> key -> 'a option
    val find : 'a t -> key -> 'a
    val find_opt: 'a t -> key -> 'a option
    val find_all : 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val find_and_replace :'a t -> key -> 'a -> 'a option
    val mem : 'a t -> key -> bool
    val iter : (key -> 'a -> unit) -> 'a t -> unit
    val filter_map_inplace: (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length : 'a t -> int
    val stats: 'a t -> statistics
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_keys : _ t -> key Seq.t
    val to_seq_values : 'a t -> 'a Seq.t
    val add_seq : 'a t -> (key * 'a) Seq.t -> unit
    val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module MakeSeeded(H: SeededHashedType): (SeededS with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let reset = reset
    let copy = copy

    let key_index h key =
      (H.seeded_hash h.seed key) land (Array.length h.buckets - 1)

    let add h key data =
      let i = key_index h key in
      let bucket = Cons {id = h.size; next=h.buckets.(i)} in
      h.buckets.(i) <- bucket;
      h.size <- h.size + 1;
      Dynarray.add_last h.data data;
      Dynarray.add_last h.keys key;
      if h.size > Dynarray.length h.data lsl 1 then resize key_index h

    let rec remove_bucket h i key prec bucket =
      match bucket with
      | Empty ->
          None
      | Cons {id; next} ->
          if H.equal (Dynarray.get h.keys id) key
          then let data = Dynarray.get h.data id in begin
            h.size <- h.size - 1;
            if Dynarray.length h.data < 2 || id = h.size then begin
              Dynarray.remove_last h.data;
              Dynarray.remove_last h.keys
            end
            else begin
              Dynarray.set h.data id (Dynarray.pop_last h.data);
              Dynarray.set h.keys id (Dynarray.pop_last h.keys);
              replace_bucket_id ~key_index h ~key:(Dynarray.get h.keys id)
                ~prev_id:h.size ~new_id:id
            end;
            begin match prec with
            | Empty -> h.buckets.(i) <- next
            | Cons c -> c.next <- next
            end;
            Some data
          end
          else remove_bucket h i key bucket next

    let find_and_remove h key =
      let i = key_index h key in remove_bucket h i key Empty h.buckets.(i)

    let remove h key =
      let i = key_index h key in
      ignore (remove_bucket h i key Empty h.buckets.(i))

    let rec find_rec h key = function
      | Empty ->
          raise Not_found
      | Cons {id; next} ->
          if H.equal key (Dynarray.get h.keys id) then Dynarray.get h.data id
          else find_rec h key next

    let find h key =
      match h.buckets.(key_index h key) with
      | Empty -> raise Not_found
      | Cons {id = id1; next = next1} ->
        if H.equal key (Dynarray.get h.keys id1) then
          Dynarray.get h.data id1
        else match next1 with
        | Empty -> raise Not_found
        | Cons {id = id2; next = next2} ->
          if H.equal key (Dynarray.get h.keys id2) then
            Dynarray.get h.data id2
          else match next2 with
          | Empty -> raise Not_found
          | Cons {id = id3; next = next3} ->
            if H.equal key (Dynarray.get h.keys id3) then
              Dynarray.get h.data id3
            else find_rec h key next3

    let rec find_rec_opt h key = function
    | Empty -> None
    | Cons {id; next} ->
      if H.equal key (Dynarray.get h.keys id) then
        Some (Dynarray.get h.data id)
      else find_rec_opt h key next

    let find_opt h key =
      match h.buckets.(key_index h key) with
      | Empty -> None
      | Cons {id = id1; next = next1} ->
          if H.equal key (Dynarray.get h.keys id1) then
            Some (Dynarray.get h.data id1)
          else match next1 with
          | Empty -> None
          | Cons {id = id2; next = next2} ->
              if H.equal key (Dynarray.get h.keys id2) then
                Some (Dynarray.get h.data id2)
              else match next2 with
              | Empty -> None
              | Cons {id = id3; next = next3} ->
                  if H.equal key (Dynarray.get h.keys id3) then
                    Some (Dynarray.get h.data id3)
                  else find_rec_opt h key next3

    let find_all h key =
      let[@tail_mod_cons] rec find_in_bucket = function
      | Empty ->
          []
      | Cons {id; next} ->
          if H.equal (Dynarray.get h.keys id) key
          then Dynarray.get h.data id :: find_in_bucket next
          else find_in_bucket next in
      find_in_bucket h.buckets.(key_index h key)

    let rec retrieve_bucket h key bucket =
      match bucket with
      | Empty ->
          bucket
      | Cons {id; next} ->
          if H.equal (Dynarray.get h.keys id) key
          then bucket
          else retrieve_bucket h key next

    let replace_bucket h key i l data = function
      | Empty ->
        h.buckets.(i) <- Cons {id = h.size; next = l};
        h.size <- h.size + 1;
        Dynarray.add_last h.data data;
        Dynarray.add_last h.keys key;
        if h.size > Array.length h.buckets lsl 1 then resize key_index h
      | Cons {id; _} ->
        Dynarray.set h.keys id key;
        Dynarray.set h.data id data

    let find_and_replace h key data =
      let i = key_index h key in
      let l = h.buckets.(i) in
      let bucket = retrieve_bucket h key l in
      let old_data = match bucket with
        | Cons {id; _} -> Some (Dynarray.get h.data id)
        | Empty -> None
      in
      replace_bucket h key i l data bucket;
      old_data

    let replace h key data =
      let i = key_index h key in
      let l = h.buckets.(i) in
      let bucket = retrieve_bucket h key l in
      replace_bucket h key i l data bucket

    (* Iterators *)

    let rec mem_in_bucket h key = function
      | Empty ->
          false
      | Cons {id; next} ->
          H.equal (Dynarray.get h.keys id) key || mem_in_bucket h key next

    let mem h key =
      mem_in_bucket h key h.buckets.(key_index h key)

    let add_seq tbl i =
      Seq.iter (fun (k,v) -> add tbl k v) i

    let replace_seq tbl i =
      Seq.iter (fun (k,v) -> replace tbl k v) i

    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl

    let iter = iter
    let filter_map_inplace = filter_map_inplace ~key_index
    let fold = fold
    let length = length
    let stats = stats
    let to_seq = to_seq
    let to_seq_keys = to_seq_keys
    let to_seq_values = to_seq_values
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
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

let key_index h key =
  if Obj.size (Obj.repr h) >= 4
  then (seeded_hash_param 10 100 h.seed key) land (Array.length h.buckets - 1)
  else invalid_arg "Hashtbl: unsupported hash table format"

let add h key data =
  let i = key_index h key in
  let bucket = Cons {id = h.size; next=h.buckets.(i)} in
  h.buckets.(i) <- bucket;
  h.size <- h.size + 1;
  Dynarray.add_last h.data data;
  Dynarray.add_last h.keys key;
  if h.size > Array.length h.buckets lsl 1 then resize key_index h

let rec remove_bucket h i key prec bucket =
  match bucket with
  | Empty ->
      None
  | Cons {id; next} ->
      if compare (Dynarray.get h.keys id) key = 0
      then let data = Dynarray.get h.data id in begin
        h.size <- h.size - 1;
        if Dynarray.length h.data < 2 || id = h.size then begin
              Dynarray.remove_last h.data;
              Dynarray.remove_last h.keys
        end
            else begin
              Dynarray.set h.data id (Dynarray.pop_last h.data);
              Dynarray.set h.keys id (Dynarray.pop_last h.keys);
              replace_bucket_id ~key_index h ~key:(Dynarray.get h.keys id)
                ~prev_id:h.size ~new_id:id
            end;
        begin match prec with
        | Empty -> h.buckets.(i) <- next
        | Cons c -> c.next <- next
        end;
        Some data
      end
      else remove_bucket h i key bucket next

let find_and_remove h key =
  let i = key_index h key in remove_bucket h i key Empty h.buckets.(i)

let remove h key =
  let i = key_index h key in
  ignore (remove_bucket h i key Empty h.buckets.(i))

let filter_map_inplace = filter_map_inplace ~key_index

let rec find_rec h key = function
  | Empty ->
      raise Not_found
  | Cons {id; next} ->
      if compare key (Dynarray.get h.keys id) = 0 then Dynarray.get h.data id
      else find_rec h key next

let find h key =
  match h.buckets.(key_index h key) with
  | Empty -> raise Not_found
  | Cons {id = id1; next = next1} ->
      if compare key (Dynarray.get h.keys id1) = 0 then
        Dynarray.get h.data id1
      else match next1 with
      | Empty -> raise Not_found
      | Cons {id = id2; next = next2} ->
          if compare key (Dynarray.get h.keys id2) = 0 then
            Dynarray.get h.data id2
          else match next2 with
          | Empty -> raise Not_found
          | Cons {id = id3; next = next3} ->
              if compare key (Dynarray.get h.keys id3) = 0 then
                Dynarray.get h.data id3
              else find_rec h key next3

let rec find_rec_opt h key = function
| Empty -> None
| Cons {id; next} ->
  if compare key (Dynarray.get h.keys id) = 0 then
    Some (Dynarray.get h.data id)
  else find_rec_opt h key next

let find_opt h key =
  match h.buckets.(key_index h key) with
  | Empty -> None
  | Cons {id = id1; next = next1} ->
      if compare key (Dynarray.get h.keys id1) = 0 then
        Some (Dynarray.get h.data id1)
      else match next1 with
      | Empty -> None
      | Cons {id = id2; next = next2} ->
          if compare key (Dynarray.get h.keys id2) = 0 then
            Some (Dynarray.get h.data id2)
          else match next2 with
          | Empty -> None
          | Cons {id = id3; next = next3} ->
              if compare key (Dynarray.get h.keys id3) = 0 then
                Some (Dynarray.get h.data id3)
              else find_rec_opt h key next3

let find_all h key =
  let[@tail_mod_cons] rec find_in_bucket = function
  | Empty ->
      []
  | Cons {id; next} ->
      if compare (Dynarray.get h.keys id) key = 0
      then Dynarray.get h.data id :: find_in_bucket next
      else find_in_bucket next in
  find_in_bucket h.buckets.(key_index h key)

let rec retrieve_bucket h key bucket =
  match bucket with
  | Empty ->
      bucket
  | Cons {id; next} ->
      if compare (Dynarray.get h.keys id) key = 0
      then bucket
      else retrieve_bucket h key next

let replace_bucket h key i l data bucket =
  match bucket with
  | Empty ->
    h.buckets.(i) <- Cons {id = h.size; next=l};
    h.size <- h.size + 1;
    Dynarray.add_last h.data data;
    Dynarray.add_last h.keys key;
    if h.size > Array.length h.buckets lsl 1 then resize key_index h
  | Cons (_ as slot) ->
    Dynarray.set h.keys slot.id key;
    Dynarray.set h.data slot.id data

let find_and_replace h key data =
  let i = key_index h key in
  let l = h.buckets.(i) in
  let bucket = retrieve_bucket h key l in
  let old_data = match bucket with
    | Empty -> None
    | Cons {id; _} -> Some (Dynarray.get h.data id)
  in
  replace_bucket h key i l data bucket;
  old_data

let replace h key data =
  let i = key_index h key in
  let l = h.buckets.(i) in
  let bucket = retrieve_bucket h key l in
  replace_bucket h key i l data bucket

let rec mem_in_bucket h key = function
  | Empty ->
      false
  | Cons {id; next} ->
      compare (Dynarray.get h.keys id) key = 0 || mem_in_bucket h key next

let mem h key =
  mem_in_bucket h key h.buckets.(key_index h key)

let add_seq tbl i =
  Seq.iter (fun (k,v) -> add tbl k v) i

let replace_seq tbl i =
  Seq.iter (fun (k,v) -> replace tbl k v) i

let of_seq i =
  let tbl = create 16 in
  replace_seq tbl i;
  tbl

let rebuild ?(random = Atomic.get randomized) h =
  let s = power_2_above 16 (Array.length h.buckets) in
  let seed =
    if random then Random.State.bits (Domain.DLS.get prng_key)
    else if Obj.size (Obj.repr h) >= 4 then h.seed
    else 0 in
  let h' = {
    size = h.size;
    buckets = Array.make s Empty;
    data = Dynarray.copy h.data;
    keys = Dynarray.copy h.keys;
    seed = seed;
    initial_size = if Obj.size (Obj.repr h) >= 4 then h.initial_size else s
  } in
  insert_all_buckets (key_index h') false h.buckets h'.buckets h'.keys;
  h'
