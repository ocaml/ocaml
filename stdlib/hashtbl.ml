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

(* Hash tables *)

external hash_param : int -> int -> 'a -> int = "hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable max_len: int;                     (* max length of a bucket *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of 'a * 'b * ('a, 'b) bucketlist

let create initial_size =
  if initial_size <= 0 then invalid_arg "Hashtbl.create" else
  { max_len = 3; data = Array.create initial_size Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done

let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = 2 * osize + 1 in
  let ndata = Array.create nsize Empty in
  let rec insert_bucket = function
      Empty -> ()
    | Cons(key, data, rest) ->
        insert_bucket rest; (* preserve original order of elements *)
        let nidx = (hashfun key) mod nsize in
        ndata.(nidx) <- Cons(key, data, ndata.(nidx)) in
  for i = 0 to osize - 1 do
    insert_bucket odata.(i)
  done;
  tbl.data <- ndata;
  tbl.max_len <- 2 * tbl.max_len
          
let rec bucket_too_long n bucket =
  if n < 0 then true else
    match bucket with
      Empty -> false
    | Cons(_,_,rest) -> bucket_too_long (n - 1) rest

let add h key info =
  let i = (hash_param 10 100 key) mod (Array.length h.data) in
  let bucket = Cons(key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  if bucket_too_long h.max_len bucket then resize hash h

let remove h key =
  let rec remove_bucket = function
      Empty ->
        Empty
    | Cons(k, i, next) ->
        if k = key then next else Cons(k, i, remove_bucket next) in
  let i = (hash_param 10 100 key) mod (Array.length h.data) in
  h.data.(i) <- remove_bucket h.data.(i)

let find h key =
  match h.data.((hash_param 10 100 key) mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(k1, d1, rest1) ->
      if key = k1 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(k2, d2, rest2) ->
          if key = k2 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(k3, d3, rest3) ->
              if key = k3 then d3 else begin
                let rec find = function
                    Empty ->
                      raise Not_found
                  | Cons(k, d, rest) ->
                      if key = k then d else find rest
                in find rest3
              end

let find_all h key =
  let rec find_in_bucket = function
    Empty ->
      []
  | Cons(k, d, rest) ->
      if k = key then d :: find_in_bucket rest else find_in_bucket rest in
  find_in_bucket h.data.((hash_param 10 100 key) mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val iter: (key -> 'a -> unit) -> 'a t -> unit
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    
    let add h key info =
      let i = (H.hash key) mod (Array.length h.data) in
      let bucket = Cons(key, info, h.data.(i)) in
      h.data.(i) <- bucket;
      if bucket_too_long h.max_len bucket then resize H.hash h

    let remove h key =
      let rec remove_bucket = function
          Empty ->
            Empty
        | Cons(k, i, next) ->
            if H.equal k key
            then next
            else Cons(k, i, remove_bucket next) in
      let i = (H.hash key) mod (Array.length h.data) in
      h.data.(i) <- remove_bucket h.data.(i)

    let find h key =
      match h.data.((H.hash key) mod (Array.length h.data)) with
        Empty -> raise Not_found
      | Cons(k1, d1, rest1) ->
          if H.equal key k1 then d1 else
          match rest1 with
            Empty -> raise Not_found
          | Cons(k2, d2, rest2) ->
              if H.equal key k2 then d2 else
              match rest2 with
                Empty -> raise Not_found
              | Cons(k3, d3, rest3) ->
                  if H.equal key k3 then d3 else begin
                    let rec find = function
                        Empty ->
                          raise Not_found
                      | Cons(k, d, rest) ->
                          if H.equal key k then d else find rest
                    in find rest3
                  end

    let find_all h key =
      let rec find_in_bucket = function
        Empty ->
          []
      | Cons(k, d, rest) ->
          if H.equal k key
          then d :: find_in_bucket rest
          else find_in_bucket rest in
      find_in_bucket h.data.((H.hash key) mod (Array.length h.data))

    let iter = iter
  end
