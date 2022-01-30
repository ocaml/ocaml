(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Damien Doligez, projet Para, INRIA Rocquencourt            *)
(*                                                                        *)
(*   Copyright 1997 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type SeededS = sig

  type key
  type !'a t
  val create : ?random (*thwart tools/sync_stdlib_docs*) : bool -> int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
  val clean: 'a t -> unit
  val stats_alive: 'a t -> Hashtbl.statistics
    (** same as {!stats} but only count the alive bindings *)
end

module type S = sig

  type key
  type !'a t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
  val add_seq : 'a t -> (key * 'a) Seq.t -> unit
  val replace_seq : 'a t -> (key * 'a) Seq.t -> unit
  val of_seq : (key * 'a) Seq.t -> 'a t
  val clean: 'a t -> unit
  val stats_alive: 'a t -> Hashtbl.statistics
    (** same as {!stats} but only count the alive bindings *)
end

module GenHashTable = struct

  type equal =
  | ETrue | EFalse
  | EDead (** the garbage collector reclaimed the data *)

  module MakeSeeded(H: sig
    type t
    type 'a container
    val create: t -> 'a -> 'a container
    val hash: int -> t -> int
    val equal: 'a container -> t -> equal
    val get_data: 'a container -> 'a option
    val set_key_data: 'a container -> t -> 'a -> unit
    val check_key: 'a container -> bool
  end) : SeededS with type key = H.t
  = struct

    type 'a t =
      { mutable size: int;                  (* number of entries *)
        mutable data: 'a bucketlist array;  (* the buckets *)
        seed: int;                          (* for randomization *)
        initial_size: int;                  (* initial array size *)
      }

    and 'a bucketlist =
    | Empty
    | Cons of int (* hash of the key *) * 'a H.container * 'a bucketlist

    (** the hash of the key is kept in order to test the equality of the hash
      before the key. Same reason as for Weak.Make *)

    type key = H.t

    let rec power_2_above x n =
      if x >= n then x
      else if x * 2 > Sys.max_array_length then x
      else power_2_above (x * 2) n

    let prng = lazy (Random.State.make_self_init())

    let create ?(random = (Hashtbl.is_randomized ())) initial_size =
      let s = power_2_above 16 initial_size in
      let seed = if random then Random.State.bits (Lazy.force prng) else 0 in
      { initial_size = s; size = 0; seed = seed; data = Array.make s Empty }

    let clear h =
      h.size <- 0;
      let len = Array.length h.data in
      for i = 0 to len - 1 do
        h.data.(i) <- Empty
      done

    let reset h =
      let len = Array.length h.data in
      if len = h.initial_size then
        clear h
      else begin
        h.size <- 0;
        h.data <- Array.make h.initial_size Empty
      end

    let copy h = { h with data = Array.copy h.data }

    let key_index h hkey =
      hkey land (Array.length h.data - 1)

    let clean h =
      let rec do_bucket = function
        | Empty ->
            Empty
        | Cons(_, c, rest) when not (H.check_key c) ->
            h.size <- h.size - 1;
            do_bucket rest
        | Cons(hkey, c, rest) ->
            Cons(hkey, c, do_bucket rest)
      in
      let d = h.data in
      for i = 0 to Array.length d - 1 do
        d.(i) <- do_bucket d.(i)
      done

    (** resize is the only function to do the actual cleaning of dead keys
        (remove does it just because it could).

        The goal is to:

        - not resize infinitely when the actual number of alive keys is
        bounded but keys are continuously added. That would happen if
        this function always resize.
        - not call this function after each addition, that would happen if this
        function don't resize even when only one key is dead.

        So the algorithm:
        - clean the keys before resizing
        - if the number of remaining keys is less than half the size of the
        array, don't resize.
        - if it is more, resize.

        The second problem remains if the table reaches {!Sys.max_array_length}.

    *)
    let resize h =
      let odata = h.data in
      let osize = Array.length odata in
      let nsize = osize * 2 in
      clean h;
      if nsize < Sys.max_array_length && h.size >= osize lsr 1 then begin
        let ndata = Array.make nsize Empty in
        h.data <- ndata;       (* so that key_index sees the new bucket count *)
        let rec insert_bucket = function
            Empty -> ()
          | Cons(hkey, data, rest) ->
              insert_bucket rest; (* preserve original order of elements *)
              let nidx = key_index h hkey in
              ndata.(nidx) <- Cons(hkey, data, ndata.(nidx)) in
        for i = 0 to osize - 1 do
          insert_bucket odata.(i)
        done
      end

    let add h key info =
      let hkey = H.hash h.seed key in
      let i = key_index h hkey in
      let container = H.create key info in
      let bucket = Cons(hkey, container, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- h.size + 1;
      if h.size > Array.length h.data lsl 1 then resize h

    let remove h key =
      let hkey = H.hash h.seed key in
      let rec remove_bucket = function
        | Empty -> Empty
        | Cons(hk, c, next) when hkey = hk ->
            begin match H.equal c key with
            | ETrue -> h.size <- h.size - 1; next
            | EFalse -> Cons(hk, c, remove_bucket next)
            | EDead ->
                (* The dead key is automatically removed. It is acceptable
                    for this function since it already removes a binding *)
                h.size <- h.size - 1;
                remove_bucket next
            end
        | Cons(hk,c,next) -> Cons(hk, c, remove_bucket next) in
      let i = key_index h hkey in
      h.data.(i) <- remove_bucket h.data.(i)

    (** {!find} don't remove dead keys because it would be surprising for
        the user that a read-only function mutates the state (eg. concurrent
        access). Same for {!mem}.
    *)
    let rec find_rec key hkey = function
      | Empty ->
          raise Not_found
      | Cons(hk, c, rest) when hkey = hk  ->
          begin match H.equal c key with
          | ETrue ->
              begin match H.get_data c with
              | None ->
                  (* This case is not impossible because the gc can run between
                      H.equal and H.get_data *)
                  find_rec key hkey rest
              | Some d -> d
              end
          | EFalse -> find_rec key hkey rest
          | EDead ->
              find_rec key hkey rest
          end
      | Cons(_, _, rest) ->
          find_rec key hkey rest

    let find h key =
      let hkey = H.hash h.seed key in
      (* TODO inline 3 iterations *)
      find_rec key hkey (h.data.(key_index h hkey))

    let rec find_rec_opt key hkey = function
      | Empty ->
          None
      | Cons(hk, c, rest) when hkey = hk  ->
          begin match H.equal c key with
          | ETrue ->
              begin match H.get_data c with
              | None ->
                  (* This case is not impossible because the gc can run between
                      H.equal and H.get_data *)
                  find_rec_opt key hkey rest
              | Some _ as d -> d
              end
          | EFalse -> find_rec_opt key hkey rest
          | EDead ->
              find_rec_opt key hkey rest
          end
      | Cons(_, _, rest) ->
          find_rec_opt key hkey rest

    let find_opt h key =
      let hkey = H.hash h.seed key in
      (* TODO inline 3 iterations *)
      find_rec_opt key hkey (h.data.(key_index h hkey))

    let find_all h key =
      let hkey = H.hash h.seed key in
      let rec find_in_bucket = function
      | Empty -> []
      | Cons(hk, c, rest) when hkey = hk  ->
          begin match H.equal c key with
          | ETrue -> begin match H.get_data c with
              | None ->
                  find_in_bucket rest
              | Some d -> d::find_in_bucket rest
            end
          | EFalse -> find_in_bucket rest
          | EDead ->
              find_in_bucket rest
          end
      | Cons(_, _, rest) ->
          find_in_bucket rest in
      find_in_bucket h.data.(key_index h hkey)


    let replace h key info =
      let hkey = H.hash h.seed key in
      let rec replace_bucket = function
        | Empty -> raise Not_found
        | Cons(hk, c, next) when hkey = hk ->
            begin match H.equal c key with
            | ETrue -> H.set_key_data c key info
            | EFalse | EDead -> replace_bucket next
            end
        | Cons(_,_,next) -> replace_bucket next
      in
      let i = key_index h hkey in
      let l = h.data.(i) in
      try
        replace_bucket l
      with Not_found ->
        let container = H.create key info in
        h.data.(i) <- Cons(hkey, container, l);
        h.size <- h.size + 1;
        if h.size > Array.length h.data lsl 1 then resize h

    let mem h key =
      let hkey = H.hash h.seed key in
      let rec mem_in_bucket = function
      | Empty ->
          false
      | Cons(hk, c, rest) when hk = hkey ->
          begin match H.equal c key with
          | ETrue -> true
          | EFalse | EDead -> mem_in_bucket rest
          end
      | Cons(_hk, _c, rest) -> mem_in_bucket rest in
      mem_in_bucket h.data.(key_index h hkey)

    let length h = h.size

    let rec bucket_length accu = function
      | Empty -> accu
      | Cons(_, _, rest) -> bucket_length (accu + 1) rest

    let stats h =
      let mbl =
        Array.fold_left (fun m b -> Int.max m (bucket_length 0 b)) 0 h.data in
      let histo = Array.make (mbl + 1) 0 in
      Array.iter
        (fun b ->
           let l = bucket_length 0 b in
           histo.(l) <- histo.(l) + 1)
        h.data;
      { Hashtbl.num_bindings = h.size;
        num_buckets = Array.length h.data;
        max_bucket_length = mbl;
        bucket_histogram = histo }

    let rec bucket_length_alive accu = function
      | Empty -> accu
      | Cons(_, c, rest) when H.check_key c ->
          bucket_length_alive (accu + 1) rest
      | Cons(_, _, rest) -> bucket_length_alive accu rest

    let stats_alive h =
      let size = ref 0 in
      let mbl =
        Array.fold_left
          (fun m b -> Int.max m (bucket_length_alive 0 b)) 0 h.data
      in
      let histo = Array.make (mbl + 1) 0 in
      Array.iter
        (fun b ->
           let l = bucket_length_alive 0 b in
           size := !size + l;
           histo.(l) <- histo.(l) + 1)
        h.data;
      { Hashtbl.num_bindings = !size;
        num_buckets = Array.length h.data;
        max_bucket_length = mbl;
        bucket_histogram = histo }

    let add_seq tbl i =
      Seq.iter (fun (k,v) -> add tbl k v) i

    let replace_seq tbl i =
      Seq.iter (fun (k,v) -> replace tbl k v) i

    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl

  end
end

module ObjEph = Obj.Ephemeron

let _obj_opt : Obj.t option -> 'a option = fun x ->
  match x with
  | None -> x
  | Some v -> Some (Obj.obj v)

(** The previous function is typed so this one is also correct *)
let obj_opt : Obj.t option -> 'a option = fun x -> Obj.magic x


module K1 = struct
  type ('k,'d) t = ObjEph.t

  let create () : ('k,'d) t = ObjEph.create 1

  let get_key (t:('k,'d) t) : 'k option = obj_opt (ObjEph.get_key t 0)
  let set_key (t:('k,'d) t) (k:'k) : unit = ObjEph.set_key t 0 (Obj.repr k)
  let check_key (t:('k,'d) t) : bool = ObjEph.check_key t 0

  let get_data (t:('k,'d) t) : 'd option = obj_opt (ObjEph.get_data t)
  let set_data (t:('k,'d) t) (d:'d) : unit = ObjEph.set_data t (Obj.repr d)
  let unset_data (t:('k,'d) t) : unit = ObjEph.unset_data t

  let make key data =
    let eph = create () in
    set_data eph data;
    set_key eph key;
    eph

  let query eph key =
    match get_key eph with
    | None -> None
    | Some k when k == key -> get_data eph
    | Some _ -> None

  module MakeSeeded (H:Hashtbl.SeededHashedType) =
    GenHashTable.MakeSeeded(struct
      type 'a container = (H.t,'a) t
      type t = H.t
      let create k d =
        let c = create () in
        set_data c d;
        set_key c k;
        c
      let hash = H.hash
      let equal c k =
        (* {!get_key_copy} is not used because the equality of the user can be
            the physical equality *)
        match get_key c with
        | None -> GenHashTable.EDead
        | Some k' ->
            if H.equal k k' then GenHashTable.ETrue else GenHashTable.EFalse
      let get_data = get_data
      let set_key_data c k d =
        unset_data c;
        set_key c k;
        set_data c d
      let check_key = check_key
    end)

  module Make(H: Hashtbl.HashedType): (S with type key = H.t) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end

  module Bucket = struct

    type nonrec ('k, 'd) t = ('k, 'd) t list ref
    let k1_make = make
    let make () = ref []
    let add b k d = b := k1_make k d :: !b

    let test_key k e =
      match get_key e with
      | Some x when x == k -> true
      | _ -> false

    let remove b k =
      let rec loop l acc =
        match l with
        | [] -> ()
        | h :: t when test_key k h -> b := List.rev_append acc t
        | h :: t -> loop t (h :: acc)
      in
      loop !b []

    let find b k =
      match List.find_opt (test_key k) !b with
      | Some e -> get_data e
      | None -> None

    let length b = List.length !b
    let clear b = b := []

  end

end

module K2 = struct
  type ('k1, 'k2, 'd) t = ObjEph.t

  let create () : ('k1,'k2,'d) t = ObjEph.create 2

  let get_key1 (t:('k1,'k2,'d) t) : 'k1 option = obj_opt (ObjEph.get_key t 0)
  let set_key1 (t:('k1,'k2,'d) t) (k:'k1) : unit =
    ObjEph.set_key t 0 (Obj.repr k)
  let check_key1 (t:('k1,'k2,'d) t) : bool = ObjEph.check_key t 0

  let get_key2 (t:('k1,'k2,'d) t) : 'k2 option = obj_opt (ObjEph.get_key t 1)
  let set_key2 (t:('k1,'k2,'d) t) (k:'k2) : unit =
    ObjEph.set_key t 1 (Obj.repr k)
  let check_key2 (t:('k1,'k2,'d) t) : bool = ObjEph.check_key t 1

  let get_data (t:('k1,'k2,'d) t) : 'd option = obj_opt (ObjEph.get_data t)
  let set_data (t:('k1,'k2,'d) t) (d:'d) : unit =
    ObjEph.set_data t (Obj.repr d)
  let unset_data (t:('k1,'k2,'d) t) : unit = ObjEph.unset_data t

  let make key1 key2 data =
    let eph = create () in
    set_data eph data;
    set_key1 eph key1;
    set_key2 eph key2;
    ignore (Sys.opaque_identity key1);
    eph

  let query eph key1 key2 =
    match get_key1 eph with
    | None -> None
    | Some k when k == key1 ->
        begin match get_key2 eph with
        | None -> None
        | Some k when k == key2 -> get_data eph
        | Some _ -> None
        end
    | Some _ -> None

  module MakeSeeded
      (H1:Hashtbl.SeededHashedType)
      (H2:Hashtbl.SeededHashedType) =
    GenHashTable.MakeSeeded(struct
      type 'a container = (H1.t,H2.t,'a) t
      type t = H1.t * H2.t
      let create (k1,k2) d =
        let c = create () in
        set_data c d;
        set_key1 c k1; set_key2 c k2;
        c
      let hash seed (k1,k2) =
        H1.hash seed k1 + H2.hash seed k2 * 65599
      let equal c (k1,k2) =
        match get_key1 c, get_key2 c with
        | None, _ | _ , None -> GenHashTable.EDead
        | Some k1', Some k2' ->
            if H1.equal k1 k1' && H2.equal k2 k2'
            then GenHashTable.ETrue else GenHashTable.EFalse
      let get_data = get_data
      let set_key_data c (k1,k2) d =
        unset_data c;
        set_key1 c k1; set_key2 c k2;
        set_data c d
      let check_key c = check_key1 c && check_key2 c
    end)

  module Make(H1: Hashtbl.HashedType)(H2: Hashtbl.HashedType):
    (S with type key = H1.t * H2.t) =
  struct
    include MakeSeeded
        (struct
          type t = H1.t
          let equal = H1.equal
          let hash (_seed: int) x = H1.hash x
        end)
        (struct
          type t = H2.t
          let equal = H2.equal
          let hash (_seed: int) x = H2.hash x
        end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end

  module Bucket = struct

    type nonrec ('k1, 'k2, 'd) t = ('k1, 'k2, 'd) t list ref
    let k2_make = make
    let make () = ref []
    let add b k1 k2 d = b := k2_make k1 k2 d :: !b

    let test_keys k1 k2 e =
      match get_key1 e, get_key2 e with
      | Some x1, Some x2 when x1 == k1 && x2 == k2 -> true
      | _ -> false

    let remove b k1 k2 =
      let rec loop l acc =
        match l with
        | [] -> ()
        | h :: t when test_keys k1 k2 h -> b := List.rev_append acc t
        | h :: t -> loop t (h :: acc)
      in
      loop !b []

    let find b k1 k2 =
      match List.find_opt (test_keys k1 k2) !b with
      | Some e -> get_data e
      | None -> None

    let length b = List.length !b
    let clear b = b := []

  end

end

module Kn = struct
  type ('k,'d) t = ObjEph.t

  let create n : ('k,'d) t = ObjEph.create n
  let length (k:('k,'d) t) : int = ObjEph.length k

  let get_key (t:('k,'d) t) (n:int) : 'k option = obj_opt (ObjEph.get_key t n)
  let set_key (t:('k,'d) t) (n:int) (k:'k) : unit =
    ObjEph.set_key t n (Obj.repr k)
  let check_key (t:('k,'d) t) (n:int) : bool = ObjEph.check_key t n

  let get_data (t:('k,'d) t) : 'd option = obj_opt (ObjEph.get_data t)
  let set_data (t:('k,'d) t) (d:'d) : unit = ObjEph.set_data t (Obj.repr d)
  let unset_data (t:('k,'d) t) : unit = ObjEph.unset_data t

  let make keys data =
    let l = Array.length keys in
    let eph = create l in
    set_data eph data;
    for i = 0 to l - 1 do set_key eph i keys.(i) done;
    eph

  let query eph keys =
    let l = length eph in
    try
      if l <> Array.length keys then raise Exit;
      for i = 0 to l - 1 do
        match get_key eph i with
        | None -> raise Exit
        | Some k when k == keys.(i) -> ()
        | Some _ -> raise Exit
      done;
      get_data eph
    with Exit -> None

  module MakeSeeded (H:Hashtbl.SeededHashedType) =
    GenHashTable.MakeSeeded(struct
      type 'a container = (H.t,'a) t
      type t = H.t array
      let create k d =
        let c = create (Array.length k) in
        set_data c d;
        for i=0 to Array.length k -1 do
          set_key c i k.(i);
        done;
        c
      let hash seed k =
        let h = ref 0 in
        for i=0 to Array.length k -1 do
          h := H.hash seed k.(i) * 65599 + !h;
        done;
        !h
      let equal c k =
        let len  = Array.length k in
        let len' = length c in
        if len != len' then GenHashTable.EFalse
        else
          let rec equal_array k c i =
            if i < 0 then GenHashTable.ETrue
            else
              match get_key c i with
              | None -> GenHashTable.EDead
              | Some ki ->
                  if H.equal k.(i) ki
                  then equal_array k c (i-1)
                  else GenHashTable.EFalse
          in
          equal_array k c (len-1)
      let get_data = get_data
      let set_key_data c k d =
        unset_data c;
        for i=0 to Array.length k -1 do
          set_key c i k.(i);
        done;
        set_data c d
      let check_key c =
        let rec check c i =
          i < 0 || (check_key c i && check c (i-1)) in
        check c (length c - 1)
    end)

  module Make(H: Hashtbl.HashedType): (S with type key = H.t array) =
  struct
    include MakeSeeded(struct
        type t = H.t
        let equal = H.equal
        let hash (_seed: int) x = H.hash x
      end)
    let create sz = create ~random:false sz
    let of_seq i =
      let tbl = create 16 in
      replace_seq tbl i;
      tbl
  end

  module Bucket = struct

    type nonrec ('k, 'd) t = ('k, 'd) t list ref
    let kn_make = make
    let make () = ref []
    let add b k d = b := kn_make k d :: !b

    let test_keys k e =
      try
        if length e <> Array.length k then raise Exit;
        for i = 0 to Array.length k - 1 do
          match get_key e i with
          | Some x when x == k.(i) -> ()
          | _ -> raise Exit
        done;
        true
      with Exit -> false

    let remove b k =
      let rec loop l acc =
        match l with
        | [] -> ()
        | h :: t when test_keys k h -> b := List.rev_append acc t
        | h :: t -> loop t (h :: acc)
      in
      loop !b []

    let find b k =
      match List.find_opt (test_keys k) !b with
      | Some e -> get_data e
      | None -> None

    let length b = List.length !b
    let clear b = b := []

  end

end
