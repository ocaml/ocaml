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

(** Weak array operations *)

type !'a t

external create : int -> 'a t = "caml_weak_create"

(** number of additional values in a weak pointer
 *    - Link
 *    - Data *)
let additional_values = 2 (* CAML_EPHE_FIRST_KEY in weak.h *)

let create l =
  if not (0 <= l && l <= Obj.Ephemeron.max_ephe_length) then
    invalid_arg("Weak.create");
  create l


let length x = Obj.size(Obj.repr x) - additional_values

let raise_if_invalid_offset e o msg =
  if not (0 <= o && o < length e) then
    invalid_arg(msg)

external set' : 'a t -> int -> 'a -> unit = "caml_ephe_set_key"
external unset : 'a t -> int -> unit = "caml_ephe_unset_key"
let set e o x =
  raise_if_invalid_offset e o "Weak.set";
  match x with
  | None -> unset e o
  | Some x -> set' e o x

external get : 'a t -> int -> 'a option = "caml_weak_get"
let get e o =
  raise_if_invalid_offset e o "Weak.get";
  get e o

external get_copy : 'a t -> int -> 'a option = "caml_weak_get_copy"
let get_copy e o =
  raise_if_invalid_offset e o "Weak.get_copy";
  get_copy e o

external check : 'a t -> int -> bool = "caml_weak_check"
let check e o =
  raise_if_invalid_offset e o "Weak.check";
  check e o

external blit : 'a t -> int -> 'a t -> int -> int -> unit = "caml_weak_blit"

(* blit: src srcoff dst dstoff len *)
let blit e1 o1 e2 o2 l =
  if l < 0 || o1 < 0 || o1 > length e1 - l
     || o2 < 0 || o2 > length e2 - l
  then invalid_arg "Weak.blit"
  else if l <> 0 then blit e1 o1 e2 o2 l

let fill ar ofs len x =
  if ofs < 0 || len < 0 || ofs > length ar - len
  then raise (Invalid_argument "Weak.fill")
  else begin
    for i = ofs to (ofs + len - 1) do
      set ar i x
    done
  end


(** Weak hash tables *)

(* this small module lets us use qualified names in the implementation
   of weak hash tables below, which is less confusing. *)
module Weak = struct
  type nonrec 'a t = 'a t
  let length = length
  let create = create
  let get = get
  let set = set
  let check = check
  let fill = fill
end

module type S = sig
  type data
  type t
  val create : int -> t
  val clear : t -> unit
  val merge : t -> data -> data
  val add : t -> data -> unit
  val remove : t -> data -> unit
  val find : t -> data -> data
  val find_opt : t -> data -> data option
  val find_all : t -> data -> data list
  val mem : t -> data -> bool
  val iter : (data -> unit) -> t -> unit
  val fold : (data -> 'acc -> 'acc) -> t -> 'acc -> 'acc
  val count : t -> int
  val stats : t -> int * int * int * int * int * int
end

(** Setting [verbose = true] will print various instrumentation
    information on weak table lookups and resizes/compressions. This
    is useful understanding the behavior for a specific use-case where
    the performance are disappointing.

    [verbose] should not noticeably reduce performance.
*)
let verbose =
  (* Option.is_some (Sys.getenv_opt "WEAK_HASHSET_VERBOSE") *)
  false

(** Setting [debug = true] will cause expensive debug assertions to be
    checked, especially in [resize] and [compress] operations which run
    implicitly and are thus harder to debug directly.
    This is very useful during fuzz-testing.

    [debug] may noticeably reduce performance, by adding linear sanity
    checks that can degrade the complexity of certain operations.
*)
let debug =
  false

(** When [verbose], we track statistics separately for each
    instantiation of the functor. This is more coarse-grained than
    tracking statistics for each table produced by [create], but it
    reduces the tracking overhead. This is better than tracking
    statistics globally for all instantiations, as we observed that
    projects that instantiate the functor several times (typically,
    performing hashconsing on different types) can have fairly
    different usage workloads for each instantiation.

    [hashset_id] is a unique identifier of the instance being tracked.
*)
let hashset_id = ref 0

module Make (K : Hashtbl.HashedType) : (S with type data = K.t) = struct
  type data = K.t

  let hashset_id =
    incr hashset_id;
    !hashset_id

  (** [Hash.t] is a representation of hashes as stored within the
      [hashes] array below. We reserve a distinguished [void] value
      which corresponds to the absence of a key in this position.

      Note: François Pottier's implementation
      ( https://github.com/fpottier/hachis/ ) also contains
      a distinguished 'tomb' value for slots whose key has been
      removed (Hashtbl.remove). For weak hash tables we do not need
      tombstones: we clear the weak key, but leave the hash around
      until the next resizing or compression.  *)
  module Hash : sig
    type t = private int
    val void : t
    val of_int : int -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end = struct
    type t = int
    let void = max_int
    let equal = Int.equal
    let compare = Int.compare
    let of_int (x : int) =
      (* Open-adressing hashtables need hash values that are "random
         enough" so that void elements are spread almost-randomly, to
         avoid long sequences of consecutive non-void elements.

         Unfortunately some hash functions provided by users tend to not
         be random at all (for example, in Rocq, integers are hashed
         by identity) -- this is not a problem for the previous
         array-of-bucket implementation. So we postprocess the hashes
         here to make them more random. *)
      let x =
        (* This constant looks very random (it comes from
           ocaml/runtime/hash.c), so the result is random. *)
        x * 0xcc9e2d51 in

      (* Avoid [void]. Because we assume the hashes are pseudo-random,
         we can simply send hashes equal to [void] another arbitrary
         value, without significantly increasing conflicts. *)
      if x = void then void + 1 else x
  end

  type t = {
    mutable hashes : Hash.t array;
    mutable keys : data Weak.t;
    mutable occupation : int;
    (** [occupation] is the number of non-void hashes *)
    mutable mask : int;
    (** [mask] is [Array.length hashes - 1]
       (note: the length must be a power of two) *)
    travel : int ref;
    (** [travel] is not used for statistics, but to schedule periodic
        compressions of the table. *)
  }

  let[@inline] capacity t =
    t.mask + 1

  let create sz =
    (* We need to guarantee that there is always at least one [void]
       slot for search to terminate, so [sz] must be at least 1.
       We also guarantee that sizes are always a power of 2,
       to compute the modulo efficiently. *)
    let sz' = ref 1 in
    while !sz' < sz do sz' := 2 * !sz' done;
    let sz = !sz' in
    {
      hashes = Array.make sz Hash.void;
      keys = Weak.create sz;
      occupation = 0;
      mask = sz - 1;
      travel = ref 0;
    }

  let clear t =
    Weak.fill t.keys 0 (Weak.length t.keys) None;
    Array.fill t.hashes 0 (Array.length t.hashes) Hash.void;
    t.occupation <- 0

  let iter f t =
    let len = Array.length t.hashes in
    for i = 0 to len - 1 do
      match Weak.get t.keys i with
      | None -> ()
      | Some k -> f k
    done

  let fold f t acc =
    let len = Array.length t.hashes in
    let acc = ref acc in
    for i = 0 to len - 1 do
      match Weak.get t.keys i with
      | None -> ()
      | Some k -> acc := f k !acc
    done;
    !acc

  let count t =
    let len = Array.length t.hashes in
    let count = ref 0 in
    for i = 0 to len - 1 do
      (* Note: calling [Weak.check] is more efficient than calling
         [Weak.get] and ignoring the returned value, as [Weak.get]
         will darken the value and force it to remain alive for
         longer. *)
      if Weak.check t.keys i
      then incr count
    done;
    !count

  let bindings t =
    let len = Array.length t.hashes in
    let acc = ref [] in
    for i = 0 to len - 1 do
      let h = t.hashes.(i) in
      match Weak.get t.keys i with
      | None -> ()
      | Some k -> acc := (h, k) :: !acc
    done;
    List.sort (fun (h1, _) (h2, _) -> Hash.compare h1 h2) !acc

  let[@inline] debug_bindings t =
    if debug then bindings t else []

  let[@inline] debug_check_bindings ~old_debug_bindings ~new_debug_bindings =
    if debug then begin
      let eq (h1, k1) (h2, k2) = Hash.equal h1 h2 && K.equal k1 k2 in
      assert (List.equal eq old_debug_bindings new_debug_bindings)
    end

  (** Statistics: we track the number of lookups performed in the
      table, and the total probe travel -- the number of hashes that
      must be tried before fiding the desired slot.

      We expect a typical average travel between 1 and 5. Higher
      averages may be the sign of problematic hash conflicts, or
      a resizing scheduling policy that is not aggressive enough for
      the given workload.
  *)
  let locate_calls = ref 0
  let locate_travel = ref 0

  let () = if verbose then at_exit (fun () ->
    Printf.eprintf "Hashset(%.2d) locate: calls %d, average travel %g/call\n%!"
      hashset_id
      !locate_calls
      (float !locate_travel /. float !locate_calls)
  )

  (* The result of [locate] functions: either we found an equal
     element at a certain position, or we stopped on a void slot. *)
  type finding =
    | Found of int * data
    | Void of int

  let[@inline] pos ~mask h = (h : Hash.t :> int) land mask
  let[@inline] next_pos ~mask i = (i + 1) land mask

  (* /!\ This is the hot loop of most operations. *)
  let rec locate_loop ~mask ~travel keys k hashes h i =
    if verbose then incr locate_travel;
    incr travel;
    let h' = Array.unsafe_get hashes i in
    let i' = next_pos ~mask i in
    if h' <> h then
      if h' = Hash.void then Void i
      else locate_loop ~mask ~travel keys k hashes h i'
    else
      match Weak.get keys i with
      | Some k' when K.equal k k' -> Found (i, k')
      | _ ->
        (* When a value has been erased by the GC (case [None]), we must
           keep looking further for another value with the same hash. It
           would be incorrect to treat it as a [void] hash, for the same
           reason that François distinguishes [tomb] from [void]. *)
        locate_loop ~mask ~travel keys k hashes h i'

  (** Locate an element starting from a given position [i].
      This is used to implement [find_all]. *)
  let[@inline] locate_from t k h i =
    assert (t.occupation < capacity t);
    if verbose then incr locate_calls;
    locate_loop
      ~mask:t.mask ~travel:t.travel
      t.keys k t.hashes h i

  let[@inline] locate t k h =
    locate_from t k h (pos ~mask:t.mask h)

  (** Specialized locate functions that are only looking for the next
      [void] slot. This is used whenever we want to add an element
      that may already exist in the table -- in [add], [resize] and
      [compress].  *)
  let rec locate_void_loop ~mask ~travel hashes i =
    if verbose then incr locate_travel;
    incr travel;
    let h' = Array.unsafe_get hashes i in
    let i' = next_pos ~mask i in
    if h' = Hash.void then i
    else locate_void_loop ~mask ~travel hashes i'

  let[@inline] locate_void t h =
    assert (t.occupation < capacity t);
    if verbose then incr locate_calls;
    locate_void_loop
      ~mask:t.mask ~travel:t.travel
      t.hashes (pos ~mask:t.mask h)

  let next_sz n =
    (* We use [max_array_length / 2] as the maximum size, beacause
       weak arrays cannot go up to [max_array_length] elements
       (they have an extra [data] slot). This is the largest power of
       two that is valid for them. *)
    min (2*n) (Sys.max_array_length / 2)

  let resize_count = ref 0

  (** [resize t] copies elements into a larger array. In the process,
      it turns elements removed by the user or the GC into [void]
      slots. *)
  let resize t =
    if verbose then incr resize_count;
    let old_debug_bindings = debug_bindings t in
    let old_occupation = t.occupation in
    let old_capacity = capacity t in
    let old_hashes, old_keys = t.hashes, t.keys in
    let new_capacity = next_sz old_capacity in
    let new_mask = new_capacity - 1 in
    let new_hashes, new_keys =
      Array.make new_capacity Hash.void,
      Weak.create new_capacity
    in
    t.hashes <- new_hashes;
    t.keys <- new_keys;
    t.mask <- new_mask;
    t.occupation <- 0;
    for i = 0 to old_capacity - 1 do
      if Weak.check old_keys i then begin
        let h = Array.unsafe_get old_hashes i in
        let new_i = locate_void t h in
        t.occupation <- t.occupation + 1;
        begin
          (* Note: in theory it should be more efficient to use
             [Weak.blit old_keys i new_keys new_i 1], but the OCaml
             runtime has an implementation of [blit] whose cost is
             proportional to the total number of keys of the
             ephemerons, resulting in a huge slowdown here.

             This issue was fixed by
               https://github.com/ocaml/ocaml/pull/9259/
             in OCaml 4.x, but was not re-applied in OCaml 5.x for now
             (it is not included in 5.5).
          *)
          Weak.set new_keys new_i (Weak.get old_keys i);
        end;
        new_hashes.(new_i) <- h;
      end
    done;
    let new_debug_bindings = debug_bindings t in
    debug_check_bindings ~old_debug_bindings ~new_debug_bindings;
    let new_occupation = t.occupation in
    if verbose then
      Printf.eprintf "[%.2d:%.2d] Resize: size %d=>%d, occupation %d=>%d\n%!"
        hashset_id !resize_count
        old_capacity new_capacity
        old_occupation new_occupation;
    ()

  (** [compress t] compresses a table in-place, by turning into [void] slots
      the elements removed by the user or the GC. *)
  let compress t =
    if verbose then incr resize_count;
    let old_debug_bindings = debug_bindings t in
    let old_occupation = t.occupation in
    let first_void =
      assert (t.occupation < capacity t);
      Array.find_index (fun h -> h = Hash.void) t.hashes |> Option.get in
    let len = Array.length t.hashes in
    for i = first_void + 1 to first_void + len - 1 do
      let i = i mod len in
      let h = t.hashes.(i) in
      if h <> Hash.void then
        match Weak.check t.keys i with
        | false ->
          t.occupation <- t.occupation - 1;
          t.hashes.(i) <- Hash.void;
        | true ->
          (* We use [locate_void] to locate where this element should
             be placed, even if equal elements are already present in
             the table. This requires first temporarily turning the
             element's hash into [void], so that [locate_void] at
             least stops at the current position. *)
          t.hashes.(i) <- Hash.void;
          let new_i = locate_void t h in
          t.hashes.(new_i) <- h;
          if i <> new_i then begin
            (* see the comment on [Weak.blit] above. *)
            Weak.set t.keys new_i (Weak.get t.keys i);
            Weak.set t.keys i None;
          end
    done;
  let new_occupation = t.occupation in
  if verbose then
    Printf.eprintf "[%.2d:%.2d] Compression: occupation %d=>%d\n%!"
      hashset_id !resize_count
      old_occupation new_occupation;
  let new_debug_bindings = debug_bindings t in
  debug_check_bindings ~old_debug_bindings ~new_debug_bindings;
  ()

  let crowded t =
    (* resize at 82% occupation (105/128);
       from François Pottier's [hachis] library. *)
    128 * t.occupation > 105 * capacity t

  (** [maybe_resize t] resizes or compresses the table if doing so
      would be beneficial to reduce average travel length. It also
      guarantees that at least one void slot exists when it returns
      ([occupancy < capacity]), which is necessary for the termination
      of all functions that call [locate]. *)
  let maybe_resize t =
    if crowded t then begin
      (* Our estimation of occupation does not take into account weak
         keys that have been removed by the GC. When the occupation
         becomes high and we consider resizing, we first look at
         whether the real occupation is low enough that no resizing is
         necessary -- in this case we just compress the data in-place,
         without moving to larger backing arrays.

         Note: we also need to check [t.occupation < capacity t], as
         compression needs at least one [void] slot.
      *)
      let real_occupation = count t in
      if t.occupation < capacity t
      && real_occupation < capacity t / 2
      then compress t
      else resize t;
      t.travel := 0;
    end
    else if !(t.travel) > 128 * capacity t then begin
      (* In workloads where hits dominate misses, the table grows very
         slowly, so the crowded criterion rarely applies. It remains
         useful to compress it from time to time, to get a chance to
         remove collected values and thus speedup future lookups.

         To compress regularly, we measure the 'travel' caused by
         lookups, the total number of positions they have visited since
         the last resizing or compression. When they have visited many
         times the total size of the structure, we have amortized the
         cost of a compression.

         On [test_qs.ml] from the [ocaml-hashcons] repository (99.8%
         hit rate), this extra source of compression reduces average
         lookup travel from 5.4 to 1.3, and runtime is reduced from 1.7s
         to 1.3s. *)
      compress t;
      t.travel := 0;
    end;
    if debug then assert (t.occupation < capacity t)

  let calls = ref 0
  let hits = ref 0
  let misses = ref 0
  let () = if verbose then at_exit (fun () ->
    let ratio n = 100. *. float n /. float !calls in
    if !calls > 0 then
    Printf.eprintf "Hachcons(%.2d) calls %d: hits %d (%g%%), misses %d (%g%%).\n%!"
      hashset_id
      !calls
      !hits (ratio !hits)
      !misses (ratio !misses)
  )

  let find_opt t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    match locate t k h with
    | Void _i ->
      if verbose then incr misses;
      None
    | Found (_i, k') ->
      if verbose then incr hits;
      Some k'

  let find t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    match locate t k h with
    | Void _i ->
      if verbose then incr misses;
      raise Not_found
    | Found (_i, k') ->
      if verbose then incr hits;
      k'

  let mem t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    match locate t k h with
    | Void _i ->
      if verbose then incr misses;
      false
    | Found (_i, _k') ->
      if verbose then incr hits;
      true

  let rec find_all t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    (* We choose to count a non-empty list as a (single) hit,
       and an empty list as a miss. *)
    match locate t k h with
    | Void _i ->
      if verbose then incr misses;
      []
    | Found (i, k') ->
      if verbose then incr hits;
      find_rest t k h ~last:i [k']

  and find_rest t k h ~last acc =
    match locate_from t k h (next_pos ~mask:t.mask last) with
    | Void _i ->
      acc
    | Found (i', k') ->
      find_rest t k h ~last:i' (k' :: acc)

  let merge t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    match locate t k h with
    | Found (_i, k') ->
      if verbose then incr hits;
      k'
    | Void i ->
      if verbose then incr misses;
      Weak.set t.keys i (Some k);
      Array.unsafe_set t.hashes i h;
      t.occupation <- t.occupation + 1;
      k

  let add t k =
    (* We do not count hit and misses, as [locate_void] does not care
       if the same hash is encountered on the way to the next void
       slot. *)
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    let i = locate_void t h in
    Weak.set t.keys i (Some k);
    Array.unsafe_set t.hashes i h;
    t.occupation <- t.occupation + 1;
    ()

  let remove t k =
    if verbose then incr calls;
    maybe_resize t;
    let h = Hash.of_int (K.hash k) in
    match locate t k h with
    | Void _ ->
      if verbose then incr misses;
      ()
    | Found (i, _) ->
      if verbose then incr hits;
      (* Notice that here we can leave the hash unchanged, instead of
         having to use a dedicated [tombstone] value as in typical
         implementations. Erasing the key suffices. Keys removed from
         the table behave like keys removed by the GC. *)
      Weak.set t.keys i None

  (** The interface of the [stats] function in Weak is not structured,
      the return type is [int * int * int * int * int * int].

      The meaning of each field was chosen for the array-of-bucket
      implementations, and we had to adapt it with the linear-probing
      implementation. Instead of "buckets", we compute statistics on
      "filled intervals", maximal intervals of non-void values.

      1. "table length": this was the size of the array of bucket,
         we now return the number of distinct filled intervals.

      2. "number of entries": the number of live keys in the table.

      3. "sum of bucket lengths": the size of the backing array.

      4, 5, 6. "{smallest, median, biggest} bucket length":
         size of the {smallest, median, biggest} interval.
  *)
  let stats t =
    let num_bindings =
      (* number of live keys *)
      let i = ref 0 in iter (fun _ -> incr i) t; !i in
    let interval_lens =
      (* Compute an array all void positions, in order. *)
      let voids = ref [] in
      Array.iteri (fun i h -> if h == Hash.void then voids := i :: !voids) t.hashes;
      let voids = Array.of_list (List.rev !voids) in
      (* An interval is a set of consecutive slots between two void positions. *)     List.init (Array.length voids - 1) (fun i ->
        if i < Array.length voids - 1 then
          voids.(i + 1) - voids.(i) - 1
        else
          Array.length t.hashes - voids.(i) - 1
          + voids.(0)
      )
      |> List.filter ((<>) 0) (* filter out empty intervals *)
      |> Array.of_list
    in
    let interval_lens =
      (* avoid out-of-bound accesses below *)
      if interval_lens = [||] then [|0|]
      else interval_lens in
    Array.sort Int.compare interval_lens;
    let nb_intervals = Array.length interval_lens in
    let min_interval_len = interval_lens.(0) in
    let median_interval_len = interval_lens.(nb_intervals / 2) in
    let max_interval_len = interval_lens.(nb_intervals - 1) in
    (nb_intervals, num_bindings, Array.length t.hashes,
     min_interval_len, median_interval_len, max_interval_len)
end
