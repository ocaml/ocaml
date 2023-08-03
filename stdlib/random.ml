(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Damien Doligez, projet Para, INRIA Rocquencourt          *)
(*          Xavier Leroy, projet Cambium, College de France and Inria     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pseudo-random number generator *)

external random_seed: unit -> int array = "caml_sys_random_seed"

module State = struct

  open Bigarray

  type t = (int64, int64_elt, c_layout) Array1.t

  external next: t -> (int64[@unboxed])
      = "caml_lxm_next" "caml_lxm_next_unboxed" [@@noalloc]

  let create () : t =
    Array1.create Int64 C_layout 4

  let set s i1 i2 i3 i4 =
    Array1.unsafe_set s 0 (Int64.logor i1 1L); (* must be odd *)
    Array1.unsafe_set s 1 i2;
    Array1.unsafe_set s 2 (if i3 <> 0L then i3 else 1L); (* must not be 0 *)
    Array1.unsafe_set s 3 (if i4 <> 0L then i4 else 2L) (* must not be 0 *)

  let mk i1 i2 i3 i4 =
    let s = create () in
    set s i1 i2 i3 i4; s

  let serialization_prefix =
    "lxm1:"
    (* "lxm" denotes the algorithm currently in use, and '1' is
       a version number. We should update this prefix if we change
       the Random algorithm or the serialization format, so that users
       get a clean error instead of believing that they faithfully
       reproduce their previous state and in fact get a different
       stream.

       Note that there is no constraint to keep the same
       "<name><ver>:<data>" format or message size in future versions,
       we could change the format completely if we wanted as long
       as there is no confusion possible with the previous formats. *)

  let serialization_prefix_len =
    String.length serialization_prefix

  let to_binary_string s =
    let prefix = serialization_prefix in
    let preflen = serialization_prefix_len in
    let buf = Bytes.create (preflen + 4 * 8) in
    Bytes.blit_string prefix 0 buf 0 preflen;
    for i = 0 to 3 do
      Bytes.set_int64_le buf (preflen + i * 8) (Array1.get s i)
    done;
    Bytes.unsafe_to_string buf

  let of_binary_string buf =
    let prefix = serialization_prefix in
    let preflen = serialization_prefix_len in
    if String.length buf <> preflen + 4 * 8
       || not (String.starts_with ~prefix buf)
    then
      failwith
        ("Random.State.of_binary_string: expected a format \
          compatible with OCaml " ^ Sys.ocaml_version);
    let i1 = String.get_int64_le buf (preflen + 0 * 8) in
    let i2 = String.get_int64_le buf (preflen + 1 * 8) in
    let i3 = String.get_int64_le buf (preflen + 2 * 8) in
    let i4 = String.get_int64_le buf (preflen + 3 * 8) in
    mk i1 i2 i3 i4

  let assign (dst: t) (src: t) =
    Array1.blit src dst

  let copy s =
    let s' = create() in assign s' s; s'

  (* The seed is an array of integers.  It can be just one integer,
     but it can also be 12 or more bytes.  To hide the difference,
     we serialize the array as a sequence of bytes, then hash the
     sequence with MD5 (Digest.bytes).  MD5 gives only 128 bits while
     we need 256 bits, so we hash twice with different suffixes. *)
  let reinit s seed =
    let n = Array.length seed in
    let b = Bytes.create (n * 8 + 1) in
    for i = 0 to n-1 do
      Bytes.set_int64_le b (i * 8) (Int64.of_int seed.(i))
    done;
    Bytes.set b (n * 8) '\x01';
    let d1 = Digest.bytes b in
    Bytes.set b (n * 8) '\x02';
    let d2 = Digest.bytes b in
    set s (String.get_int64_le d1 0)
          (String.get_int64_le d1 8)
          (String.get_int64_le d2 0)
          (String.get_int64_le d2 8)

  let make seed =
    let s = create() in reinit s seed; s

  let make_self_init () =
    make (random_seed ())

  (* NOTE:
   *   0x3FFF_FFFF = 2^30 - 1, which is max_int for 31-bit integers
   *   -0x4000_0000 = -2^30, which is min_int for 31-bit integers
   *)

  (* Return 30 random bits as an integer 0 <= x < 2^30 *)
  let bits s =
    Int64.to_int (next s) land 0x3FFF_FFFF

  (* Return an integer between 0 (included) and [bound] (excluded).
     The bound must fit in 31 bits.
     This function is designed so that it yields the same output
     on 32-bit and 64-bit platforms. *)
  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    (* Explanation of this test: see comment in [int63aux]. *)
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

  (* Return an integer between 0 (included) and [bound] (excluded).
     [bound] may be any positive [int]. *)
  let rec int63aux s n =
    (* We start by drawing any representable non-negative integer. *)
    let r = Int64.to_int (next s) land max_int in
    let v = r mod n in
    (* For uniform distribution of the result between 0 included and [n]
     * excluded, the random number [r] must have been drawn uniformly in
     * an interval whose length is a multiple of [n]. So the test below
     * checks whether [r < k*n], where [k*n] is the greatest multiple of
     * [n] such that [k*n - 1] is in the interval of representable
     * integers. If failing that, any interval whose length is
     * a multiple of [n] and which contains [r] has values that are not
     * representable, hence, drawing was not uniform; in that case, we
     * re-draw. *)
    if r - v > max_int - n + 1 then int63aux s n else v

  let full_int s bound =
    if bound <= 0 then
      invalid_arg "Random.full_int"
    (* When the bound fits in 31 bits, we use [intaux]
     * so as to yield the same output on 32-bit and 64-bit platforms. *)
    else if bound > 0x3FFFFFFF then
      int63aux s bound
    else
      intaux s bound

  (* Cast a full-width integer to a signed 31-bit integer, by computing its
   * signed remainder modulo 2^31. *)
  let int31_of_int x =
    let d = Sys.int_size - 31 in
    (* Replicate the 31th bit (sign bit) to higher positions: *)
    (x lsl d) asr d

  (* Return an integer between [min] (included) and [max] (included).
     We must have [min <= max], and [min] and [max] must fit in 31 bits.
     This function is designed so that it yields the same output on
     32-bit and 64-bit platforms. *)
  let rec int31_in_range_aux s ~min ~max =
    let r = int31_of_int (Int64.to_int (next s)) in
    if r < min || r > max then int31_in_range_aux s ~min ~max else r

  let int31_in_range s ~min ~max =
    let span = max - min + 1 in
    (* Explanation of this test: see comment in [int_in_range];
     * the comparison to 0 is for 32-bit platforms;
     * the comparison to 2^30-1 is for 64-bit platforms. *)
    if span <= 0 || span > 0x3FFF_FFFF then
      int31_in_range_aux s ~min ~max
    else
      min + intaux s span

  (* Return an integer between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let rec int_in_range_aux s ~min ~max =
    let r = Int64.to_int (next s) in
    if r < min || r > max then int_in_range_aux s ~min ~max else r

  let int_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int_in_range"
    (* When both bounds fit in 31 bits, we use [int31_in_range],
     * so as to yield the same output on 32-bit and 64-bit platforms. *)
    else if min < -0x4000_0000 || max > 0x3FFF_FFFF then
      let span = max - min + 1 in
      (* We use [int_in_range_aux] only if [max - min + 1] overflows;
       * then, the interval between [min] and [max] covers at least half
       * of the representable integers, so that it converges quickly
       * (the probability of re-drawing is at most 1/2).
       *
       * When [max - min + 1] does not overflow, we use [int63aux], for
       * 2 reasons:
       *   - [int_in_range] then has a high probability of re-drawing,
       *     so would be very slow;
       *   - we thus guarantee that, when [min = 0], [int_in_range]
       *     yields the same output as [full_int (max+1)].
       *)
      if span <= 0 then
        int_in_range_aux s ~min ~max
      else
        min + int63aux s span
    else
      int31_in_range s ~min ~max

  (* Return 32 random bits as an [int32] *)
  let bits32 s =
    Int64.to_int32 (next s)

  (* Return an [int32] between 0 (included) and [bound] (excluded). *)
  let rec int32aux s n =
    let r = Int32.shift_right_logical (bits32 s) 1 in
    let v = Int32.rem r n in
    (* Explanation of this test: see comment in [int63aux]. *)
    if Int32.(sub r v > add (sub max_int n) 1l)
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound

  (* Return an [int32] between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let rec int32_in_range_aux s ~min ~max =
    let r = Int64.to_int32 (next s) in
    if r < min || r > max then int32_in_range_aux s ~min ~max else r

  let int32_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int32_in_range"
    else
      let span = Int32.succ (Int32.sub max min) in
      (* Explanation of this test: see comment in [int_in_range]. *)
      if span <= Int32.zero then
        int32_in_range_aux s ~min ~max
      else
        Int32.add min (int32aux s span)

  (* Return 64 random bits as an [int64] *)
  let bits64 s =
    next s

  (* Return an [int64] between 0 (included) and [bound] (excluded). *)
  let rec int64aux s n =
    let r = Int64.shift_right_logical (bits64 s) 1 in
    let v = Int64.rem r n in
    (* Explanation of this test: see comment in [int63aux]. *)
    if Int64.(sub r v > add (sub max_int n) 1L)
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound

  (* Return an [int64] between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let rec int64_in_range_aux s ~min ~max =
    let r = next s in
    if r < min || r > max then int64_in_range_aux s ~min ~max else r

  let int64_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int64_in_range"
    else
      let span = Int64.succ (Int64.sub max min) in
      (* Explanation of this test: see comment in [int_in_range]. *)
      if span <= Int64.zero then
        int64_in_range_aux s ~min ~max
      else
        Int64.add min (int64aux s span)

  (* Return 32 or 64 random bits as a [nativeint] *)
  let nativebits =
    if Nativeint.size = 32
    then fun s -> Nativeint.of_int32 (bits32 s)
    else fun s -> Int64.to_nativeint (bits64 s)

  (* Return a [nativeint] between 0 (included) and [bound] (excluded). *)
  let nativeint =
    if Nativeint.size = 32
    then fun s bound -> Nativeint.of_int32 (int32 s (Nativeint.to_int32 bound))
    else fun s bound -> Int64.to_nativeint (int64 s (Int64.of_nativeint bound))

  (* Return a [nativeint] between [min] (included) and [max] (included). *)
  let nativeint_in_range =
    if Nativeint.size = 32
    then fun s ~min ~max ->
      Nativeint.of_int32 (int32_in_range s
        ~min:(Nativeint.to_int32 min) ~max:(Nativeint.to_int32 max))
    else fun s ~min ~max ->
      Int64.to_nativeint (int64_in_range s
        ~min:(Int64.of_nativeint min) ~max:(Int64.of_nativeint max))

  (* Return a float 0 < x < 1 uniformly distributed among the
     multiples of 2^-53 *)
  let rec rawfloat s =
    let b = next s in
    let n = Int64.shift_right_logical b 11 in
    if n <> 0L then Int64.to_float n *. 0x1.p-53 else rawfloat s

  (* Return a float between 0 and [bound] *)
  let float s bound = rawfloat s *. bound

  (* Return a random Boolean *)
  let bool s = next s < 0L

  (* Split a new PRNG off the given PRNG *)
  let split s =
    let i1 = bits64 s in let i2 = bits64 s in
    let i3 = bits64 s in let i4 = bits64 s in
    mk i1 i2 i3 i4
end

let mk_default () =
  (* This is the state obtained with [State.make [| 314159265 |]]. *)
  State.mk (-6196874289567705097L)
           586573249833713189L
           (-8591268803865043407L)
           6388613595849772044L

let random_key =
  Domain.DLS.new_key ~split_from_parent:State.split mk_default

let bits () = State.bits (Domain.DLS.get random_key)
let int bound = State.int (Domain.DLS.get random_key) bound
let full_int bound = State.full_int (Domain.DLS.get random_key) bound
let int_in_range ~min ~max =
  State.int_in_range (Domain.DLS.get random_key) ~min ~max
let int32 bound = State.int32 (Domain.DLS.get random_key) bound
let int32_in_range ~min ~max =
  State.int32_in_range (Domain.DLS.get random_key) ~min ~max
let nativeint bound = State.nativeint (Domain.DLS.get random_key) bound
let nativeint_in_range ~min ~max =
  State.nativeint_in_range (Domain.DLS.get random_key) ~min ~max
let int64 bound = State.int64 (Domain.DLS.get random_key) bound
let int64_in_range ~min ~max =
  State.int64_in_range (Domain.DLS.get random_key) ~min ~max
let float scale = State.float (Domain.DLS.get random_key) scale
let bool () = State.bool (Domain.DLS.get random_key)
let bits32 () = State.bits32 (Domain.DLS.get random_key)
let bits64 () = State.bits64 (Domain.DLS.get random_key)
let nativebits () = State.nativebits (Domain.DLS.get random_key)

let full_init seed = State.reinit (Domain.DLS.get random_key) seed
let init seed = full_init [| seed |]
let self_init () = full_init (random_seed())

(* Splitting *)

let split () = State.split (Domain.DLS.get random_key)

(* Manipulating the current state. *)

let get_state () = State.copy (Domain.DLS.get random_key)
let set_state s = State.assign (Domain.DLS.get random_key) s
