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

  let min_int31 = -0x4000_0000
      (* = -2{^30}, which is [min_int] for 31-bit integers *)
  let max_int31 = 0x3FFF_FFFF
      (* =  2{^30}-1, which is [max_int] for 31-bit integers *)
  (* avoid integer literals for these, 32-bit OCaml would reject them: *)
  let min_int32 = -(1 lsl 31)
      (* = -0x8000_0000 on platforms where [Sys.int_size >= 32] *)
  let max_int32 = (1 lsl 31) - 1
      (* =  0x7FFF_FFFF on platforms where [Sys.int_size >= 32] *)

  (* Return 30 random bits as an integer 0 <= x < 2^30 *)
  let bits s =
    Int64.to_int (next s) land max_int31

  (* Return an integer between 0 (included) and [n] (excluded).
     [bound] may be any positive [int].  [mask] must be of the form [2{^i}-1]
     and greater or equal to [n].  Larger values of [mask] make the function
     run faster (fewer samples are rejected).  Smaller values of [mask]
     are usable on a wider range of OCaml implementations.  *)
  let rec int_aux s n mask =
    (* We start by drawing a non-negative integer in the [ [0, mask] ] range *)
    let r = Int64.to_int (next s) land mask in
    let v = r mod n in
    (* For uniform distribution of the result between 0 included and [n]
     * excluded, the random number [r] must have been drawn uniformly in
     * an interval whose length is a multiple of [n]. To achieve this,
     * we use rejection sampling on the greatest interval [ [0, k*n-1] ]
     * that fits in [ [0, mask] ].  That is, we reject the
     * sample if it falls outside of this interval, and draw again.
     * This is what the test below does, while carefuly avoiding
     * overflows and sparing a division [mask / n]. *)
    if r - v > mask - n + 1 then int_aux s n mask else v

  (* Return an integer between 0 (included) and [bound] (excluded).
     The bound must fit in 31-bit signed integers.
     This function yields the same output regardless of the integer size. *)
  let int s bound =
    if bound > max_int31 || bound <= 0
    then invalid_arg "Random.int"
    else int_aux s bound max_int31

  (* Return an integer between 0 (included) and [bound] (excluded).
     [bound] may be any positive [int]. *)
  let full_int s bound =
    if bound <= 0 then
      invalid_arg "Random.full_int"
    (* When the bound fits in 31-bit signed integers, we use the same mask
       as in function [int] so as to yield the same output on all platforms
       supported by OCaml (32-bit OCaml, 64-bit OCaml, and JavaScript).
       When the bound fits in 32-bit signed integers, we use [max_int32]
       as the mask so as to yield the same output on all platforms where
       [Sys.int_size >= 32] (i.e. JavaScript and 64-bit OCaml). *)
    else
      int_aux s bound
        (if bound <= max_int31 then max_int31
         else if bound <= max_int32 then max_int32
         else max_int)

  (* Return an integer between [min] (included) and [max] (included).
     The [nbits] parameter is the size in bits of the signed integers
     we draw from [s].
     We must have [-2{^nbits - 1} <= min <= max < 2{^nbits - 1}].
     Moreover, for the iteration to converge quickly, the interval
     [[min, max]] should have width at least [2{^nbits - 1}].
     As the width approaches this lower limit, the average number of
     draws approaches 2, with a quite high standard deviation (2 + epsilon). *)
  let rec int_in_large_range s ~min ~max ~nbits =
    let drop = Sys.int_size - nbits in
    (* The bitshifts replicate the [nbits]-th bit (sign bit) to higher bits: *)
    let r = ((Int64.to_int (next s)) lsl drop) asr drop in
    if r < min || r > max then int_in_large_range s ~min ~max ~nbits else r

  (* Return an integer between [min] (included) and [max] (included).
     [mask] is as described for [int_aux].
     [nbits] is as described for [int_in_large_range]. *)
  let int_in_range_aux s ~min ~max ~mask ~nbits =
    let span = max - min + 1 in
    if span <= mask (* [span] is small enough *)
    && span > 0     (* no overflow occurred when computing [span] *)
    then
      (* Just draw a number in [[0, span)] and shift it by [min]. *)
      min + int_aux s span mask
    else
      (* Span too large, use the alternative drawing method. *)
      int_in_large_range s ~min ~max ~nbits

  (* Return an integer between [min] (included) and [max] (included).
     We must have [min <= max]. *)
  let int_in_range s ~min ~max =
    if min > max then
      invalid_arg "Random.int_in_range";
    (* When both bounds fit in 31-bit signed integers, we use parameters
       [mask] and [nbits] appropriate for 31-bit integers, so as to
       yield the same output on all platforms supported by OCaml.
       When both bounds fit in 32-bit signed integers, we use parameters
       [mask] and [nbits] appropriate for 32-bit integers, so as to
       yield the same output on JavaScript and on 64-bit OCaml. *)
    if min >= min_int31 && max <= max_int31 then
      int_in_range_aux s ~min ~max ~mask:max_int31 ~nbits:31
    else if min >= min_int32 && max <= max_int32 then
      int_in_range_aux s ~min ~max ~mask:max_int32 ~nbits:32
    else
      int_in_range_aux s ~min ~max ~mask:max_int ~nbits:Sys.int_size

  (* Return 32 random bits as an [int32] *)
  let bits32 s =
    Int64.to_int32 (next s)

  (* Return an [int32] between 0 (included) and [bound] (excluded). *)
  let rec int32aux s n =
    let r = Int32.shift_right_logical (bits32 s) 1 in
    let v = Int32.rem r n in
    (* Explanation of this test: see comment in [int_aux]. *)
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
      (* Explanation of this test: see comment in [int_in_range_aux]. *)
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
    (* Explanation of this test: see comment in [int_aux]. *)
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
      (* Explanation of this test: see comment in [int_in_range_aux]. *)
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
