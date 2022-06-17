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
       reproduce their previous state and in fact get a differrent
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
      failwith "Random.State.of_binary_string";
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

  (* Return 30 random bits as an integer 0 <= x < 1073741824 *)
  let bits s =
    Int64.to_int (next s) land 0x3FFF_FFFF

  (* Return an integer between 0 (included) and [bound] (excluded) *)
  let rec intaux s n =
    let r = bits s in
    let v = r mod n in
    if r - v > 0x3FFFFFFF - n + 1 then intaux s n else v

  let int s bound =
    if bound > 0x3FFFFFFF || bound <= 0
    then invalid_arg "Random.int"
    else intaux s bound

  (* Return an integer between 0 (included) and [bound] (excluded).
     [bound] may be any positive [int]. *)
  let rec int63aux s n =
    let r = Int64.to_int (next s) land max_int in
    let v = r mod n in
    if r - v > max_int - n + 1 then int63aux s n else v

  let full_int s bound =
    if bound <= 0 then
      invalid_arg "Random.full_int"
    else if bound > 0x3FFFFFFF then
      int63aux s bound
    else
      intaux s bound

  (* Return 32 random bits as an [int32] *)
  let bits32 s =
    Int64.to_int32 (next s)

  (* Return an [int32] between 0 (included) and [bound] (excluded). *)
  let rec int32aux s n =
    let r = Int32.shift_right_logical (bits32 s) 1 in
    let v = Int32.rem r n in
    if Int32.(sub r v > add (sub max_int n) 1l)
    then int32aux s n
    else v

  let int32 s bound =
    if bound <= 0l
    then invalid_arg "Random.int32"
    else int32aux s bound

  (* Return 64 random bits as an [int64] *)
  let bits64 s =
    next s

  (* Return an [int64] between 0 (included) and [bound] (excluded). *)
  let rec int64aux s n =
    let r = Int64.shift_right_logical (bits64 s) 1 in
    let v = Int64.rem r n in
    if Int64.(sub r v > add (sub max_int n) 1L)
    then int64aux s n
    else v

  let int64 s bound =
    if bound <= 0L
    then invalid_arg "Random.int64"
    else int64aux s bound

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
let int32 bound = State.int32 (Domain.DLS.get random_key) bound
let nativeint bound = State.nativeint (Domain.DLS.get random_key) bound
let int64 bound = State.int64 (Domain.DLS.get random_key) bound
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
