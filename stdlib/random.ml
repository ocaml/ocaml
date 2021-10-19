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
      = "caml_xoshiro_next" "caml_xoshiro_next_unboxed"
  external jump: t -> unit = "caml_xoshiro_jump"
(*  external long_jump: t -> unit = "caml_xoshiro_long_jump" *)
  external init: t -> int array -> unit = "caml_xoshiro_init"

  let create () : t =
    Array1.create Int64 C_layout 4

  let make seed =
    let s = create() in init s seed; s

  let make_self_init () =
    make (random_seed ())

  let assign (dst: t) (src: t) =
    Array1.blit src dst

  let copy s =
    let s' = create() in assign s' s; s

  let reinit = init

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

  (* Return a float 0 < x < 1 with at most 53 bits of precision. *)
  let rec rawfloat s =
    let b = next s in
    let n = Int64.shift_right_logical b 11 in
    if n <> 0L then Int64.to_float n *. 0x1.p-53 else rawfloat s

  (* Return a float between 0 and [bound] *)
  let float s bound = rawfloat s *. bound

  (* Return a random Boolean *)
  let bool s = next s < 0L

end

let default =
  State.make
    [| 0b001001000011111101101010100010;
       0b001000010110100011000010001101;
       0b001100010011000110011000101000;
       0b101110000000110111000001110011;
       0b010001001010010000001001001110;
       0b000010001000101001100111110011;
       0b000111010000000010000010111011;
       0b111010100110001110110001001110 |]
(* These are the first 240 binary digits of the fractional part of pi,
   as eight 30-bit integers. *)

let bits () = State.bits default
let int bound = State.int default bound
let full_int bound = State.full_int default bound
let int32 bound = State.int32 default bound
let nativeint bound = State.nativeint default bound
let int64 bound = State.int64 default bound
let float scale = State.float default scale
let bool () = State.bool default
let bits32 () = State.bits32 default
let bits64 () = State.bits64 default
let nativebits () = State.nativebits default

(* Seeding *)

let full_init seed = State.reinit default seed
let init seed = full_init [| seed |]
let self_init () = full_init (random_seed())

(* Generating a new independent state by jumping *)

let jump () =
  let st = State.copy default in State.jump default; st

(* Manipulating the current state. *)

let get_state () = State.copy default
let set_state s = State.assign default s

