(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* "Linear feedback shift register" pseudo-random number generator. *)
(* References: Robert Sedgewick, "Algorithms", Addison-Wesley *)

(* The PRNG is a linear feedback shift register.
   It is seeded by a MD5-based PRNG.
*)

type state = { st : int array; mutable idx : int };;

(* This is the state you get with [init 27182818] on a 32-bit machine. *)
let default = {
  st = [|
    561073064; 1051173471; 764306064; 9858203; 1023641486; 615350359;
    552627506; 486882977; 147054819; 951240904; 869261341; 71648846;
    848741663; 337696531; 66770770; 473370118; 998499212; 477485839;
    814302728; 281896889; 206134737; 796925167; 762624501; 971004788;
    878960411; 233350272; 965168955; 933858406; 572927557; 708896334;
    32881167; 462134267; 868098973; 768795410; 567327260; 4136554;
    268309077; 804670393; 854580894; 781847598; 310632349; 22990936;
    187230644; 714526560; 146577263; 979459837; 514922558; 414383108;
    21528564; 896816596; 33747835; 180326017; 414576093; 124177607;
    440266690;
  |];
  idx = 0;
};;

(* Returns 30 random bits as an integer 0 <= x < 1073741824 *)
let s_bits s =
  s.idx <- (s.idx + 1) mod 55;
  let newval = s.st.((s.idx + 24) mod 55) + s.st.(s.idx) in
  s.st.(s.idx) <- newval;
  newval land 0x3FFFFFFF
;;

(* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
let s_rawfloat s =
  let scale = 1073741824.0
  and r0 = Pervasives.float (s_bits s)
  and r1 = Pervasives.float (s_bits s)
  and r2 = Pervasives.float (s_bits s)
  in ((r0 /. scale +. r1) /. scale +. r2) /. scale
;;

let rec s_intaux s n =
  let r = s_bits s in
  if r >= n then s_intaux s n else r
;;
let s_int s bound =
  if bound > 0x3FFFFFFF || bound <= 0
  then invalid_arg "Random.int"
  else (s_intaux s (0x3FFFFFFF / bound * bound)) mod bound
;;

let s_float s bound = s_rawfloat s *. bound

let s_bool s = (s_bits s land 1 = 0);;

let bits () = s_bits default;;
let int bound = s_int default bound;;
let float scale = s_float default scale;;
let bool () = s_bool default;;

(* Full initialisation.  The seed is an array of integers. *)
let s_full_init s seed =
  let combine accu x = Digest.string (accu ^ string_of_int x) in
  let extract d =
    (Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16))
    lxor (Char.code d.[3] lsl 22)
  in
  let l = Array.length seed in
  for i = 0 to 54 do
    s.st.(i) <- i;
  done;
  let accu = ref "x" in
  for i = 0 to 54 + max 55 l do
    let j = i mod 55 in
    let k = i mod l in
    accu := combine !accu seed.(k);
    s.st.(j) <- s.st.(j) lxor extract !accu;
  done;
  s.idx <- 0;
;;

let full_init seed = s_full_init default seed;;

(* Simple initialisation.  The seed is an integer. *)
let init seed = s_full_init default [| seed |];;

(* Low-entropy system-dependent initialisation. *)
external random_seed: unit -> int = "sys_random_seed";;
let self_init () = init (random_seed());;

(* The default PRNG is initialised with self_init. *)
self_init ();;

let new_state () = { st = Array.make 55 0; idx = 0 };;
let assign_state st1 st2 =
  Array.blit st2.st 0 st1.st 0 55;
  st1.idx <- st2.idx;
;;

(* Create, initialise, and return a new state value. *)
let s_make seed =
  let result = new_state () in
  s_full_init result seed;
  result
;;

let s_copy s =
  let result = new_state () in
  assign_state result s;
  result
;;

(* Manipulating the current state. *)

let get_state () = s_copy default;;
let set_state s = assign_state default s;;

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results:

init 27182818; chisquare int 100000 1000;;
init 27182818; chisquare int 100000 100;;
init 27182818; chisquare int 100000 5000;;
init 27182818; chisquare int 1000000 1000;;
init 27182818; chisquare int 100000 1024;;
init 299792643; chisquare int 100000 1024;;
init 14142136; chisquare int 100000 1024;;
init 27182818; init_diff 1024; chisquare diff 100000 1024;;
init 27182818; init_diff 100; chisquare diff 100000 100;;
init 27182818; init_diff2 1024; chisquare diff2 100000 1024;;
init 27182818; init_diff2 100; chisquare diff2 100000 100;;
init 14142136; init_diff2 100; chisquare diff2 100000 100;;
init 299792643; init_diff2 100; chisquare diff2 100000 100;;
- : float * float * float = (936.754446796632465, 1032., 1063.24555320336754)
# - : float * float * float = (80., 91.3699999999953434, 120.)
# - : float * float * float = (4858.57864376269026, 4982., 5141.42135623730974)
# - : float * float * float =
(936.754446796632465, 1017.99399999994785, 1063.24555320336754)
# - : float * float * float = (960., 984.565759999997681, 1088.)
# - : float * float * float = (960., 1003.40735999999742, 1088.)
# - : float * float * float = (960., 1035.23328000000038, 1088.)
# - : float * float * float = (960., 1026.79551999999967, 1088.)
# - : float * float * float = (80., 110.194000000003143, 120.)
# - : float * float * float = (960., 1067.98080000000482, 1088.)
# - : float * float * float = (80., 107.292000000001281, 120.)
# - : float * float * float = (80., 85.1180000000022119, 120.)
# - : float * float * float = (80., 86.614000000001397, 120.)

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then Pervasives.float v.(i0) *. Pervasives.float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1
;;

let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.make r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = Pervasives.float r
  and n = Pervasives.float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)
;;

(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0;;
let init_diff r = st := int r;;
let diff r =
  let x1 = !st
  and x2 = int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2
;;

let st1 = ref 0
and st2 = ref 0
;;

(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let init_diff2 r = st1 := int r; st2 := int r;;
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r
;;

********************)
