(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* "Linear feedback shift register" random number generator. *)
(* References: Robert Sedgewick, "Algorithms", Addison-Wesley *)

(* The PRNG is an additive congruential generator.
   It is seeded by a MD5-based PRNG.
*)

(* This is the state you get with [init 27182818] on a 32-bit machine. *)
let state = [|
  561073064; 1051173471; 764306064; 9858203; 1023641486; 615350359;
  552627506; 486882977; 147054819; 951240904; 869261341; 71648846; 848741663;
  337696531; 66770770; 473370118; 998499212; 477485839; 814302728; 281896889;
  206134737; 796925167; 762624501; 971004788; 878960411; 233350272;
  965168955; 933858406; 572927557; 708896334; 32881167; 462134267; 868098973;
  768795410; 567327260; 4136554; 268309077; 804670393; 854580894; 781847598;
  310632349; 22990936; 187230644; 714526560; 146577263; 979459837; 514922558;
  414383108; 21528564; 896816596; 33747835; 180326017; 414576093; 124177607;
  440266690
|]

let j = ref 0

(* Returns an integer 0 <= x < 1073741824 *)
let raw () =
  j := (!j + 1) mod 55;
  let newval =
    Array.unsafe_get state ((!j+24) mod 55) + Array.unsafe_get state !j in
  Array.unsafe_set state !j newval;
  newval land 0x3FFFFFFF

(* Returns a float 0 <= x < 1 with at most 90 bits of precision. *)
let rawfloat () =
  let scale = 1073741824.0
  and r0 = float (raw ())
  and r1 = float (raw ())
  and r2 = float (raw ())
  in ((r0 /. scale +. r1) /. scale +. r2) /. scale

let rec intaux n =
  let r = raw () in
  if r >= n then intaux n else r
let int bound =
  (intaux (0x4000000 / bound * bound)) mod bound

let float bound = rawfloat () *. bound

(* Simple initialisation.  The seed is an integer.
   Two seeds that are close enough will not produce uncorrelated
   pseudo-random sequences.
*)
let init seed =
  let st = ref seed in
  let mdg () =
    st := !st + 1;
    let d = Digest.string (string_of_int !st) in
    (Char.code d.[0] + (Char.code d.[1] lsl 8) + (Char.code d.[2] lsl 16))
    lxor (Char.code d.[3] lsl 22)
  in
  for i = 0 to 54 do
    Array.unsafe_set state i (mdg ())
  done;
  j := 0

(* Full initialisation.  The seed is an array of integers. *)
let full_init seed =
  init 27182818;
  for i = 0 to Array.length (seed) - 1 do
    let j = i mod 55 in
    Array.unsafe_set state j
      (Array.unsafe_get state j + Array.unsafe_get seed i)
  done

(********************

(* Test functions.  Not included in the library.
   The [chisquare] function should be called with n > 10r.
   It returns a triple (low, actual, high).
   If low <= actual <= high, the [g] function passed the test,
   otherwise it failed.

  Some results (obtained on a 64-bit DEC Alpha):

#Random.init 27182818; chisquare Random.int 100000 1000;;
- : float * float * float = 936.754446797, 992.84, 1063.2455532
#Random.init 27182818; chisquare Random.int 100000 100;;
- : float * float * float = 80, 103.404, 120
#Random.init 27182818; chisquare Random.int 100000 5000;;
- : float * float * float = 4858.57864376, 5004, 5141.42135624
#Random.init 27182818; chisquare Random.int 1000000 1000;;
- : float * float * float = 936.754446797, 975.256, 1063.2455532
#Random.init 27182818; chisquare Random.int 100000 1024;;
- : float * float * float = 960, 1002.93632, 1088
#Random.init 299792643; chisquare Random.int 100000 1024;;
- : float * float * float = 960, 1050.55232, 1088
#Random.init 14142136; chisquare Random.int 100000 1024;;
- : float * float * float = 960, 1074.98496, 1088
#Random.init 27182818; init_diff 1024; chisquare diff 100000 1024;;
- : float * float * float = 960, 1000.94976, 1088
#Random.init 27182818; init_diff 100; chisquare diff 100000 100;;
- : float * float * float = 80, 98.644, 120
#Random.init 27182818; init_diff2 1024; chisquare diff2 100000 1024;;
- : float * float * float = 960, 1008.0768, 1088
#Random.init 27182818; init_diff2 100; chisquare diff2 100000 100;;
- : float * float * float = 80, 110.872, 120
#Random.init 14142136; init_diff2 100; chisquare diff2 100000 100;;
- : float * float * float = 80, 85.412, 120
#Random.init 299792643; init_diff2 100; chisquare diff2 100000 100;;
- : float * float * float = 80, 92.192, 120

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then float v.(i0) *. float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1
;;

let chisquare g n r =
  if n <= 10 * r then invalid_arg "chisquare";
  let f = Array.new r 0 in
  for i = 1 to n do
    let t = g r in
    f.(t) <- f.(t) + 1
  done;
  let t = sumsq f 0 r
  and r = float r
  and n = float n in
  let sr = 2.0 *. sqrt r in
  (r -. sr,   (r *. t /. n) -. n,   r +. sr)
;;

(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref 0;;
let init_diff r = st := Random.int r;;
let diff r =
  let x1 = !st
  and x2 = Random.int r
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
let init_diff2 r = st1 := Random.int r; st2 := Random.int r;;
let diff2 r =
  let x1 = !st1
  and x2 = !st2
  and x3 = Random.int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r
;;

********************)
