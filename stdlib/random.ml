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
   It is seeded by a LCG.  The problem with this method is
   that the LCG gives alternated odd and even numbers.  This
   means that the first 30 numbers of the ACG will be even.
   (Thanks to Malik Bouabsa for the bug report.)
   To avoid this problem we throw away the 10 low-order bits of the LCG.
*)

(* This is the state you get with [init 27182818] on a 32-bit machine. *)
let state = [|
 -204961010;    1246631;  357783517;  184869393;  535995446;
   41174190;  550175840;  815470094; -460644623;  251426447;
 -119043119;  -69327520; -165739721; -137021568;  276159658;
 -348545345; -239477247; -206765893; -624895390;  966352356;
  216539461; -844199816;  -74138255;  241691526; -977992707;
  361144813; -146902192;  764665181;  313385504; -815751852;
 -494338442;  663172641;  316513447;  602482660;  314627675;
  154060171; 1037040008;  273361877; -844019849;  200738898;
  328303549;  372486240; -106507453; -976615378; -860951746;
 -338806341; -848457675; -652788777;  171074817;  734439712;
    5343687;  617047814;   94964480;   88677575;  -98623375
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

(* We avoid using the least significant bits whenever possible *)
let int bound =
  if (bound <= 0x400000) then
    (raw () / (0x400000/bound)) mod bound
  else
    raw () mod bound

let float bound = rawfloat () *. bound

(* Simple initialisation.  The seed is an integer. *)
let init seed =
  let st = ref seed in
  let lcg () =
    st := !st * 314159221 + 1;
    (!st lsr 10) land 0xFFFF
  in
  for i = 0 to 54 do
    Array.unsafe_set state i (lcg () + (lcg () lsl 15))
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

#Random.init 27182818; chisquare Random.int 100000 1000
- : float * float * float = 936.754446797, 974.02, 1063.2455532
#Random.init 27182818; chisquare Random.int 100000 100
- : float * float * float = 80, 90.792, 120
#Random.init 27182818; chisquare Random.int 100000 5000
- : float * float * float = 4858.57864376, 4982.7, 5141.42135624
#Random.init 27182818; chisquare Random.int 1000000 1000
- : float * float * float = 936.754446797, 991.71, 1063.2455532
#Random.init 27182818; chisquare Random.int 100000 1024
- : float * float * float = 960, 985.40544, 1088
#Random.init 299792643; chisquare Random.int 100000 1024
- : float * float * float = 960, 971.76576, 1088
#Random.init 14142136; chisquare Random.int 100000 1024
- : float * float * float = 960, 987.6992, 1088
#Random.init 27182818; chisquare diff 100000 1024
- : float * float * float = 960, 1048.13568, 1088
#Random.init 27182818; chisquare diff 100000 100
- : float * float * float = 80, 97.385, 120
#Random.init 27182818; chisquare diff2 100000 1024
- : float * float * float = 960, 986.71616, 1088
#Random.init 27182818; chisquare diff2 100000 100
- : float * float * float = 80, 126.645, 120         <<<<<<<<<<
#Random.init 14142136; chisquare diff2 100000 100
- : float * float * float = 80, 118.578, 120
#Random.init 299792643; chisquare diff2 100000 100
- : float * float * float = 80, 91.488, 120

*)

(* Return the sum of the squares of v[i0,i1[ *)
let rec sumsq v i0 i1 =
  if i0 >= i1 then 0.0
  else if i1 = i0 + 1 then float v.(i0) *. float v.(i0)
  else sumsq v i0 ((i0+i1)/2) +. sumsq v ((i0+i1)/2) i1

let chisquare g n r =
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

(* This is to test for linear dependencies between successive random numbers.
*)
let st = ref (-1)
let diff r =
  if !st = -1 then st := Random.int r;
  let x1 = !st
  and x2 = Random.int r
  in
  st := x2;
  if x1 >= x2 then
    x1 - x2
  else
    r + x1 - x2

let st1 = ref (-1)
and st2 = ref (-1)

(* This is to test for quadratic dependencies between successive random
   numbers.
*)
let diff2 r =
  if !st1 = -1 then (st1 := Random.int r; st2 := Random.int r);
  let x1 = !st1
  and x2 = !st2
  and x3 = Random.int r
  in
  st1 := x2;
  st2 := x3;
  (x3 - x2 - x2 + x1 + 2*r) mod r

********************)
