(* TEST
 flags = "-drawlambda -dlambda";
 expect;
*)

(* These test casees are meant to study the performance impact of
   strict bindings of mutable fields. Each example lists two
   intermediate representations;

   -drawlambda output: exactly what the pattern compiler produced,
    easier to see the splits / exit handlers

   -dlambda output: after simplificatino, easier to check that some bindings
    have been optimized away as expected.
*)

type ('a, 'b) mut_first = { mutable mut : 'a; immut : 'b ; };;

let _no_mention {immut} = immut in ();;
(* exceptation: no mutable read
   (this expectation is met, as the StrictOpt mutable read is
    optimized away by Simplif.)
*)
[%%expect {|
0
0
type ('a, 'b) mut_first = { mutable mut : 'a; immut : 'b; }
(let
  (_no_mention/279 =
     (function param/282
       (let
         (*strict*/283 =o (field_mut 0 param/282)
          immut/281 =a (field_imm 1 param/282))
         immut/281)))
  0)
(let (_no_mention/279 = (function param/282 (field_imm 1 param/282))) 0)
- : unit = ()
|}];;

let _no_use {immut; mut = _} = immut in ();;
(* exceptation: no mutable read *)
[%%expect {|
(let
  (_no_use/284 =
     (function param/287
       (let
         (*strict*/288 =o (field_mut 0 param/287)
          immut/286 =a (field_imm 1 param/287))
         immut/286)))
  0)
(let (_no_use/284 = (function param/287 (field_imm 1 param/287))) 0)
- : unit = ()
|}];;

let _matching : _ mut_first -> _ = function
| {mut = None} -> 0
| {mut = Some n} -> n
in ();;
(* exceptation: a single mutable read *)
[%%expect {|
(let
  (_matching/289 =
     (function param/292 : int
       (let (*strict*/293 =o (field_mut 0 param/292))
         (if *strict*/293
           (let
             (*match*/295 =a (field_imm 1 param/292)
              n/291 =a (field_imm 0 *strict*/293))
             n/291)
           (let (*match*/294 =a (field_imm 1 param/292)) 0)))))
  0)
(let
  (_matching/289 =
     (function param/292 : int
       (let (*strict*/293 =o (field_mut 0 param/292))
         (if *strict*/293 (field_imm 0 *strict*/293) 0))))
  0)
- : unit = ()
|}];;


let _matching_first : _ mut_first -> _ = function
| {mut = None; immut = false} -> -1
| {mut = None; immut = true} -> 0
| {mut = Some n} -> n
in ();;
(* exceptation: a mutable read, a test, then testing [immut] *)
[%%expect {|
(let
  (_matching_first/296 =
     (function param/299 : int
       (let (*strict*/300 =o (field_mut 0 param/299))
         (if *strict*/300
           (let
             (*match*/302 =a (field_imm 1 param/299)
              n/298 =a (field_imm 0 *strict*/300))
             n/298)
           (let (*match*/301 =a (field_imm 1 param/299))
             (if *match*/301 0 -1))))))
  0)
(let
  (_matching_first/296 =
     (function param/299 : int
       (let (*strict*/300 =o (field_mut 0 param/299))
         (if *strict*/300 (field_imm 0 *strict*/300)
           (if (field_imm 1 param/299) 0 -1)))))
  0)
- : unit = ()
|}];;


type ('a, 'b) mut_second = { immut : 'a; mutable mut : 'b; };;
(* The pattern-matching order is (currently) determined by the field
   order in the type declaration! Swapping the two fields in the
   declaration means that the immutable value will be tested first. *)

let _matching_second_in_two_switches : (_, _) mut_second -> _ = function
| {immut = false; mut = None} -> -1
| {immut = false; mut = Some n} -> -n
| {immut = true ; mut = None} -> 0
| {immut = true ; mut = Some n} -> n
in ();;
(* exceptation: one mutable read as a strict bindings at the top. *)
[%%expect {|
0
0
type ('a, 'b) mut_second = { immut : 'a; mutable mut : 'b; }
(let
  (_matching_second_in_two_switches/306 =
     (function param/310 : int
       (let
         (*strict*/311 =o (field_mut 1 param/310)
          *match*/312 =a (field_imm 0 param/310))
         (if *match*/312
           (if *strict*/311 (let (n/309 =a (field_imm 0 *strict*/311)) n/309)
             0)
           (if *strict*/311
             (let (n/308 =a (field_imm 0 *strict*/311)) (~ n/308)) -1)))))
  0)
(let
  (_matching_second_in_two_switches/306 =
     (function param/310 : int
       (let (*strict*/311 =o (field_mut 1 param/310))
         (if (field_imm 0 param/310)
           (if *strict*/311 (field_imm 0 *strict*/311) 0)
           (if *strict*/311 (~ (field_imm 0 *strict*/311)) -1)))))
  0)
- : unit = ()
|}];;

let _matching_second_in_one_switch : (_, _) mut_second -> _ = function
| {immut = false; _ } -> -1
| {immut = true ; mut = None} -> 0
| {immut = true ; mut = Some n} -> n
in ();;
(* exceptation: a single mutable read in the [true] branch.
   current (suboptimal) behavior: the mutable read is performed as the
   top, so it is computed even in the [false] branch.
*)
[%%expect {|
(let
  (_matching_second_in_one_switch/313 =
     (function param/316 : int
       (let
         (*strict*/317 =o (field_mut 1 param/316)
          *match*/318 =a (field_imm 0 param/316))
         (if *match*/318
           (if *strict*/317 (let (n/315 =a (field_imm 0 *strict*/317)) n/315)
             0)
           -1))))
  0)
(let
  (_matching_second_in_one_switch/313 =
     (function param/316 : int
       (let (*strict*/317 =o (field_mut 1 param/316))
         (if (field_imm 0 param/316)
           (if *strict*/317 (field_imm 0 *strict*/317) 0) -1))))
  0)
- : unit = ()
|}];;

type t = A | B | C | D;;
[%%expect {|
0
0
type t = A | B | C | D
|}];;

let _matching_second_in_one_switch_among_many : (_, _) mut_second -> _ = function
| {immut = A; _ } -> -1
| {immut = B ; mut = None} -> 0
| {immut = B ; mut = Some n} -> n
| {immut = C; _ } -> -2
| {immut = D; _ } -> -3
in ();;
(* exceptation: a single mutable read in the [true] branch.
   current (suboptimal) behavior: the mutable read is performed
   at the beginning, so it is evaluated in all branches. *)
[%%expect {|
(let
  (_matching_second_in_one_switch_among_many/324 =
     (function param/327 : int
       (let
         (*strict*/328 =o (field_mut 1 param/327)
          *match*/329 =a (field_imm 0 param/327))
         (switch* *match*/329
          case int 0: -1
          case int 1:
           (if *strict*/328 (let (n/326 =a (field_imm 0 *strict*/328)) n/326)
             0)
          case int 2: -2
          case int 3: -3))))
  0)
(let
  (_matching_second_in_one_switch_among_many/324 =
     (function param/327 : int
       (let (*strict*/328 =o (field_mut 1 param/327))
         (switch* (field_imm 0 param/327)
          case int 0: -1
          case int 1: (if *strict*/328 (field_imm 0 *strict*/328) 0)
          case int 2: -2
          case int 3: -3))))
  0)
- : unit = ()
|}];;

let _matching_second_in_two_cuts : (_, _) mut_second -> _ = function
| {immut = false; mut = None} -> -1
| {immut = true ; mut = None} -> 0
| {immut = _ ;    mut = Some n} -> n
in ();;
(* exceptation: a single mutable read, shared by the two split submatrices. *)
[%%expect {|
(let
  (_matching_second_in_two_cuts/330 =
     (function param/333 : int
       (let
         (*strict*/334 =o (field_mut 1 param/333)
          *match*/335 =a (field_imm 0 param/333))
         (catch
           (if *match*/335 (if *strict*/334 (exit 1) 0)
             (if *strict*/334 (exit 1) -1))
          with (1) (let (n/332 =a (field_imm 0 *strict*/334)) n/332)))))
  0)
(let
  (_matching_second_in_two_cuts/330 =
     (function param/333 : int
       (let (*strict*/334 =o (field_mut 1 param/333))
         (catch
           (if (field_imm 0 param/333) (if *strict*/334 (exit 1) 0)
             (if *strict*/334 (exit 1) -1))
          with (1) (field_imm 0 *strict*/334)))))
  0)
- : unit = ()
|}];;
