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
(* exceptation: no mutable read *)
[%%expect {|
0
0
type ('a, 'b) mut_first = { mutable mut : 'a; immut : 'b; }
(let
  (_no_mention/279 =
     (function param/282
       (let
         (*match*/283 =o (field_mut 0 param/282)
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
         (*match*/288 =o (field_mut 0 param/287)
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
       (let (*match*/293 =o (field_mut 0 param/292))
         (if *match*/293
           (let
             (*match*/295 =a (field_imm 1 param/292)
              n/291 =a (field_imm 0 *match*/293))
             n/291)
           (let (*match*/294 =a (field_imm 1 param/292)) 0)))))
  0)
(let
  (_matching/289 =
     (function param/292 : int
       (let (*match*/293 =o (field_mut 0 param/292))
         (if *match*/293 (field_imm 0 *match*/293) 0))))
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
       (let (*match*/300 =o (field_mut 0 param/299))
         (if *match*/300
           (let
             (*match*/302 =a (field_imm 1 param/299)
              n/298 =a (field_imm 0 *match*/300))
             n/298)
           (let (*match*/301 =a (field_imm 1 param/299))
             (if *match*/301 0 -1))))))
  0)
(let
  (_matching_first/296 =
     (function param/299 : int
       (let (*match*/300 =o (field_mut 0 param/299))
         (if *match*/300 (field_imm 0 *match*/300)
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
(* exceptation: two mutable read, one in each branch of an 'if' on [immut] *)
[%%expect {|
0
0
type ('a, 'b) mut_second = { immut : 'a; mutable mut : 'b; }
(let
  (_matching_second_in_two_switches/306 =
     (function param/310 : int
       (let (*match*/311 =a (field_imm 0 param/310))
         (if *match*/311
           (let (*match*/313 =o (field_mut 1 param/310))
             (if *match*/313 (let (n/309 =a (field_imm 0 *match*/313)) n/309)
               0))
           (let (*match*/312 =o (field_mut 1 param/310))
             (if *match*/312
               (let (n/308 =a (field_imm 0 *match*/312)) (~ n/308)) -1))))))
  0)
(let
  (_matching_second_in_two_switches/306 =
     (function param/310 : int
       (if (field_imm 0 param/310)
         (let (*match*/313 =o (field_mut 1 param/310))
           (if *match*/313 (field_imm 0 *match*/313) 0))
         (let (*match*/312 =o (field_mut 1 param/310))
           (if *match*/312 (~ (field_imm 0 *match*/312)) -1)))))
  0)
- : unit = ()
|}];;

let _matching_second_in_one_switch : (_, _) mut_second -> _ = function
| {immut = false; _ } -> -1
| {immut = true ; mut = None} -> 0
| {immut = true ; mut = Some n} -> n
in ();;
(* exceptation: a single mutable read in the [true] branch. *)
[%%expect {|
(let
  (_matching_second_in_one_switch/314 =
     (function param/317 : int
       (let (*match*/318 =a (field_imm 0 param/317))
         (if *match*/318
           (let (*match*/320 =o (field_mut 1 param/317))
             (if *match*/320 (let (n/316 =a (field_imm 0 *match*/320)) n/316)
               0))
           (let (*match*/319 =o (field_mut 1 param/317)) -1)))))
  0)
(let
  (_matching_second_in_one_switch/314 =
     (function param/317 : int
       (if (field_imm 0 param/317)
         (let (*match*/320 =o (field_mut 1 param/317))
           (if *match*/320 (field_imm 0 *match*/320) 0))
         -1)))
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
(* exceptation: a single mutable read in the [true] branch. *)
[%%expect {|
(let
  (_matching_second_in_one_switch_among_many/326 =
     (function param/329 : int
       (let (*match*/330 =a (field_imm 0 param/329))
         (switch* *match*/330
          case int 0: (let (*match*/331 =o (field_mut 1 param/329)) -1)
          case int 1:
           (let (*match*/332 =o (field_mut 1 param/329))
             (if *match*/332 (let (n/328 =a (field_imm 0 *match*/332)) n/328)
               0))
          case int 2: (let (*match*/333 =o (field_mut 1 param/329)) -2)
          case int 3: (let (*match*/334 =o (field_mut 1 param/329)) -3)))))
  0)
(let
  (_matching_second_in_one_switch_among_many/326 =
     (function param/329 : int
       (switch* (field_imm 0 param/329)
        case int 0: -1
        case int 1:
         (let (*match*/332 =o (field_mut 1 param/329))
           (if *match*/332 (field_imm 0 *match*/332) 0))
        case int 2: -2
        case int 3: -3)))
  0)
- : unit = ()
|}];;

let _matching_second_in_two_cuts : (_, _) mut_second -> _ = function
| {immut = false; mut = None} -> -1
| {immut = true ; mut = None} -> 0
| {immut = _ ;    mut = Some n} -> n
in ();;
(* exceptation: a mutable read in each switch -- the second is in an
   exit handler. *)
[%%expect {|
(let
  (_matching_second_in_two_cuts/335 =
     (function param/338 : int
       (let (*match*/339 =a (field_imm 0 param/338))
         (catch
           (if *match*/339
             (let (*match*/341 =o (field_mut 1 param/338))
               (if *match*/341 (exit 1) 0))
             (let (*match*/340 =o (field_mut 1 param/338))
               (if *match*/340 (exit 1) -1)))
          with (1)
           (let
             (*match*/342 =o (field_mut 1 param/338)
              n/337 =a (field_imm 0 *match*/342))
             n/337)))))
  0)
(let
  (_matching_second_in_two_cuts/335 =
     (function param/338 : int
       (catch
         (if (field_imm 0 param/338)
           (let (*match*/341 =o (field_mut 1 param/338))
             (if *match*/341 (exit 1) 0))
           (let (*match*/340 =o (field_mut 1 param/338))
             (if *match*/340 (exit 1) -1)))
        with (1)
         (let (*match*/342 =o (field_mut 1 param/338))
           (field_imm 0 *match*/342)))))
  0)
- : unit = ()
|}];;
