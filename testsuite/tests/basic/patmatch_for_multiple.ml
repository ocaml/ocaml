(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let
  (*match*/88 = 3
   *match*/89 = 2
   *match*/90 = 1
   *match*/91 = *match*/88
   *match*/92 = *match*/89
   *match*/93 = *match*/90)
  (catch
    (catch
      (catch (if (!= *match*/92 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/91 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let
  (*match*/96 = 3
   *match*/97 = 2
   *match*/98 = 1
   *match*/99 = (makeblock 0 *match*/96 *match*/97 *match*/98))
  (catch
    (catch
      (let (*match*/100 =a (field 0 *match*/99))
        (catch
          (let (*match*/101 =a (field 1 *match*/99))
            (if (!= *match*/101 3) (exit 7)
              (let (*match*/102 =a (field 2 *match*/99)) (exit 5 *match*/99))))
         with (7)
          (if (!= *match*/100 1) (exit 6)
            (let
              (*match*/104 =a (field 2 *match*/99)
               *match*/103 =a (field 1 *match*/99))
              (exit 5 *match*/99)))))
     with (6) 0)
   with (5 x/94) (seq (ignore x/94) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/105 b/106 (let (*match*/109 = a/105 *match*/110 = b/106) 0))
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The current strategy of the compiler is to determine whether
   flattening of tuple patterns is possible during precompilation,
   after the pattern has been half-simplified (toplevel alias
   patterns are gone), during simplification (explosion of
   or-patterns). Flattening fail if there is an alias pattern found
   under an or-pattern during explosion.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during
   half-simplification, then flattened) or inside an or-pattern
   (handled during simplification, blocks flattening).

   TL;DR:
   - outside: flattening happens, good code generated
   - inside: raises Cannot_flatten, worse code generated
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/111 b/112
  (let
    (*match*/115 = a/111
     *match*/116 = b/112
     p/113 =a (makeblock 0 a/111 b/112))
    p/113))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/117 b/118
  (let (*match*/121 = (makeblock 0 a/117 b/118) p/119 =a *match*/121) p/119))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/125 b/126
  (let
    (*match*/131 = a/125
     *match*/132 = b/126
     x/127 =a *match*/131
     p/128 =a (makeblock 0 a/125 b/126))
    (makeblock 0 x/127 p/128)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/133 b/134
  (let
    (*match*/140 = (makeblock 0 a/133 b/134)
     x/135 =a (field 0 *match*/140)
     p/136 =a *match*/140)
    (makeblock 0 x/135 p/136)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/145 b/146
  (let (*match*/151 = a/145 *match*/152 = b/146)
    (if *match*/151
      (let (x/147 =a *match*/151 p/148 =a (makeblock 0 a/145 b/146))
        (makeblock 0 x/147 p/148))
      (let (x/149 =a *match*/152 p/150 =a (makeblock 0 a/145 b/146))
        (makeblock 0 x/149 p/150)))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/153 b/154
  (let (*match*/160 = (makeblock 0 a/153 b/154))
    (catch
      (let (x/162 =a (field 0 *match*/160))
        (if x/162
          (let (*match*/163 =a (field 1 *match*/160))
            (exit 14 x/162 *match*/160))
          (let (x/161 =a (field 1 *match*/160)) (exit 14 x/161 *match*/160))))
     with (14 x/155 p/156) (makeblock 0 x/155 p/156))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation,
   as we avoid allocating the tuple in the first case,
   and only allocate in the second case *)

let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/164 b/165
  (let (*match*/170 = a/164 *match*/171 = b/165)
    (if *match*/170
      (let (x/166 =a *match*/170 _p/167 =a (makeblock 0 a/164 b/165))
        (makeblock 0 x/166 [0: 1 1]))
      (let (x/168 =a *match*/170 p/169 =a (makeblock 0 a/164 b/165))
        (makeblock 0 x/168 p/169)))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/172 b/173
  (let
    (*match*/179 = (makeblock 0 a/172 b/173)
     x/174 =a (field 0 *match*/179)
     p/175 =a *match*/179)
    (makeblock 0 x/174 p/175)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/187 b/188
  (let (*match*/191 = a/187 *match*/192 = b/188)
    (catch
      (if *match*/191
        (if *match*/192 (let (p/189 =a (field 0 *match*/192)) p/189)
          (exit 17))
        (exit 17))
     with (17) (let (p/190 =a (makeblock 0 a/187 b/188)) p/190))))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

(* if we cannot flatten, we generate worse code *)
let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/193 b/194
  (let (*match*/197 = (makeblock 0 a/193 b/194))
    (catch
      (let (*match*/199 =a (field 0 *match*/197))
        (catch
          (if *match*/199
            (let (*match*/200 =a (field 1 *match*/197))
              (if *match*/200
                (let (p/198 =a (field 0 *match*/200)) (exit 19 p/198))
                (exit 20)))
            (exit 20))
         with (20)
          (let (*match*/201 =a (field 1 *match*/197)) (exit 19 *match*/197))))
     with (19 p/195) p/195)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
