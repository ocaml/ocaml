(* TEST
   flags = "-drawlambda"
   * expect
*)

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

(* This tests needs to allocate the tuple to bind 'x',
   but this is only done in the branches that use it. *)
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
   *match*/103 = *match*/96
   *match*/104 = *match*/97
   *match*/105 = *match*/98)
  (catch
    (catch
      (catch
        (if (!= *match*/104 3) (exit 6)
          (let (x/102 =a (makeblock 0 *match*/96 *match*/97 *match*/98))
            (exit 4 x/102)))
       with (6)
        (if (!= *match*/103 1) (exit 5)
          (let (x/100 =a (makeblock 0 *match*/96 *match*/97 *match*/98))
            (exit 4 x/100))))
     with (5) 0)
   with (4 x/94) (seq (ignore x/94) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/106 b/107 (let (*match*/110 = a/106 *match*/111 = b/107) 0))
- : bool -> 'a -> unit = <fun>
|}];;

(* More complete tests.

   The test cases below compare the compiler output on alias patterns
   that are outside an or-pattern (handled during half-simplification,
   then flattened) or inside an or-pattern (handled during simplification).

   We used to have a Cannot_flatten exception that would result in fairly
   different code generated in both cases, but now the compilation strategy
   is fairly similar.
*)
let _ = fun a b -> match a, b with
| (true, _) as p -> p
| (false, _) as p -> p
(* outside, trivial *)
[%%expect {|
(function a/112 b/113
  (let
    (*match*/116 = a/112
     *match*/117 = b/113
     p/114 =a (makeblock 0 a/112 b/113))
    p/114))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/118 b/119
  (let
    (*match*/126 = a/118
     *match*/127 = b/119
     p/120 =a (makeblock 0 a/118 b/119))
    p/120))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/128 b/129
  (let
    (*match*/134 = a/128
     *match*/135 = b/129
     x/130 =a *match*/134
     p/131 =a (makeblock 0 a/128 b/129))
    (makeblock 0 x/130 p/131)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/136 b/137
  (let
    (*match*/148 = a/136
     *match*/149 = b/137
     x/138 =a *match*/148
     p/139 =a (makeblock 0 a/136 b/137))
    (makeblock 0 x/138 p/139)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/150 b/151
  (let (*match*/156 = a/150 *match*/157 = b/151)
    (if *match*/156
      (let (x/152 =a *match*/156 p/153 =a (makeblock 0 a/150 b/151))
        (makeblock 0 x/152 p/153))
      (let (x/154 =a *match*/157 p/155 =a (makeblock 0 a/150 b/151))
        (makeblock 0 x/154 p/155)))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/158 b/159
  (let (*match*/170 = a/158 *match*/171 = b/159)
    (catch
      (if *match*/170
        (let (x/167 =a *match*/170 p/169 =a (makeblock 0 a/158 b/159))
          (exit 10 x/167 p/169))
        (let (x/164 =a *match*/171 p/166 =a (makeblock 0 a/158 b/159))
          (exit 10 x/164 p/166)))
     with (10 x/160 p/161) (makeblock 0 x/160 p/161))))
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
(function a/172 b/173
  (let (*match*/178 = a/172 *match*/179 = b/173)
    (if *match*/178
      (let (x/174 =a *match*/178 _p/175 =a (makeblock 0 a/172 b/173))
        (makeblock 0 x/174 [0: 1 1]))
      (let (x/176 =a *match*/178 p/177 =a (makeblock 0 a/172 b/173))
        (makeblock 0 x/176 p/177)))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/180 b/181
  (let
    (*match*/192 = a/180
     *match*/193 = b/181
     x/182 =a *match*/192
     p/183 =a (makeblock 0 a/180 b/181))
    (makeblock 0 x/182 p/183)))
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
(function a/197 b/198
  (let (*match*/201 = a/197 *match*/202 = b/198)
    (catch
      (if *match*/201
        (if *match*/202 (let (p/199 =a (field 0 *match*/202)) p/199)
          (exit 12))
        (exit 12))
     with (12) (let (p/200 =a (makeblock 0 a/197 b/198)) p/200))))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/203 b/204
  (let (*match*/210 = a/203 *match*/211 = b/204)
    (catch
      (catch
        (if *match*/210
          (if *match*/211
            (let (p/209 =a (field 0 *match*/211)) (exit 13 p/209)) (exit 14))
          (exit 14))
       with (14) (let (p/208 =a (makeblock 0 a/203 b/204)) (exit 13 p/208)))
     with (13 p/205) p/205)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
