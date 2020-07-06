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
(let (*match*/88 = 3 *match*/89 = 2 *match*/90 = 1)
  (catch
    (catch
      (catch (if (!= *match*/89 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/88 1) (exit 2) (exit 1)))
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
(let (*match*/93 = 3 *match*/94 = 2 *match*/95 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/94 3) (exit 6)
          (let (x/99 =a (makeblock 0 *match*/93 *match*/94 *match*/95))
            (exit 4 x/99)))
       with (6)
        (if (!= *match*/93 1) (exit 5)
          (let (x/97 =a (makeblock 0 *match*/93 *match*/94 *match*/95))
            (exit 4 x/97))))
     with (5) 0)
   with (4 x/91) (seq (ignore x/91) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/100 b/101 0)
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
(function a/104 b/105 (let (p/106 =a (makeblock 0 a/104 b/105)) p/106))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/108 b/109 (let (p/110 =a (makeblock 0 a/108 b/109)) p/110))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/116 b/117
  (let (x/118 =a a/116 p/119 =a (makeblock 0 a/116 b/117))
    (makeblock 0 x/118 p/119)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/122 b/123
  (let (x/124 =a a/122 p/125 =a (makeblock 0 a/122 b/123))
    (makeblock 0 x/124 p/125)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/134 b/135
  (if a/134
    (let (x/136 =a a/134 p/137 =a (makeblock 0 a/134 b/135))
      (makeblock 0 x/136 p/137))
    (let (x/138 =a b/135 p/139 =a (makeblock 0 a/134 b/135))
      (makeblock 0 x/138 p/139))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/140 b/141
  (catch
    (if a/140
      (let (x/149 =a a/140 p/151 =a (makeblock 0 a/140 b/141))
        (exit 10 x/149 p/151))
      (let (x/146 =a b/141 p/148 =a (makeblock 0 a/140 b/141))
        (exit 10 x/146 p/148)))
   with (10 x/142 p/143) (makeblock 0 x/142 p/143)))
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
(function a/152 b/153
  (if a/152
    (let (x/154 =a a/152 _p/155 =a (makeblock 0 a/152 b/153))
      (makeblock 0 x/154 [0: 1 1]))
    (let (x/156 =a a/152 p/157 =a (makeblock 0 a/152 b/153))
      (makeblock 0 x/156 p/157))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/158 b/159
  (let (x/160 =a a/158 p/161 =a (makeblock 0 a/158 b/159))
    (makeblock 0 x/160 p/161)))
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
(function a/173 b/174
  (catch
    (if a/173 (if b/174 (let (p/175 =a (field 0 b/174)) p/175) (exit 12))
      (exit 12))
   with (12) (let (p/176 =a (makeblock 0 a/173 b/174)) p/176)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/177 b/178
  (catch
    (catch
      (if a/177
        (if b/178 (let (p/183 =a (field 0 b/178)) (exit 13 p/183)) (exit 14))
        (exit 14))
     with (14) (let (p/182 =a (makeblock 0 a/177 b/178)) (exit 13 p/182)))
   with (13 p/179) p/179))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
