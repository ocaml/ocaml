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
          (let (x/97 =a (makeblock 0 *match*/93 *match*/94 *match*/95))
            (exit 4 x/97)))
       with (6)
        (if (!= *match*/93 1) (exit 5)
          (let (x/96 =a (makeblock 0 *match*/93 *match*/94 *match*/95))
            (exit 4 x/96))))
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
(function a/98 b/99 0)
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
(function a/102 b/103 (let (p/104 =a (makeblock 0 a/102 b/103)) p/104))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/106 b/107 (let (p/108 =a (makeblock 0 a/106 b/107)) p/108))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/112 b/113
  (let (x/114 =a a/112 p/115 =a (makeblock 0 a/112 b/113))
    (makeblock 0 x/114 p/115)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/118 b/119
  (let (x/120 =a a/118 p/121 =a (makeblock 0 a/118 b/119))
    (makeblock 0 x/120 p/121)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/128 b/129
  (if a/128
    (let (x/130 =a a/128 p/131 =a (makeblock 0 a/128 b/129))
      (makeblock 0 x/130 p/131))
    (let (x/132 =a b/129 p/133 =a (makeblock 0 a/128 b/129))
      (makeblock 0 x/132 p/133))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/134 b/135
  (catch
    (if a/134
      (let (x/142 =a a/134 p/143 =a (makeblock 0 a/134 b/135))
        (exit 10 x/142 p/143))
      (let (x/140 =a b/135 p/141 =a (makeblock 0 a/134 b/135))
        (exit 10 x/140 p/141)))
   with (10 x/136 p/137) (makeblock 0 x/136 p/137)))
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
(function a/144 b/145
  (if a/144
    (let (x/146 =a a/144 _p/147 =a (makeblock 0 a/144 b/145))
      (makeblock 0 x/146 [0: 1 1]))
    (let (x/148 =a a/144 p/149 =a (makeblock 0 a/144 b/145))
      (makeblock 0 x/148 p/149))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/150 b/151
  (let (x/152 =a a/150 p/153 =a (makeblock 0 a/150 b/151))
    (makeblock 0 x/152 p/153)))
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
(function a/163 b/164
  (catch
    (if a/163 (if b/164 (let (p/165 =a (field 0 b/164)) p/165) (exit 12))
      (exit 12))
   with (12) (let (p/166 =a (makeblock 0 a/163 b/164)) p/166)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/167 b/168
  (catch
    (catch
      (if a/167
        (if b/168 (let (p/172 =a (field 0 b/168)) (exit 13 p/172)) (exit 14))
        (exit 14))
     with (14) (let (p/171 =a (makeblock 0 a/167 b/168)) (exit 13 p/171)))
   with (13 p/169) p/169))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
