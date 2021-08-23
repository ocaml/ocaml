(* TEST
   flags = "-drawlambda -dlambda"
   * expect
*)

(* Note: the tests below contain *both* the -drawlambda and
   the -dlambda intermediate representations:
   -drawlambda is the Lambda code generated directly by the
     pattern-matching compiler; it contain "alias" bindings or static
     exits that are unused, and will be removed by simplification, or
     that are used only once, and will be inlined by simplification.
   -dlambda is the Lambda code resulting from simplification.

  The -drawlambda output more closely matches what the
  pattern-compiler produces, and the -dlambda output more closely
  matches the final generated code.

  In this test we decided to show both to notice that some allocations
  are "optimized away" during simplification (see "here flattening is
  an optimization" below).
*)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/90 = 3 *match*/91 = 2 *match*/92 = 1)
  (catch
    (catch
      (catch (if (!= *match*/91 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/90 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/90 = 3 *match*/91 = 2 *match*/92 = 1)
  (catch (if (!= *match*/91 3) (if (!= *match*/90 1) 0 (exit 1)) (exit 1))
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
(let (*match*/95 = 3 *match*/96 = 2 *match*/97 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/96 3) (exit 6)
          (let (x/99 =a (makeblock 0 *match*/95 *match*/96 *match*/97))
            (exit 4 x/99)))
       with (6)
        (if (!= *match*/95 1) (exit 5)
          (let (x/98 =a (makeblock 0 *match*/95 *match*/96 *match*/97))
            (exit 4 x/98))))
     with (5) 0)
   with (4 x/93) (seq (ignore x/93) 1)))
(let (*match*/95 = 3 *match*/96 = 2 *match*/97 = 1)
  (catch
    (if (!= *match*/96 3)
      (if (!= *match*/95 1) 0
        (exit 4 (makeblock 0 *match*/95 *match*/96 *match*/97)))
      (exit 4 (makeblock 0 *match*/95 *match*/96 *match*/97)))
   with (4 x/93) (seq (ignore x/93) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/100[int] b/101 : int 0)
(function a/100[int] b/101 : int 0)
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
(function a/104[int] b/105 (let (p/106 =a (makeblock 0 a/104 b/105)) p/106))
(function a/104[int] b/105 (makeblock 0 a/104 b/105))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/108[int] b/109 (let (p/110 =a (makeblock 0 a/108 b/109)) p/110))
(function a/108[int] b/109 (makeblock 0 a/108 b/109))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/114[int] b/115
  (let (x/116 =a[int] a/114 p/117 =a (makeblock 0 a/114 b/115))
    (makeblock 0 (int,*) x/116 p/117)))
(function a/114[int] b/115
  (makeblock 0 (int,*) a/114 (makeblock 0 a/114 b/115)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/120[int] b/121
  (let (x/122 =a[int] a/120 p/123 =a (makeblock 0 a/120 b/121))
    (makeblock 0 (int,*) x/122 p/123)))
(function a/120[int] b/121
  (makeblock 0 (int,*) a/120 (makeblock 0 a/120 b/121)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/130[int] b/131[int]
  (if a/130
    (let (x/132 =a[int] a/130 p/133 =a (makeblock 0 a/130 b/131))
      (makeblock 0 (int,*) x/132 p/133))
    (let (x/134 =a b/131 p/135 =a (makeblock 0 a/130 b/131))
      (makeblock 0 (int,*) x/134 p/135))))
(function a/130[int] b/131[int]
  (if a/130 (makeblock 0 (int,*) a/130 (makeblock 0 a/130 b/131))
    (makeblock 0 (int,*) b/131 (makeblock 0 a/130 b/131))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/136[int] b/137[int]
  (catch
    (if a/136
      (let (x/144 =a[int] a/136 p/145 =a (makeblock 0 a/136 b/137))
        (exit 10 x/144 p/145))
      (let (x/142 =a b/137 p/143 =a (makeblock 0 a/136 b/137))
        (exit 10 x/142 p/143)))
   with (10 x/138[int] p/139) (makeblock 0 (int,*) x/138 p/139)))
(function a/136[int] b/137[int]
  (catch
    (if a/136 (exit 10 a/136 (makeblock 0 a/136 b/137))
      (exit 10 b/137 (makeblock 0 a/136 b/137)))
   with (10 x/138[int] p/139) (makeblock 0 (int,*) x/138 p/139)))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

(* here flattening is an optimisation: the allocation is moved as an
   alias within each branch, and in the first branch it is unused and
   will be removed by simplification, so the final code
   (see the -dlambda output) will not allocate in the first branch. *)
let _ = fun a b -> match a, b with
| (true as x, _) as _p -> x, (true, true)
| (false as x, _) as p -> x, p
(* outside, onecase *)
[%%expect {|
(function a/146[int] b/147[int]
  (if a/146
    (let (x/148 =a[int] a/146 _p/149 =a (makeblock 0 a/146 b/147))
      (makeblock 0 (int,*) x/148 [0: 1 1]))
    (let (x/150 =a[int] a/146 p/151 =a (makeblock 0 a/146 b/147))
      (makeblock 0 (int,*) x/150 p/151))))
(function a/146[int] b/147[int]
  (if a/146 (makeblock 0 (int,*) a/146 [0: 1 1])
    (makeblock 0 (int,*) a/146 (makeblock 0 a/146 b/147))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/152[int] b/153
  (let (x/154 =a[int] a/152 p/155 =a (makeblock 0 a/152 b/153))
    (makeblock 0 (int,*) x/154 p/155)))
(function a/152[int] b/153
  (makeblock 0 (int,*) a/152 (makeblock 0 a/152 b/153)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/165[int] b/166
  (catch
    (if a/165 (if b/166 (let (p/167 =a (field 0 b/166)) p/167) (exit 12))
      (exit 12))
   with (12) (let (p/168 =a (makeblock 0 a/165 b/166)) p/168)))
(function a/165[int] b/166
  (catch (if a/165 (if b/166 (field 0 b/166) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/165 b/166)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/169[int] b/170
  (catch
    (catch
      (if a/169
        (if b/170 (let (p/174 =a (field 0 b/170)) (exit 13 p/174)) (exit 14))
        (exit 14))
     with (14) (let (p/173 =a (makeblock 0 a/169 b/170)) (exit 13 p/173)))
   with (13 p/171) p/171))
(function a/169[int] b/170
  (catch
    (catch
      (if a/169 (if b/170 (exit 13 (field 0 b/170)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/169 b/170)))
   with (13 p/171) p/171))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
