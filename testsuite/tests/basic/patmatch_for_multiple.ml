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
(let (*match*/94 = 3 *match*/95 = 2 *match*/96 = 1)
  (catch
    (catch
      (catch (if (!= *match*/95 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/94 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/94 = 3 *match*/95 = 2 *match*/96 = 1)
  (catch (if (!= *match*/95 3) (if (!= *match*/94 1) 0 (exit 1)) (exit 1))
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
(let (*match*/99 = 3 *match*/100 = 2 *match*/101 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/100 3) (exit 6)
          (let (x/103 =a (makeblock 0 *match*/99 *match*/100 *match*/101))
            (exit 4 x/103)))
       with (6)
        (if (!= *match*/99 1) (exit 5)
          (let (x/102 =a (makeblock 0 *match*/99 *match*/100 *match*/101))
            (exit 4 x/102))))
     with (5) 0)
   with (4 x/97) (seq (ignore x/97) 1)))
(let (*match*/99 = 3 *match*/100 = 2 *match*/101 = 1)
  (catch
    (if (!= *match*/100 3)
      (if (!= *match*/99 1) 0
        (exit 4 (makeblock 0 *match*/99 *match*/100 *match*/101)))
      (exit 4 (makeblock 0 *match*/99 *match*/100 *match*/101)))
   with (4 x/97) (seq (ignore x/97) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/104[int] b/105 : int 0)
(function a/104[int] b/105 : int 0)
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
(function a/108[int] b/109 (let (p/110 =a (makeblock 0 a/108 b/109)) p/110))
(function a/108[int] b/109 (makeblock 0 a/108 b/109))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/112[int] b/113 (let (p/114 =a (makeblock 0 a/112 b/113)) p/114))
(function a/112[int] b/113 (makeblock 0 a/112 b/113))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/118[int] b/119
  (let (x/120 =a[int] a/118 p/121 =a (makeblock 0 a/118 b/119))
    (makeblock 0 (int,*) x/120 p/121)))
(function a/118[int] b/119
  (makeblock 0 (int,*) a/118 (makeblock 0 a/118 b/119)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/124[int] b/125
  (let (x/126 =a[int] a/124 p/127 =a (makeblock 0 a/124 b/125))
    (makeblock 0 (int,*) x/126 p/127)))
(function a/124[int] b/125
  (makeblock 0 (int,*) a/124 (makeblock 0 a/124 b/125)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/134[int] b/135[int]
  (if a/134
    (let (x/136 =a[int] a/134 p/137 =a (makeblock 0 a/134 b/135))
      (makeblock 0 (int,*) x/136 p/137))
    (let (x/138 =a b/135 p/139 =a (makeblock 0 a/134 b/135))
      (makeblock 0 (int,*) x/138 p/139))))
(function a/134[int] b/135[int]
  (if a/134 (makeblock 0 (int,*) a/134 (makeblock 0 a/134 b/135))
    (makeblock 0 (int,*) b/135 (makeblock 0 a/134 b/135))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/140[int] b/141[int]
  (catch
    (if a/140
      (let (x/148 =a[int] a/140 p/149 =a (makeblock 0 a/140 b/141))
        (exit 10 x/148 p/149))
      (let (x/146 =a b/141 p/147 =a (makeblock 0 a/140 b/141))
        (exit 10 x/146 p/147)))
   with (10 x/142[int] p/143) (makeblock 0 (int,*) x/142 p/143)))
(function a/140[int] b/141[int]
  (catch
    (if a/140 (exit 10 a/140 (makeblock 0 a/140 b/141))
      (exit 10 b/141 (makeblock 0 a/140 b/141)))
   with (10 x/142[int] p/143) (makeblock 0 (int,*) x/142 p/143)))
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
(function a/150[int] b/151[int]
  (if a/150
    (let (x/152 =a[int] a/150 _p/153 =a (makeblock 0 a/150 b/151))
      (makeblock 0 (int,*) x/152 [0: 1 1]))
    (let (x/154 =a[int] a/150 p/155 =a (makeblock 0 a/150 b/151))
      (makeblock 0 (int,*) x/154 p/155))))
(function a/150[int] b/151[int]
  (if a/150 (makeblock 0 (int,*) a/150 [0: 1 1])
    (makeblock 0 (int,*) a/150 (makeblock 0 a/150 b/151))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/156[int] b/157
  (let (x/158 =a[int] a/156 p/159 =a (makeblock 0 a/156 b/157))
    (makeblock 0 (int,*) x/158 p/159)))
(function a/156[int] b/157
  (makeblock 0 (int,*) a/156 (makeblock 0 a/156 b/157)))
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
(function a/169[int] b/170
  (catch
    (if a/169 (if b/170 (let (p/171 =a (field_imm 0 b/170)) p/171) (exit 12))
      (exit 12))
   with (12) (let (p/172 =a (makeblock 0 a/169 b/170)) p/172)))
(function a/169[int] b/170
  (catch (if a/169 (if b/170 (field_imm 0 b/170) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/169 b/170)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/173[int] b/174
  (catch
    (catch
      (if a/173
        (if b/174 (let (p/178 =a (field_imm 0 b/174)) (exit 13 p/178))
          (exit 14))
        (exit 14))
     with (14) (let (p/177 =a (makeblock 0 a/173 b/174)) (exit 13 p/177)))
   with (13 p/175) p/175))
(function a/173[int] b/174
  (catch
    (catch
      (if a/173 (if b/174 (exit 13 (field_imm 0 b/174)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/173 b/174)))
   with (13 p/175) p/175))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
