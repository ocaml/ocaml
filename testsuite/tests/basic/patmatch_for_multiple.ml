(* TEST
 flags = "-drawlambda -dlambda -dcanonical-ids";
 expect;
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
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch
    (catch
      (catch (if (!= *match*/1 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/0 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/0 = 3 *match*/1 = 2 *match*/2 = 1)
  (catch (if (!= *match*/1 3) (if (!= *match*/0 1) 0 (exit 2)) (exit 2))
   with (2) 1))
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
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/4 3) (exit 8)
          (let (x/0 =a (makeblock 0 *match*/3 *match*/4 *match*/5))
            (exit 6 x/0)))
       with (8)
        (if (!= *match*/3 1) (exit 7)
          (let (x/1 =a (makeblock 0 *match*/3 *match*/4 *match*/5))
            (exit 6 x/1))))
     with (7) 0)
   with (6 x/2) (seq (ignore x/2) 1)))
(let (*match*/3 = 3 *match*/4 = 2 *match*/5 = 1)
  (catch
    (if (!= *match*/4 3)
      (if (!= *match*/3 1) 0
        (exit 6 (makeblock 0 *match*/3 *match*/4 *match*/5)))
      (exit 6 (makeblock 0 *match*/3 *match*/4 *match*/5)))
   with (6 x/2) (seq (ignore x/2) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/0[int] b/0 : int 0)
(function a/0[int] b/0 : int 0)
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
(function a/1[int] b/1 (let (p/0 =a (makeblock 0 a/1 b/1)) p/0))
(function a/1[int] b/1 (makeblock 0 a/1 b/1))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/2[int] b/2 (let (p/1 =a (makeblock 0 a/2 b/2)) p/1))
(function a/2[int] b/2 (makeblock 0 a/2 b/2))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/3[int] b/3
  (let (x/3 =a[int] a/3 p/2 =a (makeblock 0 a/3 b/3))
    (makeblock 0 (int,*) x/3 p/2)))
(function a/3[int] b/3 (makeblock 0 (int,*) a/3 (makeblock 0 a/3 b/3)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/4[int] b/4
  (let (x/4 =a[int] a/4 p/3 =a (makeblock 0 a/4 b/4))
    (makeblock 0 (int,*) x/4 p/3)))
(function a/4[int] b/4 (makeblock 0 (int,*) a/4 (makeblock 0 a/4 b/4)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/5[int] b/5[int]
  (if a/5
    (let (x/5 =a[int] a/5 p/4 =a (makeblock 0 a/5 b/5))
      (makeblock 0 (int,*) x/5 p/4))
    (let (x/6 =a b/5 p/5 =a (makeblock 0 a/5 b/5))
      (makeblock 0 (int,*) x/6 p/5))))
(function a/5[int] b/5[int]
  (if a/5 (makeblock 0 (int,*) a/5 (makeblock 0 a/5 b/5))
    (makeblock 0 (int,*) b/5 (makeblock 0 a/5 b/5))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/6[int] b/6[int]
  (catch
    (if a/6
      (let (x/7 =a[int] a/6 p/6 =a (makeblock 0 a/6 b/6)) (exit 31 x/7 p/6))
      (let (x/8 =a b/6 p/7 =a (makeblock 0 a/6 b/6)) (exit 31 x/8 p/7)))
   with (31 x/9[int] p/8) (makeblock 0 (int,*) x/9 p/8)))
(function a/6[int] b/6[int]
  (catch
    (if a/6 (exit 31 a/6 (makeblock 0 a/6 b/6))
      (exit 31 b/6 (makeblock 0 a/6 b/6)))
   with (31 x/9[int] p/8) (makeblock 0 (int,*) x/9 p/8)))
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
(function a/7[int] b/7[int]
  (if a/7
    (let (x/10 =a[int] a/7 _p/0 =a (makeblock 0 a/7 b/7))
      (makeblock 0 (int,*) x/10 [0: 1 1]))
    (let (x/11 =a[int] a/7 p/9 =a (makeblock 0 a/7 b/7))
      (makeblock 0 (int,*) x/11 p/9))))
(function a/7[int] b/7[int]
  (if a/7 (makeblock 0 (int,*) a/7 [0: 1 1])
    (makeblock 0 (int,*) a/7 (makeblock 0 a/7 b/7))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/8[int] b/8
  (let (x/12 =a[int] a/8 p/10 =a (makeblock 0 a/8 b/8))
    (makeblock 0 (int,*) x/12 p/10)))
(function a/8[int] b/8 (makeblock 0 (int,*) a/8 (makeblock 0 a/8 b/8)))
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
(function a/9[int] b/9
  (catch
    (if a/9 (if b/9 (let (p/11 =a (field_imm 0 b/9)) p/11) (exit 42))
      (exit 42))
   with (42) (let (p/12 =a (makeblock 0 a/9 b/9)) p/12)))
(function a/9[int] b/9
  (catch (if a/9 (if b/9 (field_imm 0 b/9) (exit 42)) (exit 42)) with (42)
    (makeblock 0 a/9 b/9)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/10[int] b/10
  (catch
    (catch
      (if a/10
        (if b/10 (let (p/13 =a (field_imm 0 b/10)) (exit 46 p/13)) (exit 47))
        (exit 47))
     with (47) (let (p/14 =a (makeblock 0 a/10 b/10)) (exit 46 p/14)))
   with (46 p/15) p/15))
(function a/10[int] b/10
  (catch
    (catch
      (if a/10 (if b/10 (exit 46 (field_imm 0 b/10)) (exit 47)) (exit 47))
     with (47) (exit 46 (makeblock 0 a/10 b/10)))
   with (46 p/15) p/15))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
