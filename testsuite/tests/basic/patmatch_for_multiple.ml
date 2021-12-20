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
(let (*match*/279 = 3 *match*/280 = 2 *match*/281 = 1)
  (catch
    (catch
      (catch (if (!= *match*/280 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/279 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/279 = 3 *match*/280 = 2 *match*/281 = 1)
  (catch (if (!= *match*/280 3) (if (!= *match*/279 1) 0 (exit 1)) (exit 1))
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
(let (*match*/284 = 3 *match*/285 = 2 *match*/286 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/285 3) (exit 6)
          (let (x/288 =a (makeblock 0 *match*/284 *match*/285 *match*/286))
            (exit 4 x/288)))
       with (6)
        (if (!= *match*/284 1) (exit 5)
          (let (x/287 =a (makeblock 0 *match*/284 *match*/285 *match*/286))
            (exit 4 x/287))))
     with (5) 0)
   with (4 x/282) (seq (ignore x/282) 1)))
(let (*match*/284 = 3 *match*/285 = 2 *match*/286 = 1)
  (catch
    (if (!= *match*/285 3)
      (if (!= *match*/284 1) 0
        (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
      (exit 4 (makeblock 0 *match*/284 *match*/285 *match*/286)))
   with (4 x/282) (seq (ignore x/282) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/289[int] b/290 : int 0)
(function a/289[int] b/290 : int 0)
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
(function a/293[int] b/294 (let (p/295 =a (makeblock 0 a/293 b/294)) p/295))
(function a/293[int] b/294 (makeblock 0 a/293 b/294))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/297[int] b/298 (let (p/299 =a (makeblock 0 a/297 b/298)) p/299))
(function a/297[int] b/298 (makeblock 0 a/297 b/298))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/303[int] b/304
  (let (x/305 =a[int] a/303 p/306 =a (makeblock 0 a/303 b/304))
    (makeblock 0 (int,*) x/305 p/306)))
(function a/303[int] b/304
  (makeblock 0 (int,*) a/303 (makeblock 0 a/303 b/304)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/309[int] b/310
  (let (x/311 =a[int] a/309 p/312 =a (makeblock 0 a/309 b/310))
    (makeblock 0 (int,*) x/311 p/312)))
(function a/309[int] b/310
  (makeblock 0 (int,*) a/309 (makeblock 0 a/309 b/310)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/319[int] b/320[int]
  (if a/319
    (let (x/321 =a[int] a/319 p/322 =a (makeblock 0 a/319 b/320))
      (makeblock 0 (int,*) x/321 p/322))
    (let (x/323 =a b/320 p/324 =a (makeblock 0 a/319 b/320))
      (makeblock 0 (int,*) x/323 p/324))))
(function a/319[int] b/320[int]
  (if a/319 (makeblock 0 (int,*) a/319 (makeblock 0 a/319 b/320))
    (makeblock 0 (int,*) b/320 (makeblock 0 a/319 b/320))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/325[int] b/326[int]
  (catch
    (if a/325
      (let (x/333 =a[int] a/325 p/334 =a (makeblock 0 a/325 b/326))
        (exit 10 x/333 p/334))
      (let (x/331 =a b/326 p/332 =a (makeblock 0 a/325 b/326))
        (exit 10 x/331 p/332)))
   with (10 x/327[int] p/328) (makeblock 0 (int,*) x/327 p/328)))
(function a/325[int] b/326[int]
  (catch
    (if a/325 (exit 10 a/325 (makeblock 0 a/325 b/326))
      (exit 10 b/326 (makeblock 0 a/325 b/326)))
   with (10 x/327[int] p/328) (makeblock 0 (int,*) x/327 p/328)))
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
(function a/335[int] b/336[int]
  (if a/335
    (let (x/337 =a[int] a/335 _p/338 =a (makeblock 0 a/335 b/336))
      (makeblock 0 (int,*) x/337 [0: 1 1]))
    (let (x/339 =a[int] a/335 p/340 =a (makeblock 0 a/335 b/336))
      (makeblock 0 (int,*) x/339 p/340))))
(function a/335[int] b/336[int]
  (if a/335 (makeblock 0 (int,*) a/335 [0: 1 1])
    (makeblock 0 (int,*) a/335 (makeblock 0 a/335 b/336))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/341[int] b/342
  (let (x/343 =a[int] a/341 p/344 =a (makeblock 0 a/341 b/342))
    (makeblock 0 (int,*) x/343 p/344)))
(function a/341[int] b/342
  (makeblock 0 (int,*) a/341 (makeblock 0 a/341 b/342)))
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
(function a/354[int] b/355
  (catch
    (if a/354 (if b/355 (let (p/356 =a (field_imm 0 b/355)) p/356) (exit 12))
      (exit 12))
   with (12) (let (p/357 =a (makeblock 0 a/354 b/355)) p/357)))
(function a/354[int] b/355
  (catch (if a/354 (if b/355 (field_imm 0 b/355) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/354 b/355)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/358[int] b/359
  (catch
    (catch
      (if a/358
        (if b/359 (let (p/363 =a (field_imm 0 b/359)) (exit 13 p/363))
          (exit 14))
        (exit 14))
     with (14) (let (p/362 =a (makeblock 0 a/358 b/359)) (exit 13 p/362)))
   with (13 p/360) p/360))
(function a/358[int] b/359
  (catch
    (catch
      (if a/358 (if b/359 (exit 13 (field_imm 0 b/359)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/358 b/359)))
   with (13 p/360) p/360))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
