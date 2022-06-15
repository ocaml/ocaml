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
(let (*match*/294 = 3 *match*/295 = 2 *match*/296 = 1)
  (catch
    (catch
      (catch (if (!= *match*/295 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/294 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/294 = 3 *match*/295 = 2 *match*/296 = 1)
  (catch (if (!= *match*/295 3) (if (!= *match*/294 1) 0 (exit 1)) (exit 1))
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
(let (*match*/299 = 3 *match*/300 = 2 *match*/301 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/300 3) (exit 6)
          (let (x/303 =a (makeblock 0 *match*/299 *match*/300 *match*/301))
            (exit 4 x/303)))
       with (6)
        (if (!= *match*/299 1) (exit 5)
          (let (x/302 =a (makeblock 0 *match*/299 *match*/300 *match*/301))
            (exit 4 x/302))))
     with (5) 0)
   with (4 x/297) (seq (ignore x/297) 1)))
(let (*match*/299 = 3 *match*/300 = 2 *match*/301 = 1)
  (catch
    (if (!= *match*/300 3)
      (if (!= *match*/299 1) 0
        (exit 4 (makeblock 0 *match*/299 *match*/300 *match*/301)))
      (exit 4 (makeblock 0 *match*/299 *match*/300 *match*/301)))
   with (4 x/297) (seq (ignore x/297) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/304[int] b/305 : int 0)
(function a/304[int] b/305 : int 0)
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
(function a/308[int] b/309 (let (p/310 =a (makeblock 0 a/308 b/309)) p/310))
(function a/308[int] b/309 (makeblock 0 a/308 b/309))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/312[int] b/313 (let (p/314 =a (makeblock 0 a/312 b/313)) p/314))
(function a/312[int] b/313 (makeblock 0 a/312 b/313))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/318[int] b/319
  (let (x/320 =a[int] a/318 p/321 =a (makeblock 0 a/318 b/319))
    (makeblock 0 (int,*) x/320 p/321)))
(function a/318[int] b/319
  (makeblock 0 (int,*) a/318 (makeblock 0 a/318 b/319)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/324[int] b/325
  (let (x/326 =a[int] a/324 p/327 =a (makeblock 0 a/324 b/325))
    (makeblock 0 (int,*) x/326 p/327)))
(function a/324[int] b/325
  (makeblock 0 (int,*) a/324 (makeblock 0 a/324 b/325)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/334[int] b/335[int]
  (if a/334
    (let (x/336 =a[int] a/334 p/337 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/336 p/337))
    (let (x/338 =a b/335 p/339 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/338 p/339))))
(function a/334[int] b/335[int]
  (if a/334 (makeblock 0 (int,*) a/334 (makeblock 0 a/334 b/335))
    (makeblock 0 (int,*) b/335 (makeblock 0 a/334 b/335))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/340[int] b/341[int]
  (catch
    (if a/340
      (let (x/348 =a[int] a/340 p/349 =a (makeblock 0 a/340 b/341))
        (exit 10 x/348 p/349))
      (let (x/346 =a b/341 p/347 =a (makeblock 0 a/340 b/341))
        (exit 10 x/346 p/347)))
   with (10 x/342[int] p/343) (makeblock 0 (int,*) x/342 p/343)))
(function a/340[int] b/341[int]
  (catch
    (if a/340 (exit 10 a/340 (makeblock 0 a/340 b/341))
      (exit 10 b/341 (makeblock 0 a/340 b/341)))
   with (10 x/342[int] p/343) (makeblock 0 (int,*) x/342 p/343)))
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
(function a/350[int] b/351[int]
  (if a/350
    (let (x/352 =a[int] a/350 _p/353 =a (makeblock 0 a/350 b/351))
      (makeblock 0 (int,*) x/352 [0: 1 1]))
    (let (x/354 =a[int] a/350 p/355 =a (makeblock 0 a/350 b/351))
      (makeblock 0 (int,*) x/354 p/355))))
(function a/350[int] b/351[int]
  (if a/350 (makeblock 0 (int,*) a/350 [0: 1 1])
    (makeblock 0 (int,*) a/350 (makeblock 0 a/350 b/351))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/356[int] b/357
  (let (x/358 =a[int] a/356 p/359 =a (makeblock 0 a/356 b/357))
    (makeblock 0 (int,*) x/358 p/359)))
(function a/356[int] b/357
  (makeblock 0 (int,*) a/356 (makeblock 0 a/356 b/357)))
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
(function a/369[int] b/370
  (catch
    (if a/369 (if b/370 (let (p/371 =a (field_imm 0 b/370)) p/371) (exit 12))
      (exit 12))
   with (12) (let (p/372 =a (makeblock 0 a/369 b/370)) p/372)))
(function a/369[int] b/370
  (catch (if a/369 (if b/370 (field_imm 0 b/370) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/369 b/370)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/373[int] b/374
  (catch
    (catch
      (if a/373
        (if b/374 (let (p/378 =a (field_imm 0 b/374)) (exit 13 p/378))
          (exit 14))
        (exit 14))
     with (14) (let (p/377 =a (makeblock 0 a/373 b/374)) (exit 13 p/377)))
   with (13 p/375) p/375))
(function a/373[int] b/374
  (catch
    (catch
      (if a/373 (if b/374 (exit 13 (field_imm 0 b/374)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/373 b/374)))
   with (13 p/375) p/375))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
