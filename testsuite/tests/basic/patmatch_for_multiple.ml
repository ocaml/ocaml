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
(let (*match*/274 = 3 *match*/275 = 2 *match*/276 = 1)
  (catch
    (catch
      (catch (if (!= *match*/275 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/274 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/274 = 3 *match*/275 = 2 *match*/276 = 1)
  (catch (if (!= *match*/275 3) (if (!= *match*/274 1) 0 (exit 1)) (exit 1))
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
(let (*match*/279 = 3 *match*/280 = 2 *match*/281 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/280 3) (exit 6)
          (let (x/283 =a (makeblock 0 *match*/279 *match*/280 *match*/281))
            (exit 4 x/283)))
       with (6)
        (if (!= *match*/279 1) (exit 5)
          (let (x/282 =a (makeblock 0 *match*/279 *match*/280 *match*/281))
            (exit 4 x/282))))
     with (5) 0)
   with (4 x/277) (seq (ignore x/277) 1)))
(let (*match*/279 = 3 *match*/280 = 2 *match*/281 = 1)
  (catch
    (if (!= *match*/280 3)
      (if (!= *match*/279 1) 0
        (exit 4 (makeblock 0 *match*/279 *match*/280 *match*/281)))
      (exit 4 (makeblock 0 *match*/279 *match*/280 *match*/281)))
   with (4 x/277) (seq (ignore x/277) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/284[int] b/285 : int 0)
(function a/284[int] b/285 : int 0)
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
(function a/288[int] b/289 (let (p/290 =a (makeblock 0 a/288 b/289)) p/290))
(function a/288[int] b/289 (makeblock 0 a/288 b/289))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/292[int] b/293 (let (p/294 =a (makeblock 0 a/292 b/293)) p/294))
(function a/292[int] b/293 (makeblock 0 a/292 b/293))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/298[int] b/299
  (let (x/300 =a[int] a/298 p/301 =a (makeblock 0 a/298 b/299))
    (makeblock 0 (int,*) x/300 p/301)))
(function a/298[int] b/299
  (makeblock 0 (int,*) a/298 (makeblock 0 a/298 b/299)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/304[int] b/305
  (let (x/306 =a[int] a/304 p/307 =a (makeblock 0 a/304 b/305))
    (makeblock 0 (int,*) x/306 p/307)))
(function a/304[int] b/305
  (makeblock 0 (int,*) a/304 (makeblock 0 a/304 b/305)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/314[int] b/315[int]
  (if a/314
    (let (x/316 =a[int] a/314 p/317 =a (makeblock 0 a/314 b/315))
      (makeblock 0 (int,*) x/316 p/317))
    (let (x/318 =a b/315 p/319 =a (makeblock 0 a/314 b/315))
      (makeblock 0 (int,*) x/318 p/319))))
(function a/314[int] b/315[int]
  (if a/314 (makeblock 0 (int,*) a/314 (makeblock 0 a/314 b/315))
    (makeblock 0 (int,*) b/315 (makeblock 0 a/314 b/315))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/320[int] b/321[int]
  (catch
    (if a/320
      (let (x/328 =a[int] a/320 p/329 =a (makeblock 0 a/320 b/321))
        (exit 10 x/328 p/329))
      (let (x/326 =a b/321 p/327 =a (makeblock 0 a/320 b/321))
        (exit 10 x/326 p/327)))
   with (10 x/322[int] p/323) (makeblock 0 (int,*) x/322 p/323)))
(function a/320[int] b/321[int]
  (catch
    (if a/320 (exit 10 a/320 (makeblock 0 a/320 b/321))
      (exit 10 b/321 (makeblock 0 a/320 b/321)))
   with (10 x/322[int] p/323) (makeblock 0 (int,*) x/322 p/323)))
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
(function a/330[int] b/331[int]
  (if a/330
    (let (x/332 =a[int] a/330 _p/333 =a (makeblock 0 a/330 b/331))
      (makeblock 0 (int,*) x/332 [0: 1 1]))
    (let (x/334 =a[int] a/330 p/335 =a (makeblock 0 a/330 b/331))
      (makeblock 0 (int,*) x/334 p/335))))
(function a/330[int] b/331[int]
  (if a/330 (makeblock 0 (int,*) a/330 [0: 1 1])
    (makeblock 0 (int,*) a/330 (makeblock 0 a/330 b/331))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/336[int] b/337
  (let (x/338 =a[int] a/336 p/339 =a (makeblock 0 a/336 b/337))
    (makeblock 0 (int,*) x/338 p/339)))
(function a/336[int] b/337
  (makeblock 0 (int,*) a/336 (makeblock 0 a/336 b/337)))
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
(function a/349[int] b/350
  (catch
    (if a/349 (if b/350 (let (p/351 =a (field 0 b/350)) p/351) (exit 12))
      (exit 12))
   with (12) (let (p/352 =a (makeblock 0 a/349 b/350)) p/352)))
(function a/349[int] b/350
  (catch (if a/349 (if b/350 (field 0 b/350) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/349 b/350)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/353[int] b/354
  (catch
    (catch
      (if a/353
        (if b/354 (let (p/358 =a (field 0 b/354)) (exit 13 p/358)) (exit 14))
        (exit 14))
     with (14) (let (p/357 =a (makeblock 0 a/353 b/354)) (exit 13 p/357)))
   with (13 p/355) p/355))
(function a/353[int] b/354
  (catch
    (catch
      (if a/353 (if b/354 (exit 13 (field 0 b/354)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/353 b/354)))
   with (13 p/355) p/355))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
