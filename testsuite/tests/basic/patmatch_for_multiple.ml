(* TEST
 flags = "-drawlambda -dlambda";
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
<<<<<<< HEAD
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
||||||| parent of 5c123dc50b (Immutable arrays)
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
=======
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
    (catch
<<<<<<< HEAD
      (catch (if (!= *match*/279 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/278 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
  (catch (if (!= *match*/279 3) (if (!= *match*/278 1) 0 (exit 2)) (exit 2))
   with (2) 1))
||||||| parent of 5c123dc50b (Immutable arrays)
      (catch (if (!= *match*/278 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/277 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
  (catch (if (!= *match*/278 3) (if (!= *match*/277 1) 0 (exit 1)) (exit 1))
   with (1) 1))
=======
      (catch (if (!= *match*/281 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/280 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
  (catch (if (!= *match*/281 3) (if (!= *match*/280 1) 0 (exit 1)) (exit 1))
   with (1) 1))
>>>>>>> 5c123dc50b (Immutable arrays)
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
<<<<<<< HEAD
(let (*match*/283 = 3 *match*/284 = 2 *match*/285 = 1)
||||||| parent of 5c123dc50b (Immutable arrays)
(let (*match*/282 = 3 *match*/283 = 2 *match*/284 = 1)
=======
(let (*match*/285 = 3 *match*/286 = 2 *match*/287 = 1)
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
    (catch
      (catch
<<<<<<< HEAD
        (if (!= *match*/284 3) (exit 8)
          (let (x/287 =a (makeblock 0 *match*/283 *match*/284 *match*/285))
            (exit 6 x/287)))
       with (8)
        (if (!= *match*/283 1) (exit 7)
          (let (x/286 =a (makeblock 0 *match*/283 *match*/284 *match*/285))
            (exit 6 x/286))))
     with (7) 0)
   with (6 x/281) (seq (ignore x/281) 1)))
(let (*match*/283 = 3 *match*/284 = 2 *match*/285 = 1)
||||||| parent of 5c123dc50b (Immutable arrays)
        (if (!= *match*/283 3) (exit 6)
          (let (x/286 =a (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/286)))
       with (6)
        (if (!= *match*/282 1) (exit 5)
          (let (x/285 =a (makeblock 0 *match*/282 *match*/283 *match*/284))
            (exit 4 x/285))))
     with (5) 0)
   with (4 x/280) (seq (ignore x/280) 1)))
(let (*match*/282 = 3 *match*/283 = 2 *match*/284 = 1)
=======
        (if (!= *match*/286 3) (exit 6)
          (let (x/289 =a (makeblock 0 *match*/285 *match*/286 *match*/287))
            (exit 4 x/289)))
       with (6)
        (if (!= *match*/285 1) (exit 5)
          (let (x/288 =a (makeblock 0 *match*/285 *match*/286 *match*/287))
            (exit 4 x/288))))
     with (5) 0)
   with (4 x/283) (seq (ignore x/283) 1)))
(let (*match*/285 = 3 *match*/286 = 2 *match*/287 = 1)
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
<<<<<<< HEAD
    (if (!= *match*/284 3)
      (if (!= *match*/283 1) 0
        (exit 6 (makeblock 0 *match*/283 *match*/284 *match*/285)))
      (exit 6 (makeblock 0 *match*/283 *match*/284 *match*/285)))
   with (6 x/281) (seq (ignore x/281) 1)))
||||||| parent of 5c123dc50b (Immutable arrays)
    (if (!= *match*/283 3)
      (if (!= *match*/282 1) 0
        (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
      (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
   with (4 x/280) (seq (ignore x/280) 1)))
=======
    (if (!= *match*/286 3)
      (if (!= *match*/285 1) 0
        (exit 4 (makeblock 0 *match*/285 *match*/286 *match*/287)))
      (exit 4 (makeblock 0 *match*/285 *match*/286 *match*/287)))
   with (4 x/283) (seq (ignore x/283) 1)))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
<<<<<<< HEAD
(function a/288[int] b/289 : int 0)
(function a/288[int] b/289 : int 0)
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/287[int] b/288 : int 0)
(function a/287[int] b/288 : int 0)
=======
(function a/290[int] b/291 : int 0)
(function a/290[int] b/291 : int 0)
>>>>>>> 5c123dc50b (Immutable arrays)
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
<<<<<<< HEAD
(function a/292[int] b/293 (let (p/294 =a (makeblock 0 a/292 b/293)) p/294))
(function a/292[int] b/293 (makeblock 0 a/292 b/293))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/291[int] b/292 (let (p/293 =a (makeblock 0 a/291 b/292)) p/293))
(function a/291[int] b/292 (makeblock 0 a/291 b/292))
=======
(function a/294[int] b/295 (let (p/296 =a (makeblock 0 a/294 b/295)) p/296))
(function a/294[int] b/295 (makeblock 0 a/294 b/295))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
<<<<<<< HEAD
(function a/296[int] b/297 (let (p/298 =a (makeblock 0 a/296 b/297)) p/298))
(function a/296[int] b/297 (makeblock 0 a/296 b/297))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/295[int] b/296 (let (p/297 =a (makeblock 0 a/295 b/296)) p/297))
(function a/295[int] b/296 (makeblock 0 a/295 b/296))
=======
(function a/298[int] b/299 (let (p/300 =a (makeblock 0 a/298 b/299)) p/300))
(function a/298[int] b/299 (makeblock 0 a/298 b/299))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
<<<<<<< HEAD
(function a/302[int] b/303
  (let (x/304 =a[int] a/302 p/305 =a (makeblock 0 a/302 b/303))
    (makeblock 0 (int,*) x/304 p/305)))
(function a/302[int] b/303
  (makeblock 0 (int,*) a/302 (makeblock 0 a/302 b/303)))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/301[int] b/302
  (let (x/303 =a[int] a/301 p/304 =a (makeblock 0 a/301 b/302))
    (makeblock 0 (int,*) x/303 p/304)))
(function a/301[int] b/302
  (makeblock 0 (int,*) a/301 (makeblock 0 a/301 b/302)))
=======
(function a/304[int] b/305
  (let (x/306 =a[int] a/304 p/307 =a (makeblock 0 a/304 b/305))
    (makeblock 0 (int,*) x/306 p/307)))
(function a/304[int] b/305
  (makeblock 0 (int,*) a/304 (makeblock 0 a/304 b/305)))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
<<<<<<< HEAD
(function a/308[int] b/309
  (let (x/310 =a[int] a/308 p/311 =a (makeblock 0 a/308 b/309))
    (makeblock 0 (int,*) x/310 p/311)))
(function a/308[int] b/309
  (makeblock 0 (int,*) a/308 (makeblock 0 a/308 b/309)))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/307[int] b/308
  (let (x/309 =a[int] a/307 p/310 =a (makeblock 0 a/307 b/308))
    (makeblock 0 (int,*) x/309 p/310)))
(function a/307[int] b/308
  (makeblock 0 (int,*) a/307 (makeblock 0 a/307 b/308)))
=======
(function a/310[int] b/311
  (let (x/312 =a[int] a/310 p/313 =a (makeblock 0 a/310 b/311))
    (makeblock 0 (int,*) x/312 p/313)))
(function a/310[int] b/311
  (makeblock 0 (int,*) a/310 (makeblock 0 a/310 b/311)))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
<<<<<<< HEAD
(function a/318[int] b/319[int]
  (if a/318
    (let (x/320 =a[int] a/318 p/321 =a (makeblock 0 a/318 b/319))
      (makeblock 0 (int,*) x/320 p/321))
    (let (x/322 =a b/319 p/323 =a (makeblock 0 a/318 b/319))
      (makeblock 0 (int,*) x/322 p/323))))
(function a/318[int] b/319[int]
  (if a/318 (makeblock 0 (int,*) a/318 (makeblock 0 a/318 b/319))
    (makeblock 0 (int,*) b/319 (makeblock 0 a/318 b/319))))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/317[int] b/318[int]
  (if a/317
    (let (x/319 =a[int] a/317 p/320 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/319 p/320))
    (let (x/321 =a b/318 p/322 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/321 p/322))))
(function a/317[int] b/318[int]
  (if a/317 (makeblock 0 (int,*) a/317 (makeblock 0 a/317 b/318))
    (makeblock 0 (int,*) b/318 (makeblock 0 a/317 b/318))))
=======
(function a/320[int] b/321[int]
  (if a/320
    (let (x/322 =a[int] a/320 p/323 =a (makeblock 0 a/320 b/321))
      (makeblock 0 (int,*) x/322 p/323))
    (let (x/324 =a b/321 p/325 =a (makeblock 0 a/320 b/321))
      (makeblock 0 (int,*) x/324 p/325))))
(function a/320[int] b/321[int]
  (if a/320 (makeblock 0 (int,*) a/320 (makeblock 0 a/320 b/321))
    (makeblock 0 (int,*) b/321 (makeblock 0 a/320 b/321))))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
<<<<<<< HEAD
(function a/324[int] b/325[int]
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/323[int] b/324[int]
=======
(function a/326[int] b/327[int]
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
<<<<<<< HEAD
    (if a/324
      (let (x/332 =a[int] a/324 p/333 =a (makeblock 0 a/324 b/325))
        (exit 31 x/332 p/333))
      (let (x/330 =a b/325 p/331 =a (makeblock 0 a/324 b/325))
        (exit 31 x/330 p/331)))
   with (31 x/326[int] p/327) (makeblock 0 (int,*) x/326 p/327)))
(function a/324[int] b/325[int]
||||||| parent of 5c123dc50b (Immutable arrays)
    (if a/323
      (let (x/331 =a[int] a/323 p/332 =a (makeblock 0 a/323 b/324))
        (exit 10 x/331 p/332))
      (let (x/329 =a b/324 p/330 =a (makeblock 0 a/323 b/324))
        (exit 10 x/329 p/330)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
(function a/323[int] b/324[int]
=======
    (if a/326
      (let (x/334 =a[int] a/326 p/335 =a (makeblock 0 a/326 b/327))
        (exit 10 x/334 p/335))
      (let (x/332 =a b/327 p/333 =a (makeblock 0 a/326 b/327))
        (exit 10 x/332 p/333)))
   with (10 x/328[int] p/329) (makeblock 0 (int,*) x/328 p/329)))
(function a/326[int] b/327[int]
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
<<<<<<< HEAD
    (if a/324 (exit 31 a/324 (makeblock 0 a/324 b/325))
      (exit 31 b/325 (makeblock 0 a/324 b/325)))
   with (31 x/326[int] p/327) (makeblock 0 (int,*) x/326 p/327)))
||||||| parent of 5c123dc50b (Immutable arrays)
    (if a/323 (exit 10 a/323 (makeblock 0 a/323 b/324))
      (exit 10 b/324 (makeblock 0 a/323 b/324)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
=======
    (if a/326 (exit 10 a/326 (makeblock 0 a/326 b/327))
      (exit 10 b/327 (makeblock 0 a/326 b/327)))
   with (10 x/328[int] p/329) (makeblock 0 (int,*) x/328 p/329)))
>>>>>>> 5c123dc50b (Immutable arrays)
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
<<<<<<< HEAD
(function a/334[int] b/335[int]
  (if a/334
    (let (x/336 =a[int] a/334 _p/337 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/336 [0: 1 1]))
    (let (x/338 =a[int] a/334 p/339 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/338 p/339))))
(function a/334[int] b/335[int]
  (if a/334 (makeblock 0 (int,*) a/334 [0: 1 1])
    (makeblock 0 (int,*) a/334 (makeblock 0 a/334 b/335))))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/333[int] b/334[int]
  (if a/333
    (let (x/335 =a[int] a/333 _p/336 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/335 [0: 1 1]))
    (let (x/337 =a[int] a/333 p/338 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/337 p/338))))
(function a/333[int] b/334[int]
  (if a/333 (makeblock 0 (int,*) a/333 [0: 1 1])
    (makeblock 0 (int,*) a/333 (makeblock 0 a/333 b/334))))
=======
(function a/336[int] b/337[int]
  (if a/336
    (let (x/338 =a[int] a/336 _p/339 =a (makeblock 0 a/336 b/337))
      (makeblock 0 (int,*) x/338 [0: 1 1]))
    (let (x/340 =a[int] a/336 p/341 =a (makeblock 0 a/336 b/337))
      (makeblock 0 (int,*) x/340 p/341))))
(function a/336[int] b/337[int]
  (if a/336 (makeblock 0 (int,*) a/336 [0: 1 1])
    (makeblock 0 (int,*) a/336 (makeblock 0 a/336 b/337))))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
<<<<<<< HEAD
(function a/340[int] b/341
  (let (x/342 =a[int] a/340 p/343 =a (makeblock 0 a/340 b/341))
    (makeblock 0 (int,*) x/342 p/343)))
(function a/340[int] b/341
  (makeblock 0 (int,*) a/340 (makeblock 0 a/340 b/341)))
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/339[int] b/340
  (let (x/341 =a[int] a/339 p/342 =a (makeblock 0 a/339 b/340))
    (makeblock 0 (int,*) x/341 p/342)))
(function a/339[int] b/340
  (makeblock 0 (int,*) a/339 (makeblock 0 a/339 b/340)))
=======
(function a/342[int] b/343
  (let (x/344 =a[int] a/342 p/345 =a (makeblock 0 a/342 b/343))
    (makeblock 0 (int,*) x/344 p/345)))
(function a/342[int] b/343
  (makeblock 0 (int,*) a/342 (makeblock 0 a/342 b/343)))
>>>>>>> 5c123dc50b (Immutable arrays)
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
<<<<<<< HEAD
(function a/353[int] b/354
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/352[int] b/353
=======
(function a/355[int] b/356
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
<<<<<<< HEAD
    (if a/353 (if b/354 (let (p/355 =a (field_imm 0 b/354)) p/355) (exit 42))
      (exit 42))
   with (42) (let (p/356 =a (makeblock 0 a/353 b/354)) p/356)))
(function a/353[int] b/354
  (catch (if a/353 (if b/354 (field_imm 0 b/354) (exit 42)) (exit 42))
   with (42) (makeblock 0 a/353 b/354)))
||||||| parent of 5c123dc50b (Immutable arrays)
    (if a/352 (if b/353 (let (p/354 =a (field_imm 0 b/353)) p/354) (exit 12))
      (exit 12))
   with (12) (let (p/355 =a (makeblock 0 a/352 b/353)) p/355)))
(function a/352[int] b/353
  (catch (if a/352 (if b/353 (field_imm 0 b/353) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/352 b/353)))
=======
    (if a/355 (if b/356 (let (p/357 =a (field_imm 0 b/356)) p/357) (exit 12))
      (exit 12))
   with (12) (let (p/358 =a (makeblock 0 a/355 b/356)) p/358)))
(function a/355[int] b/356
  (catch (if a/355 (if b/356 (field_imm 0 b/356) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/355 b/356)))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
<<<<<<< HEAD
(function a/357[int] b/358
||||||| parent of 5c123dc50b (Immutable arrays)
(function a/356[int] b/357
=======
(function a/359[int] b/360
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
    (catch
<<<<<<< HEAD
      (if a/357
        (if b/358 (let (p/362 =a (field_imm 0 b/358)) (exit 46 p/362))
          (exit 47))
        (exit 47))
     with (47) (let (p/361 =a (makeblock 0 a/357 b/358)) (exit 46 p/361)))
   with (46 p/359) p/359))
(function a/357[int] b/358
||||||| parent of 5c123dc50b (Immutable arrays)
      (if a/356
        (if b/357 (let (p/361 =a (field_imm 0 b/357)) (exit 13 p/361))
          (exit 14))
        (exit 14))
     with (14) (let (p/360 =a (makeblock 0 a/356 b/357)) (exit 13 p/360)))
   with (13 p/358) p/358))
(function a/356[int] b/357
=======
      (if a/359
        (if b/360 (let (p/364 =a (field_imm 0 b/360)) (exit 13 p/364))
          (exit 14))
        (exit 14))
     with (14) (let (p/363 =a (makeblock 0 a/359 b/360)) (exit 13 p/363)))
   with (13 p/361) p/361))
(function a/359[int] b/360
>>>>>>> 5c123dc50b (Immutable arrays)
  (catch
    (catch
<<<<<<< HEAD
      (if a/357 (if b/358 (exit 46 (field_imm 0 b/358)) (exit 47)) (exit 47))
     with (47) (exit 46 (makeblock 0 a/357 b/358)))
   with (46 p/359) p/359))
||||||| parent of 5c123dc50b (Immutable arrays)
      (if a/356 (if b/357 (exit 13 (field_imm 0 b/357)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/356 b/357)))
   with (13 p/358) p/358))
=======
      (if a/359 (if b/360 (exit 13 (field_imm 0 b/360)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/359 b/360)))
   with (13 p/361) p/361))
>>>>>>> 5c123dc50b (Immutable arrays)
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
