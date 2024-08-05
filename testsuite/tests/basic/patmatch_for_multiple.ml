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
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
  (catch
    (catch
      (catch (if (!= *match*/279 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/278 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
  (catch (if (!= *match*/279 3) (if (!= *match*/278 1) 0 (exit 2)) (exit 2))
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
(let (*match*/283 = 3 *match*/284 = 2 *match*/285 = 1)
  (catch
    (catch
      (catch
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
  (catch
    (if (!= *match*/284 3)
      (if (!= *match*/283 1) 0
        (exit 6 (makeblock 0 *match*/283 *match*/284 *match*/285)))
      (exit 6 (makeblock 0 *match*/283 *match*/284 *match*/285)))
   with (6 x/281) (seq (ignore x/281) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/288[int] b/289 : int 0)
(function a/288[int] b/289 : int 0)
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
(function a/292[int] b/293 (let (p/294 =a (makeblock 0 a/292 b/293)) p/294))
(function a/292[int] b/293 (makeblock 0 a/292 b/293))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/296[int] b/297 (let (p/298 =a (makeblock 0 a/296 b/297)) p/298))
(function a/296[int] b/297 (makeblock 0 a/296 b/297))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/302[int] b/303
  (let (x/304 =a[int] a/302 p/305 =a (makeblock 0 a/302 b/303))
    (makeblock 0 (int,*) x/304 p/305)))
(function a/302[int] b/303
  (makeblock 0 (int,*) a/302 (makeblock 0 a/302 b/303)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/308[int] b/309
  (let (x/310 =a[int] a/308 p/311 =a (makeblock 0 a/308 b/309))
    (makeblock 0 (int,*) x/310 p/311)))
(function a/308[int] b/309
  (makeblock 0 (int,*) a/308 (makeblock 0 a/308 b/309)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/318[int] b/319[int]
  (if a/318
    (let (x/320 =a[int] a/318 p/321 =a (makeblock 0 a/318 b/319))
      (makeblock 0 (int,*) x/320 p/321))
    (let (x/322 =a b/319 p/323 =a (makeblock 0 a/318 b/319))
      (makeblock 0 (int,*) x/322 p/323))))
(function a/318[int] b/319[int]
  (if a/318 (makeblock 0 (int,*) a/318 (makeblock 0 a/318 b/319))
    (makeblock 0 (int,*) b/319 (makeblock 0 a/318 b/319))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/324[int] b/325[int]
  (catch
    (if a/324
      (let (x/332 =a[int] a/324 p/333 =a (makeblock 0 a/324 b/325))
        (exit 31 x/332 p/333))
      (let (x/330 =a b/325 p/331 =a (makeblock 0 a/324 b/325))
        (exit 31 x/330 p/331)))
   with (31 x/326[int] p/327) (makeblock 0 (int,*) x/326 p/327)))
(function a/324[int] b/325[int]
  (catch
    (if a/324 (exit 31 a/324 (makeblock 0 a/324 b/325))
      (exit 31 b/325 (makeblock 0 a/324 b/325)))
   with (31 x/326[int] p/327) (makeblock 0 (int,*) x/326 p/327)))
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
(function a/334[int] b/335[int]
  (if a/334
    (let (x/336 =a[int] a/334 _p/337 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/336 [0: 1 1]))
    (let (x/338 =a[int] a/334 p/339 =a (makeblock 0 a/334 b/335))
      (makeblock 0 (int,*) x/338 p/339))))
(function a/334[int] b/335[int]
  (if a/334 (makeblock 0 (int,*) a/334 [0: 1 1])
    (makeblock 0 (int,*) a/334 (makeblock 0 a/334 b/335))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/340[int] b/341
  (let (x/342 =a[int] a/340 p/343 =a (makeblock 0 a/340 b/341))
    (makeblock 0 (int,*) x/342 p/343)))
(function a/340[int] b/341
  (makeblock 0 (int,*) a/340 (makeblock 0 a/340 b/341)))
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
(function a/353[int] b/354
  (catch
    (if a/353 (if b/354 (let (p/355 =a (field_imm 0 b/354)) p/355) (exit 42))
      (exit 42))
   with (42) (let (p/356 =a (makeblock 0 a/353 b/354)) p/356)))
(function a/353[int] b/354
  (catch (if a/353 (if b/354 (field_imm 0 b/354) (exit 42)) (exit 42))
   with (42) (makeblock 0 a/353 b/354)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/357[int] b/358
  (catch
    (catch
      (if a/357
        (if b/358 (let (p/362 =a (field_imm 0 b/358)) (exit 46 p/362))
          (exit 47))
        (exit 47))
     with (47) (let (p/361 =a (makeblock 0 a/357 b/358)) (exit 46 p/361)))
   with (46 p/359) p/359))
(function a/357[int] b/358
  (catch
    (catch
      (if a/357 (if b/358 (exit 46 (field_imm 0 b/358)) (exit 47)) (exit 47))
     with (47) (exit 46 (makeblock 0 a/357 b/358)))
   with (46 p/359) p/359))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
