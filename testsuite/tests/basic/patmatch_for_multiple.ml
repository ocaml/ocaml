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
(let (*match*/275 = 3 *match*/276 = 2 *match*/277 = 1)
  (catch
    (catch
      (catch (if (!= *match*/276 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/275 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/275 = 3 *match*/276 = 2 *match*/277 = 1)
  (catch (if (!= *match*/276 3) (if (!= *match*/275 1) 0 (exit 1)) (exit 1))
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
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/281 3) (exit 6)
          (let (x/284 =a (makeblock 0 *match*/280 *match*/281 *match*/282))
            (exit 4 x/284)))
       with (6)
        (if (!= *match*/280 1) (exit 5)
          (let (x/283 =a (makeblock 0 *match*/280 *match*/281 *match*/282))
            (exit 4 x/283))))
     with (5) 0)
   with (4 x/278) (seq (ignore x/278) 1)))
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
  (catch
    (if (!= *match*/281 3)
      (if (!= *match*/280 1) 0
        (exit 4 (makeblock 0 *match*/280 *match*/281 *match*/282)))
      (exit 4 (makeblock 0 *match*/280 *match*/281 *match*/282)))
   with (4 x/278) (seq (ignore x/278) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/285[int] b/286 : int 0)
(function a/285[int] b/286 : int 0)
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
(function a/289[int] b/290 (let (p/291 =a (makeblock 0 a/289 b/290)) p/291))
(function a/289[int] b/290 (makeblock 0 a/289 b/290))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/293[int] b/294 (let (p/295 =a (makeblock 0 a/293 b/294)) p/295))
(function a/293[int] b/294 (makeblock 0 a/293 b/294))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/299[int] b/300
  (let (x/301 =a[int] a/299 p/302 =a (makeblock 0 a/299 b/300))
    (makeblock 0 (int,*) x/301 p/302)))
(function a/299[int] b/300
  (makeblock 0 (int,*) a/299 (makeblock 0 a/299 b/300)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/305[int] b/306
  (let (x/307 =a[int] a/305 p/308 =a (makeblock 0 a/305 b/306))
    (makeblock 0 (int,*) x/307 p/308)))
(function a/305[int] b/306
  (makeblock 0 (int,*) a/305 (makeblock 0 a/305 b/306)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/315[int] b/316[int]
  (if a/315
    (let (x/317 =a[int] a/315 p/318 =a (makeblock 0 a/315 b/316))
      (makeblock 0 (int,*) x/317 p/318))
    (let (x/319 =a b/316 p/320 =a (makeblock 0 a/315 b/316))
      (makeblock 0 (int,*) x/319 p/320))))
(function a/315[int] b/316[int]
  (if a/315 (makeblock 0 (int,*) a/315 (makeblock 0 a/315 b/316))
    (makeblock 0 (int,*) b/316 (makeblock 0 a/315 b/316))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/321[int] b/322[int]
  (catch
    (if a/321
      (let (x/329 =a[int] a/321 p/330 =a (makeblock 0 a/321 b/322))
        (exit 10 x/329 p/330))
      (let (x/327 =a b/322 p/328 =a (makeblock 0 a/321 b/322))
        (exit 10 x/327 p/328)))
   with (10 x/323[int] p/324) (makeblock 0 (int,*) x/323 p/324)))
(function a/321[int] b/322[int]
  (catch
    (if a/321 (exit 10 a/321 (makeblock 0 a/321 b/322))
      (exit 10 b/322 (makeblock 0 a/321 b/322)))
   with (10 x/323[int] p/324) (makeblock 0 (int,*) x/323 p/324)))
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
(function a/331[int] b/332[int]
  (if a/331
    (let (x/333 =a[int] a/331 _p/334 =a (makeblock 0 a/331 b/332))
      (makeblock 0 (int,*) x/333 [0: 1 1]))
    (let (x/335 =a[int] a/331 p/336 =a (makeblock 0 a/331 b/332))
      (makeblock 0 (int,*) x/335 p/336))))
(function a/331[int] b/332[int]
  (if a/331 (makeblock 0 (int,*) a/331 [0: 1 1])
    (makeblock 0 (int,*) a/331 (makeblock 0 a/331 b/332))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/337[int] b/338
  (let (x/339 =a[int] a/337 p/340 =a (makeblock 0 a/337 b/338))
    (makeblock 0 (int,*) x/339 p/340)))
(function a/337[int] b/338
  (makeblock 0 (int,*) a/337 (makeblock 0 a/337 b/338)))
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
(function a/350[int] b/351
  (catch
    (if a/350 (if b/351 (let (p/352 =a (field_imm 0 b/351)) p/352) (exit 12))
      (exit 12))
   with (12) (let (p/353 =a (makeblock 0 a/350 b/351)) p/353)))
(function a/350[int] b/351
  (catch (if a/350 (if b/351 (field_imm 0 b/351) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/350 b/351)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/354[int] b/355
  (catch
    (catch
      (if a/354
        (if b/355 (let (p/359 =a (field_imm 0 b/355)) (exit 13 p/359))
          (exit 14))
        (exit 14))
     with (14) (let (p/358 =a (makeblock 0 a/354 b/355)) (exit 13 p/358)))
   with (13 p/356) p/356))
(function a/354[int] b/355
  (catch
    (catch
      (if a/354 (if b/355 (exit 13 (field_imm 0 b/355)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/354 b/355)))
   with (13 p/356) p/356))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
