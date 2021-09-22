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
(let (*match*/273 = 3 *match*/274 = 2 *match*/275 = 1)
  (catch
    (catch
      (catch (if (!= *match*/274 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/273 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/273 = 3 *match*/274 = 2 *match*/275 = 1)
  (catch (if (!= *match*/274 3) (if (!= *match*/273 1) 0 (exit 1)) (exit 1))
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
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/279 3) (exit 6)
          (let (x/282 =a (makeblock 0 *match*/278 *match*/279 *match*/280))
            (exit 4 x/282)))
       with (6)
        (if (!= *match*/278 1) (exit 5)
          (let (x/281 =a (makeblock 0 *match*/278 *match*/279 *match*/280))
            (exit 4 x/281))))
     with (5) 0)
   with (4 x/276) (seq (ignore x/276) 1)))
(let (*match*/278 = 3 *match*/279 = 2 *match*/280 = 1)
  (catch
    (if (!= *match*/279 3)
      (if (!= *match*/278 1) 0
        (exit 4 (makeblock 0 *match*/278 *match*/279 *match*/280)))
      (exit 4 (makeblock 0 *match*/278 *match*/279 *match*/280)))
   with (4 x/276) (seq (ignore x/276) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/283[int] b/284 : int 0)
(function a/283[int] b/284 : int 0)
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
(function a/287[int] b/288 (let (p/289 =a (makeblock 0 a/287 b/288)) p/289))
(function a/287[int] b/288 (makeblock 0 a/287 b/288))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/291[int] b/292 (let (p/293 =a (makeblock 0 a/291 b/292)) p/293))
(function a/291[int] b/292 (makeblock 0 a/291 b/292))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/297[int] b/298
  (let (x/299 =a[int] a/297 p/300 =a (makeblock 0 a/297 b/298))
    (makeblock 0 (int,*) x/299 p/300)))
(function a/297[int] b/298
  (makeblock 0 (int,*) a/297 (makeblock 0 a/297 b/298)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/303[int] b/304
  (let (x/305 =a[int] a/303 p/306 =a (makeblock 0 a/303 b/304))
    (makeblock 0 (int,*) x/305 p/306)))
(function a/303[int] b/304
  (makeblock 0 (int,*) a/303 (makeblock 0 a/303 b/304)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/313[int] b/314[int]
  (if a/313
    (let (x/315 =a[int] a/313 p/316 =a (makeblock 0 a/313 b/314))
      (makeblock 0 (int,*) x/315 p/316))
    (let (x/317 =a b/314 p/318 =a (makeblock 0 a/313 b/314))
      (makeblock 0 (int,*) x/317 p/318))))
(function a/313[int] b/314[int]
  (if a/313 (makeblock 0 (int,*) a/313 (makeblock 0 a/313 b/314))
    (makeblock 0 (int,*) b/314 (makeblock 0 a/313 b/314))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/319[int] b/320[int]
  (catch
    (if a/319
      (let (x/327 =a[int] a/319 p/328 =a (makeblock 0 a/319 b/320))
        (exit 10 x/327 p/328))
      (let (x/325 =a b/320 p/326 =a (makeblock 0 a/319 b/320))
        (exit 10 x/325 p/326)))
   with (10 x/321[int] p/322) (makeblock 0 (int,*) x/321 p/322)))
(function a/319[int] b/320[int]
  (catch
    (if a/319 (exit 10 a/319 (makeblock 0 a/319 b/320))
      (exit 10 b/320 (makeblock 0 a/319 b/320)))
   with (10 x/321[int] p/322) (makeblock 0 (int,*) x/321 p/322)))
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
(function a/329[int] b/330[int]
  (if a/329
    (let (x/331 =a[int] a/329 _p/332 =a (makeblock 0 a/329 b/330))
      (makeblock 0 (int,*) x/331 [0: 1 1]))
    (let (x/333 =a[int] a/329 p/334 =a (makeblock 0 a/329 b/330))
      (makeblock 0 (int,*) x/333 p/334))))
(function a/329[int] b/330[int]
  (if a/329 (makeblock 0 (int,*) a/329 [0: 1 1])
    (makeblock 0 (int,*) a/329 (makeblock 0 a/329 b/330))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/335[int] b/336
  (let (x/337 =a[int] a/335 p/338 =a (makeblock 0 a/335 b/336))
    (makeblock 0 (int,*) x/337 p/338)))
(function a/335[int] b/336
  (makeblock 0 (int,*) a/335 (makeblock 0 a/335 b/336)))
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
(function a/348[int] b/349
  (catch
    (if a/348 (if b/349 (let (p/350 =a (field 0 b/349)) p/350) (exit 12))
      (exit 12))
   with (12) (let (p/351 =a (makeblock 0 a/348 b/349)) p/351)))
(function a/348[int] b/349
  (catch (if a/348 (if b/349 (field 0 b/349) (exit 12)) (exit 12)) with (12)
    (makeblock 0 a/348 b/349)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/352[int] b/353
  (catch
    (catch
      (if a/352
        (if b/353 (let (p/357 =a (field 0 b/353)) (exit 13 p/357)) (exit 14))
        (exit 14))
     with (14) (let (p/356 =a (makeblock 0 a/352 b/353)) (exit 13 p/356)))
   with (13 p/354) p/354))
(function a/352[int] b/353
  (catch
    (catch
      (if a/352 (if b/353 (exit 13 (field 0 b/353)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/352 b/353)))
   with (13 p/354) p/354))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
