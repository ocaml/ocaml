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
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
  (catch
    (catch
      (catch (if (!= *match*/278 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/277 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/277 = 3 *match*/278 = 2 *match*/279 = 1)
  (catch (if (!= *match*/278 3) (if (!= *match*/277 1) 0 (exit 1)) (exit 1))
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
(let (*match*/282 = 3 *match*/283 = 2 *match*/284 = 1)
  (catch
    (catch
      (catch
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
  (catch
    (if (!= *match*/283 3)
      (if (!= *match*/282 1) 0
        (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
      (exit 4 (makeblock 0 *match*/282 *match*/283 *match*/284)))
   with (4 x/280) (seq (ignore x/280) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/287[int] b/288 : int 0)
(function a/287[int] b/288 : int 0)
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
(function a/291[int] b/292 (let (p/293 =a (makeblock 0 a/291 b/292)) p/293))
(function a/291[int] b/292 (makeblock 0 a/291 b/292))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/295[int] b/296 (let (p/297 =a (makeblock 0 a/295 b/296)) p/297))
(function a/295[int] b/296 (makeblock 0 a/295 b/296))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/301[int] b/302
  (let (x/303 =a[int] a/301 p/304 =a (makeblock 0 a/301 b/302))
    (makeblock 0 (int,*) x/303 p/304)))
(function a/301[int] b/302
  (makeblock 0 (int,*) a/301 (makeblock 0 a/301 b/302)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/307[int] b/308
  (let (x/309 =a[int] a/307 p/310 =a (makeblock 0 a/307 b/308))
    (makeblock 0 (int,*) x/309 p/310)))
(function a/307[int] b/308
  (makeblock 0 (int,*) a/307 (makeblock 0 a/307 b/308)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/317[int] b/318[int]
  (if a/317
    (let (x/319 =a[int] a/317 p/320 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/319 p/320))
    (let (x/321 =a b/318 p/322 =a (makeblock 0 a/317 b/318))
      (makeblock 0 (int,*) x/321 p/322))))
(function a/317[int] b/318[int]
  (if a/317 (makeblock 0 (int,*) a/317 (makeblock 0 a/317 b/318))
    (makeblock 0 (int,*) b/318 (makeblock 0 a/317 b/318))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/323[int] b/324[int]
  (catch
    (if a/323
      (let (x/331 =a[int] a/323 p/332 =a (makeblock 0 a/323 b/324))
        (exit 10 x/331 p/332))
      (let (x/329 =a b/324 p/330 =a (makeblock 0 a/323 b/324))
        (exit 10 x/329 p/330)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
(function a/323[int] b/324[int]
  (catch
    (if a/323 (exit 10 a/323 (makeblock 0 a/323 b/324))
      (exit 10 b/324 (makeblock 0 a/323 b/324)))
   with (10 x/325[int] p/326) (makeblock 0 (int,*) x/325 p/326)))
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
(function a/333[int] b/334[int]
  (if a/333
    (let (x/335 =a[int] a/333 _p/336 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/335 [0: 1 1]))
    (let (x/337 =a[int] a/333 p/338 =a (makeblock 0 a/333 b/334))
      (makeblock 0 (int,*) x/337 p/338))))
(function a/333[int] b/334[int]
  (if a/333 (makeblock 0 (int,*) a/333 [0: 1 1])
    (makeblock 0 (int,*) a/333 (makeblock 0 a/333 b/334))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/339[int] b/340
  (let (x/341 =a[int] a/339 p/342 =a (makeblock 0 a/339 b/340))
    (makeblock 0 (int,*) x/341 p/342)))
(function a/339[int] b/340
  (makeblock 0 (int,*) a/339 (makeblock 0 a/339 b/340)))
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
(function a/352[int] b/353
  (catch
    (if a/352 (if b/353 (let (p/354 =a (field_imm 0 b/353)) p/354) (exit 12))
      (exit 12))
   with (12) (let (p/355 =a (makeblock 0 a/352 b/353)) p/355)))
(function a/352[int] b/353
  (catch (if a/352 (if b/353 (field_imm 0 b/353) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/352 b/353)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/356[int] b/357
  (catch
    (catch
      (if a/356
        (if b/357 (let (p/361 =a (field_imm 0 b/357)) (exit 13 p/361))
          (exit 14))
        (exit 14))
     with (14) (let (p/360 =a (makeblock 0 a/356 b/357)) (exit 13 p/360)))
   with (13 p/358) p/358))
(function a/356[int] b/357
  (catch
    (catch
      (if a/356 (if b/357 (exit 13 (field_imm 0 b/357)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/356 b/357)))
   with (13 p/358) p/358))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
