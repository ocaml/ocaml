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
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
  (catch
    (catch
      (catch (if (!= *match*/281 3) (exit 4) (exit 2)) with (4)
        (if (!= *match*/280 1) (exit 3) (exit 2)))
     with (3) 0)
   with (2) 1))
(let (*match*/280 = 3 *match*/281 = 2 *match*/282 = 1)
  (catch (if (!= *match*/281 3) (if (!= *match*/280 1) 0 (exit 2)) (exit 2))
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
(let (*match*/285 = 3 *match*/286 = 2 *match*/287 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/286 3) (exit 8)
          (let (x/289 =a (makeblock 0 *match*/285 *match*/286 *match*/287))
            (exit 6 x/289)))
       with (8)
        (if (!= *match*/285 1) (exit 7)
          (let (x/288 =a (makeblock 0 *match*/285 *match*/286 *match*/287))
            (exit 6 x/288))))
     with (7) 0)
   with (6 x/283) (seq (ignore x/283) 1)))
(let (*match*/285 = 3 *match*/286 = 2 *match*/287 = 1)
  (catch
    (if (!= *match*/286 3)
      (if (!= *match*/285 1) 0
        (exit 6 (makeblock 0 *match*/285 *match*/286 *match*/287)))
      (exit 6 (makeblock 0 *match*/285 *match*/286 *match*/287)))
   with (6 x/283) (seq (ignore x/283) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/290[int] b/291 : int 0)
(function a/290[int] b/291 : int 0)
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
(function a/294[int] b/295 (let (p/296 =a (makeblock 0 a/294 b/295)) p/296))
(function a/294[int] b/295 (makeblock 0 a/294 b/295))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/298[int] b/299 (let (p/300 =a (makeblock 0 a/298 b/299)) p/300))
(function a/298[int] b/299 (makeblock 0 a/298 b/299))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/304[int] b/305
  (let (x/306 =a[int] a/304 p/307 =a (makeblock 0 a/304 b/305))
    (makeblock 0 (int,*) x/306 p/307)))
(function a/304[int] b/305
  (makeblock 0 (int,*) a/304 (makeblock 0 a/304 b/305)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/310[int] b/311
  (let (x/312 =a[int] a/310 p/313 =a (makeblock 0 a/310 b/311))
    (makeblock 0 (int,*) x/312 p/313)))
(function a/310[int] b/311
  (makeblock 0 (int,*) a/310 (makeblock 0 a/310 b/311)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/320[int] b/321[int]
  (if a/320
    (let (x/322 =a[int] a/320 p/323 =a (makeblock 0 a/320 b/321))
      (makeblock 0 (int,*) x/322 p/323))
    (let (x/324 =a b/321 p/325 =a (makeblock 0 a/320 b/321))
      (makeblock 0 (int,*) x/324 p/325))))
(function a/320[int] b/321[int]
  (if a/320 (makeblock 0 (int,*) a/320 (makeblock 0 a/320 b/321))
    (makeblock 0 (int,*) b/321 (makeblock 0 a/320 b/321))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/326[int] b/327[int]
  (catch
    (if a/326
      (let (x/334 =a[int] a/326 p/335 =a (makeblock 0 a/326 b/327))
        (exit 31 x/334 p/335))
      (let (x/332 =a b/327 p/333 =a (makeblock 0 a/326 b/327))
        (exit 31 x/332 p/333)))
   with (31 x/328[int] p/329) (makeblock 0 (int,*) x/328 p/329)))
(function a/326[int] b/327[int]
  (catch
    (if a/326 (exit 31 a/326 (makeblock 0 a/326 b/327))
      (exit 31 b/327 (makeblock 0 a/326 b/327)))
   with (31 x/328[int] p/329) (makeblock 0 (int,*) x/328 p/329)))
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
(function a/336[int] b/337[int]
  (if a/336
    (let (x/338 =a[int] a/336 _p/339 =a (makeblock 0 a/336 b/337))
      (makeblock 0 (int,*) x/338 [0: 1 1]))
    (let (x/340 =a[int] a/336 p/341 =a (makeblock 0 a/336 b/337))
      (makeblock 0 (int,*) x/340 p/341))))
(function a/336[int] b/337[int]
  (if a/336 (makeblock 0 (int,*) a/336 [0: 1 1])
    (makeblock 0 (int,*) a/336 (makeblock 0 a/336 b/337))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/342[int] b/343
  (let (x/344 =a[int] a/342 p/345 =a (makeblock 0 a/342 b/343))
    (makeblock 0 (int,*) x/344 p/345)))
(function a/342[int] b/343
  (makeblock 0 (int,*) a/342 (makeblock 0 a/342 b/343)))
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
(function a/355[int] b/356
  (catch
    (if a/355 (if b/356 (let (p/357 =a (field_imm 0 b/356)) p/357) (exit 42))
      (exit 42))
   with (42) (let (p/358 =a (makeblock 0 a/355 b/356)) p/358)))
(function a/355[int] b/356
  (catch (if a/355 (if b/356 (field_imm 0 b/356) (exit 42)) (exit 42))
   with (42) (makeblock 0 a/355 b/356)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/359[int] b/360
  (catch
    (catch
      (if a/359
        (if b/360 (let (p/364 =a (field_imm 0 b/360)) (exit 46 p/364))
          (exit 47))
        (exit 47))
     with (47) (let (p/363 =a (makeblock 0 a/359 b/360)) (exit 46 p/363)))
   with (46 p/361) p/361))
(function a/359[int] b/360
  (catch
    (catch
      (if a/359 (if b/360 (exit 46 (field_imm 0 b/360)) (exit 47)) (exit 47))
     with (47) (exit 46 (makeblock 0 a/359 b/360)))
   with (46 p/361) p/361))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
