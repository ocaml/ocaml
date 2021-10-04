(* TEST
   flags = "-drawlambda"
   * expect
*)

(* Successful flattening *)

match (3, 2, 1) with
| (_, 3, _)
| (1, _, _) -> true
| _ -> false
;;
[%%expect{|
(let (*match*/92 = 3 *match*/93 = 2 *match*/94 = 1)
  (catch
    (catch
      (catch (if (!= *match*/93 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/92 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
- : bool = false
|}];;

(* Failed flattening: we need to allocate the tuple to bind x. *)

match (3, 2, 1) with
| ((_, 3, _) as x)
| ((1, _, _) as x) -> ignore x; true
| _ -> false
;;
[%%expect{|
(let (*match*/97 = 3 *match*/98 = 2 *match*/99 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/98 3) (exit 6)
          (let (x/101 =a (makeblock 0 *match*/97 *match*/98 *match*/99))
            (exit 4 x/101)))
       with (6)
        (if (!= *match*/97 1) (exit 5)
          (let (x/100 =a (makeblock 0 *match*/97 *match*/98 *match*/99))
            (exit 4 x/100))))
     with (5) 0)
   with (4 x/95) (seq (ignore x/95) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/102[int] b/103 : int 0)
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
(function a/106[int] b/107 (let (p/108 =a (makeblock 0 a/106 b/107)) p/108))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/110[int] b/111 (let (p/112 =a (makeblock 0 a/110 b/111)) p/112))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/116[int] b/117
  (let (x/118 =a[int] a/116 p/119 =a (makeblock 0 a/116 b/117))
    (makeblock 0 (int,*) x/118 p/119)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/122[int] b/123
  (let (x/124 =a[int] a/122 p/125 =a (makeblock 0 a/122 b/123))
    (makeblock 0 (int,*) x/124 p/125)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/132[int] b/133[int]
  (if a/132
    (let (x/134 =a[int] a/132 p/135 =a (makeblock 0 a/132 b/133))
      (makeblock 0 (int,*) x/134 p/135))
    (let (x/136 =a b/133 p/137 =a (makeblock 0 a/132 b/133))
      (makeblock 0 (int,*) x/136 p/137))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/138[int] b/139[int]
  (catch
    (if a/138
      (let (x/146 =a[int] a/138 p/147 =a (makeblock 0 a/138 b/139))
        (exit 10 x/146 p/147))
      (let (x/144 =a b/139 p/145 =a (makeblock 0 a/138 b/139))
        (exit 10 x/144 p/145)))
   with (10 x/140[int] p/141) (makeblock 0 (int,*) x/140 p/141)))
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
(function a/148[int] b/149[int]
  (if a/148
    (let (x/150 =a[int] a/148 _p/151 =a (makeblock 0 a/148 b/149))
      (makeblock 0 (int,*) x/150 [0: 1 1]))
    (let (x/152 =a[int] a/148 p/153 =a (makeblock 0 a/148 b/149))
      (makeblock 0 (int,*) x/152 p/153))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/154[int] b/155
  (let (x/156 =a[int] a/154 p/157 =a (makeblock 0 a/154 b/155))
    (makeblock 0 (int,*) x/156 p/157)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
[%%expect{|
0
type 'a tuplist = Nil | Cons of ('a * 'a tuplist)
|}]

(* another example where we avoid an allocation in the first case *)
let _ =fun a b -> match a, b with
| (true, Cons p) -> p
| (_, _) as p -> p
(* outside, tuplist *)
[%%expect {|
(function a/167[int] b/168
  (catch
    (if a/167 (if b/168 (let (p/169 =a (field_imm 0 b/168)) p/169) (exit 12))
      (exit 12))
   with (12) (let (p/170 =a (makeblock 0 a/167 b/168)) p/170)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/171[int] b/172
  (catch
    (catch
      (if a/171
        (if b/172 (let (p/176 =a (field_imm 0 b/172)) (exit 13 p/176))
          (exit 14))
        (exit 14))
     with (14) (let (p/175 =a (makeblock 0 a/171 b/172)) (exit 13 p/175)))
   with (13 p/173) p/173))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
