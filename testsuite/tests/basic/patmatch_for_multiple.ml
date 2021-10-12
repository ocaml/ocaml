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
(let (*match*/95 = 3 *match*/96 = 2 *match*/97 = 1)
  (catch
    (catch
      (catch (if (!= *match*/96 3) (exit 3) (exit 1)) with (3)
        (if (!= *match*/95 1) (exit 2) (exit 1)))
     with (2) 0)
   with (1) 1))
(let (*match*/95 = 3 *match*/96 = 2 *match*/97 = 1)
  (catch (if (!= *match*/96 3) (if (!= *match*/95 1) 0 (exit 1)) (exit 1))
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
(let (*match*/100 = 3 *match*/101 = 2 *match*/102 = 1)
  (catch
    (catch
      (catch
        (if (!= *match*/101 3) (exit 6)
          (let (x/104 =a (makeblock 0 *match*/100 *match*/101 *match*/102))
            (exit 4 x/104)))
       with (6)
        (if (!= *match*/100 1) (exit 5)
          (let (x/103 =a (makeblock 0 *match*/100 *match*/101 *match*/102))
            (exit 4 x/103))))
     with (5) 0)
   with (4 x/98) (seq (ignore x/98) 1)))
(let (*match*/100 = 3 *match*/101 = 2 *match*/102 = 1)
  (catch
    (if (!= *match*/101 3)
      (if (!= *match*/100 1) 0
        (exit 4 (makeblock 0 *match*/100 *match*/101 *match*/102)))
      (exit 4 (makeblock 0 *match*/100 *match*/101 *match*/102)))
   with (4 x/98) (seq (ignore x/98) 1)))
- : bool = false
|}];;

(* Regression test for #3780 *)
let _ = fun a b ->
  match a, b with
  | ((true, _) as _g)
  | ((false, _) as _g) -> ()
[%%expect{|
(function a/105[int] b/106 : int 0)
(function a/105[int] b/106 : int 0)
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
(function a/109[int] b/110 (let (p/111 =a (makeblock 0 a/109 b/110)) p/111))
(function a/109[int] b/110 (makeblock 0 a/109 b/110))
- : bool -> 'a -> bool * 'a = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true, _) as p)
| ((false, _) as p) -> p
(* inside, trivial *)
[%%expect{|
(function a/113[int] b/114 (let (p/115 =a (makeblock 0 a/113 b/114)) p/115))
(function a/113[int] b/114 (makeblock 0 a/113 b/114))
- : bool -> 'a -> bool * 'a = <fun>
|}];;

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false as x, _) as p -> x, p
(* outside, simple *)
[%%expect {|
(function a/119[int] b/120
  (let (x/121 =a[int] a/119 p/122 =a (makeblock 0 a/119 b/120))
    (makeblock 0 (int,*) x/121 p/122)))
(function a/119[int] b/120
  (makeblock 0 (int,*) a/119 (makeblock 0 a/119 b/120)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, simple *)
[%%expect {|
(function a/125[int] b/126
  (let (x/127 =a[int] a/125 p/128 =a (makeblock 0 a/125 b/126))
    (makeblock 0 (int,*) x/127 p/128)))
(function a/125[int] b/126
  (makeblock 0 (int,*) a/125 (makeblock 0 a/125 b/126)))
- : bool -> 'a -> bool * (bool * 'a) = <fun>
|}]

let _ = fun a b -> match a, b with
| (true as x, _) as p -> x, p
| (false, x) as p -> x, p
(* outside, complex *)
[%%expect{|
(function a/135[int] b/136[int]
  (if a/135
    (let (x/137 =a[int] a/135 p/138 =a (makeblock 0 a/135 b/136))
      (makeblock 0 (int,*) x/137 p/138))
    (let (x/139 =a b/136 p/140 =a (makeblock 0 a/135 b/136))
      (makeblock 0 (int,*) x/139 p/140))))
(function a/135[int] b/136[int]
  (if a/135 (makeblock 0 (int,*) a/135 (makeblock 0 a/135 b/136))
    (makeblock 0 (int,*) b/136 (makeblock 0 a/135 b/136))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false, x) as p)
  -> x, p
(* inside, complex *)
[%%expect{|
(function a/141[int] b/142[int]
  (catch
    (if a/141
      (let (x/149 =a[int] a/141 p/150 =a (makeblock 0 a/141 b/142))
        (exit 10 x/149 p/150))
      (let (x/147 =a b/142 p/148 =a (makeblock 0 a/141 b/142))
        (exit 10 x/147 p/148)))
   with (10 x/143[int] p/144) (makeblock 0 (int,*) x/143 p/144)))
(function a/141[int] b/142[int]
  (catch
    (if a/141 (exit 10 a/141 (makeblock 0 a/141 b/142))
      (exit 10 b/142 (makeblock 0 a/141 b/142)))
   with (10 x/143[int] p/144) (makeblock 0 (int,*) x/143 p/144)))
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
(function a/151[int] b/152[int]
  (if a/151
    (let (x/153 =a[int] a/151 _p/154 =a (makeblock 0 a/151 b/152))
      (makeblock 0 (int,*) x/153 [0: 1 1]))
    (let (x/155 =a[int] a/151 p/156 =a (makeblock 0 a/151 b/152))
      (makeblock 0 (int,*) x/155 p/156))))
(function a/151[int] b/152[int]
  (if a/151 (makeblock 0 (int,*) a/151 [0: 1 1])
    (makeblock 0 (int,*) a/151 (makeblock 0 a/151 b/152))))
- : bool -> bool -> bool * (bool * bool) = <fun>
|}]

let _ = fun a b -> match a, b with
| ((true as x, _) as p)
| ((false as x, _) as p) -> x, p
(* inside, onecase *)
[%%expect{|
(function a/157[int] b/158
  (let (x/159 =a[int] a/157 p/160 =a (makeblock 0 a/157 b/158))
    (makeblock 0 (int,*) x/159 p/160)))
(function a/157[int] b/158
  (makeblock 0 (int,*) a/157 (makeblock 0 a/157 b/158)))
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
(function a/170[int] b/171
  (catch
    (if a/170 (if b/171 (let (p/172 =a (field_imm 0 b/171)) p/172) (exit 12))
      (exit 12))
   with (12) (let (p/173 =a (makeblock 0 a/170 b/171)) p/173)))
(function a/170[int] b/171
  (catch (if a/170 (if b/171 (field_imm 0 b/171) (exit 12)) (exit 12))
   with (12) (makeblock 0 a/170 b/171)))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]

let _ = fun a b -> match a, b with
| (true, Cons p)
| ((_, _) as p) -> p
(* inside, tuplist *)
[%%expect{|
(function a/174[int] b/175
  (catch
    (catch
      (if a/174
        (if b/175 (let (p/179 =a (field_imm 0 b/175)) (exit 13 p/179))
          (exit 14))
        (exit 14))
     with (14) (let (p/178 =a (makeblock 0 a/174 b/175)) (exit 13 p/178)))
   with (13 p/176) p/176))
(function a/174[int] b/175
  (catch
    (catch
      (if a/174 (if b/175 (exit 13 (field_imm 0 b/175)) (exit 14)) (exit 14))
     with (14) (exit 13 (makeblock 0 a/174 b/175)))
   with (13 p/176) p/176))
- : bool -> bool tuplist -> bool * bool tuplist = <fun>
|}]
