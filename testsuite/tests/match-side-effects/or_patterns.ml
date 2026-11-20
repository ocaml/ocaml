(* TEST
 flags = "-drawlambda -dcanonical-ids";
 expect;
*)

(* Or-patterns flow through the pattern-matching compiler in a particular way.
   This test checks that mutable context is properly erased even when or-patterns
   are present. *)

let f (r : int option ref) =
  match r with
  | ({ contents = Some (1 as x) } | { contents = Some x }) when (r := None; false) -> x
  | { contents = None } -> 0
  | { contents = Some v } -> v + 1000
(* In this version, there must be a check on the second mutable read
   [*match*/1 = (field_mut 0 r/0)]: the read of the sub-field
   [field_imm 0 *match*/1] can only happen after checking that
   [*match*/1] is not [0] / [None]. *)
[%%expect {|
(let
  (f/0 =
     (function r/0 : int
       (catch
         (catch
           (catch
             (let (*match*/0 =o (field_mut 0 r/0))
               (if *match*/0
                 (let (x/0 =a (field_imm 0 *match*/0))
                   (catch (if (!= x/0 1) (exit 4) (exit 3 x/0)) with (4)
                     (let (x/1 =a x/0) (exit 3 x/1))))
                 0))
            with (3 x/2[int])
             (if (seq (setfield_ptr 0 r/0 0) 0) x/2 (exit 2)))
          with (2)
           (let (*match*/1 =o (field_mut 0 r/0))
             (if *match*/1
               (let (v/0 =a (field_imm 0 *match*/1)) (+ v/0 1000)) (exit 1))))
        with (1) (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2])))))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))
val f : int option ref -> int = <fun>
|}];;

type 'a immut_ref = { contents : 'a }
let g (r : int option immut_ref) =
  match r with
  | ({ contents = Some (1 as x) } | { contents = Some x })
    when (ignore (Sys.opaque_identity r); false) -> x
  | { contents = None } -> 0
  | { contents = Some v } -> v + 1000
(* In this immutable version there should be no pessimization of
   contexts, so the access [field_imm 0 *match*/3] is not necessarily
   guarded by a check that [*match/3*] is not 0. *)
[%%expect {|
0
type 'a immut_ref = { contents : 'a; }
(let
  (g/0 =
     (function r/1 : int
       (catch
         (catch
           (let (*match*/2 =a (field_imm 0 r/1))
             (if *match*/2
               (let (x/3 =a (field_imm 0 *match*/2))
                 (catch (if (!= x/3 1) (exit 9) (exit 8 x/3)) with (9)
                   (let (x/4 =a x/3) (exit 8 x/4))))
               0))
          with (8 x/5[int]) (if (seq (ignore (opaque r/1)) 0) x/5 (exit 7)))
        with (7)
         (let (*match*/3 =a (field_imm 0 r/1) v/1 =a (field_imm 0 *match*/3))
           (+ v/1 1000)))))
  (apply (field_mut 1 (global Toploop!)) "g" g/0))
val g : int option immut_ref -> int = <fun>
|}];;

(* a variation of the [immut_ref] example where code gets pessimized *)
let h (r : int option immut_ref) =
  match r with
  | ({ contents = Some (1 as x) } | { contents = Some x }) when (false) -> x
  | { contents = None } -> 0
  | { contents = Some v } when true -> v + 1000
[@@warning "-partial-match"]
[%%expect {|
(let
  (h/0 =
     (function r/2 : int
       (catch
         (catch
           (catch
             (let (*match*/4 =a (field_imm 0 r/2))
               (if *match*/4
                 (let (x/6 =a (field_imm 0 *match*/4))
                   (catch (if (!= x/6 1) (exit 14) (exit 13 x/6)) with (14)
                     (let (x/7 =a x/6) (exit 13 x/7))))
                 0))
            with (13 x/8[int]) (if 0 x/8 (exit 12)))
          with (12)
           (let (*match*/5 =a (field_imm 0 r/2))
             (if *match*/5
               (if 1 (let (v/2 =a (field_imm 0 *match*/5)) (+ v/2 1000))
                 (exit 11))
               (exit 11))))
        with (11)
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2])))))
  (apply (field_mut 1 (global Toploop!)) "h" h/0))
val h : int option immut_ref -> int = <fun>
|}]
