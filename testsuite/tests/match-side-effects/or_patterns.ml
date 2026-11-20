(* TEST
 flags = "-dlambda -dcanonical-ids";
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
[%%expect {|
(let
  (f/0 =
     (function r/0 : int
       (catch
         (let (*match*/0 =o (field_mut 0 r/0))
           (if *match*/0
             (let (x/0 =a (field_imm 0 *match*/0))
               (if (!= x/0 1) (exit 3 x/0) (exit 3 x/0)))
             0))
        with (3 x/1[int])
         (if (seq (setfield_ptr 0 r/0 0) 0) x/1
           (let (*match*/1 =o (field_mut 0 r/0))
             (if *match*/1 (+ (field_imm 0 *match*/1) 1000)
               (raise (makeblock 0 (global Match_failure/0!) [0: "" 2 2]))))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/0))
val f : int option ref -> int = <fun>
|}];;
