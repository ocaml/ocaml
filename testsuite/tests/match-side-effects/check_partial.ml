(* TEST
 flags = "-dlambda";
 expect;
*)

(* This test exercises pattern-matching examples that mix mutable
   state with code execution (through guards or lazy patterns). Some
   of those tests appear to be exhaustive to the type-checker but are
   in fact not exhaustive, forcing the pattern-matching compiler to
   add Match_failure clauses for soundness. The pattern-matching
   compiler also sometimes conservatively add Match_failure clauses in
   cases that were in fact exhaustive.
*)

type _ t =
  | Int : int -> int t
  | True : bool t
  | False : bool t

let lazy_total : _ * bool t -> int = function
  | ({ contents = _ }, True) -> 0
  | ({ contents = lazy () }, False) -> 12
(* This pattern-matching is in fact total: a Match_failure case is
   not necessary for soundness. *)
[%%expect {|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/281 =
     (function param/283 : int
       (let
         (*match*/285 =o (field_mut 0 (field_imm 0 param/283))
          *match*/286 =a (field_imm 1 param/283))
         (if (isint *match*/286)
           (if *match*/286
             (let
               (*match*/293 =
                  (let (tag/288 =a (caml_obj_tag *match*/285))
                    (if (== tag/288 250) (field_mut 0 *match*/285)
                      (if (|| (== tag/288 246) (== tag/288 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/285))
                        *match*/285))))
               12)
             0)
           (raise (makeblock 0 (global Match_failure/20!) [0: "" 6 37]))))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/281))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/295 =
     (function param/297 : int
       (catch
         (let
           (*match*/298 =a (field_imm 0 param/297)
            *match*/300 =o (field_mut 0 (field_imm 1 param/297)))
           (if (isint *match*/300)
             (if *match*/300
               (let
                 (*match*/303 =
                    (let (tag/302 =a (caml_obj_tag *match*/298))
                      (if (== tag/302 250) (field_mut 0 *match*/298)
                        (if (|| (== tag/302 246) (== tag/302 244))
                          (apply (field_imm 1 (global CamlinternalLazy!))
                            (opaque *match*/298))
                          *match*/298)))
                  *match*/305 =o (field_mut 0 (field_imm 1 param/297)))
                 (if (isint *match*/305) (if *match*/305 12 (exit 3))
                   (exit 3)))
               0)
             (exit 3)))
        with (3)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/295))
val lazy_needs_partial : unit lazy_t * bool t ref -> int = <fun>
|}];;

let guard_total : bool t ref -> int = function
  | _ when Sys.opaque_identity false -> 1
  | { contents = True } -> 0
  | { contents = False } -> 12
(* This pattern-matching is total: a Match_failure case is not
   necessary for soundness. *)
[%%expect {|
(let
  (guard_total/306 =
     (function param/383 : int
       (if (opaque 0) 1
         (let (*match*/384 =o (field_mut 0 param/383))
           (switch* *match*/384 case int 0: 0
                                case int 1: 12)))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/306))
val guard_total : bool t ref -> int = <fun>
|}];;

let guard_needs_partial : bool t ref -> int = function
  | { contents = True } -> 0
  | _ when Sys.opaque_identity false -> 1
  | { contents = False } -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness.

   FAIL: the compiler is currently unsound here. *)
[%%expect {|
(let
  (guard_needs_partial/385 =
     (function param/387 : int
       (let (*match*/388 =o (field_mut 0 param/387))
         (catch (if (isint *match*/388) (if *match*/388 (exit 9) 0) (exit 9))
          with (9) (if (opaque 0) 1 12)))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/385))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
