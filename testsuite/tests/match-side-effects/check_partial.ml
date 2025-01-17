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
(* This pattern-matching is total: a Match_failure case is not
   necessary for soundness. *)
[%%expect {|
0
type _ t = Int : int -> int t | True : bool t | False : bool t
(let
  (lazy_total/283 =
     (function param/285 : int
       (let (*match*/287 =o (field_mut 0 (field_imm 0 param/285)))
         (switch* (field_imm 1 param/285)
          case int 0: 0
          case int 1:
           (let
             (*match*/295 =
                (let (tag/290 =a (caml_obj_tag *match*/287))
                  (if (== tag/290 250) (field_mut 0 *match*/287)
                    (if (|| (== tag/290 246) (== tag/290 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/287))
                      *match*/287))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/283))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/297 =
     (function param/299 : int
       (catch
         (let
           (*match*/300 =a (field_imm 0 param/299)
            *match*/302 =o (field_mut 0 (field_imm 1 param/299)))
           (switch* *match*/302
            case int 0: 0
            case int 1:
             (let
               (*match*/305 =
                  (let (tag/304 =a (caml_obj_tag *match*/300))
                    (if (== tag/304 250) (field_mut 0 *match*/300)
                      (if (|| (== tag/304 246) (== tag/304 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/300))
                        *match*/300)))
                *match*/307 =o (field_mut 0 (field_imm 1 param/299)))
               (if (isint *match*/307) (if *match*/307 12 (exit 3)) (exit 3)))))
        with (3)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/297))
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
  (guard_total/308 =
     (function param/385 : int
       (if (opaque 0) 1
         (let (*match*/386 =o (field_mut 0 param/385))
           (if (isint *match*/386) (if *match*/386 12 0)
             (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/308))
val guard_total : bool t ref -> int = <fun>
|}];;

let guard_needs_partial : bool t ref -> int = function
  | { contents = True } -> 0
  | _ when Sys.opaque_identity false -> 1
  | { contents = False } -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (guard_needs_partial/387 =
     (function param/389 : int
       (let (*match*/390 =o (field_mut 0 param/389))
         (catch (if (isint *match*/390) (if *match*/390 (exit 9) 0) (exit 9))
          with (9)
           (if (opaque 0) 1
             (if (isint *match*/390) 12
               (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/387))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
