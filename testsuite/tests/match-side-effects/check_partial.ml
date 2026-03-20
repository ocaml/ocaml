(* TEST
 flags = "-dlambda -dcanonical-ids";
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
  (lazy_total/0 =
     (function param/0 : int
       (let (*match*/0 =o (field_mut 0 (field_imm 0 param/0)))
         (switch* (field_imm 1 param/0)
          case int 0: 0
          case int 1:
           (let
             (*match*/1 =
                (let (tag/0 =a (caml_obj_tag *match*/0))
                  (if (== tag/0 250) (field_mut 0 *match*/0)
                    (if (|| (== tag/0 246) (== tag/0 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/0))
                      *match*/0))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/0))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/0 =
     (function param/1 : int
       (catch
         (let
           (*match*/2 =a (field_imm 0 param/1)
            *match*/3 =o (field_mut 0 (field_imm 1 param/1)))
           (switch* *match*/3
            case int 0: 0
            case int 1:
             (let
               (*match*/4 =
                  (let (tag/1 =a (caml_obj_tag *match*/2))
                    (if (== tag/1 250) (field_mut 0 *match*/2)
                      (if (|| (== tag/1 246) (== tag/1 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/2))
                        *match*/2)))
                *match*/5 =o (field_mut 0 (field_imm 1 param/1)))
               (if (isint *match*/5) (if *match*/5 12 (exit 3)) (exit 3)))))
        with (3)
         (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/0))
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
  (guard_total/0 =
     (function param/2 : int
       (if (opaque 0) 1
         (let (*match*/6 =o (field_mut 0 param/2))
           (if (isint *match*/6) (if *match*/6 12 0)
             (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/0))
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
  (guard_needs_partial/0 =
     (function param/3 : int
       (let (*match*/7 =o (field_mut 0 param/3))
         (catch (if (isint *match*/7) (if *match*/7 (exit 9) 0) (exit 9))
          with (9)
           (if (opaque 0) 1
             (if (isint *match*/7) 12
               (raise (makeblock 0 (global Match_failure/0!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/0))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
