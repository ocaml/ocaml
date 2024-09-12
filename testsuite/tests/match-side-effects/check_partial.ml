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
  (lazy_total/282 =
     (function param/284 : int
       (let (*match*/286 =o (field_mut 0 (field_imm 0 param/284)))
         (switch* (field_imm 1 param/284)
          case int 0: 0
          case int 1:
           (let
             (*match*/294 =
                (let (tag/289 =a (caml_obj_tag *match*/286))
                  (if (== tag/289 250) (field_mut 0 *match*/286)
                    (if (|| (== tag/289 246) (== tag/289 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/286))
                      *match*/286))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/282))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/296 =
     (function param/298 : int
       (catch
         (let
           (*match*/299 =a (field_imm 0 param/298)
            *match*/301 =o (field_mut 0 (field_imm 1 param/298)))
           (switch* *match*/301
            case int 0: 0
            case int 1:
             (let
               (*match*/304 =
                  (let (tag/303 =a (caml_obj_tag *match*/299))
                    (if (== tag/303 250) (field_mut 0 *match*/299)
                      (if (|| (== tag/303 246) (== tag/303 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/299))
                        *match*/299)))
                *match*/306 =o (field_mut 0 (field_imm 1 param/298)))
               (if (isint *match*/306) (if *match*/306 12 (exit 3)) (exit 3)))))
        with (3)
         (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/296))
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
  (guard_total/307 =
     (function param/384 : int
       (if (opaque 0) 1
         (let (*match*/385 =o (field_mut 0 param/384))
           (if (isint *match*/385) (if *match*/385 12 0)
             (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/307))
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
  (guard_needs_partial/386 =
     (function param/388 : int
       (let (*match*/389 =o (field_mut 0 param/388))
         (catch (if (isint *match*/389) (if *match*/389 (exit 9) 0) (exit 9))
          with (9)
           (if (opaque 0) 1
             (if (isint *match*/389) 12
               (raise (makeblock 0 (global Match_failure/20!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/386))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
