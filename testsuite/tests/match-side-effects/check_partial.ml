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
  (lazy_total/284 =
     (function param/286 : int
       (let (*match*/288 =o (field_mut 0 (field_imm 0 param/286)))
         (switch* (field_imm 1 param/286)
          case int 0: 0
          case int 1:
           (let
             (*match*/296 =
                (let (tag/291 =a (caml_obj_tag *match*/288))
                  (if (== tag/291 250) (field_mut 0 *match*/288)
                    (if (|| (== tag/291 246) (== tag/291 244))
                      (apply (field_imm 1 (global CamlinternalLazy!))
                        (opaque *match*/288))
                      *match*/288))))
             12)))))
  (apply (field_mut 1 (global Toploop!)) "lazy_total" lazy_total/284))
val lazy_total : unit lazy_t ref * bool t -> int = <fun>
|}];;

let lazy_needs_partial : _ * bool t ref -> int = function
  | (_, { contents = True }) -> 0
  | (lazy (), { contents = False }) -> 12
(* This pattern-matching is partial: a Match_failure case is
   necessary for soundness. *)
[%%expect {|
(let
  (lazy_needs_partial/298 =
     (function param/300 : int
       (catch
         (let
           (*match*/301 =a (field_imm 0 param/300)
            *match*/303 =o (field_mut 0 (field_imm 1 param/300)))
           (switch* *match*/303
            case int 0: 0
            case int 1:
             (let
               (*match*/306 =
                  (let (tag/305 =a (caml_obj_tag *match*/301))
                    (if (== tag/305 250) (field_mut 0 *match*/301)
                      (if (|| (== tag/305 246) (== tag/305 244))
                        (apply (field_imm 1 (global CamlinternalLazy!))
                          (opaque *match*/301))
                        *match*/301)))
                *match*/308 =o (field_mut 0 (field_imm 1 param/300)))
               (if (isint *match*/308) (if *match*/308 12 (exit 3)) (exit 3)))))
        with (3)
         (raise (makeblock 0 (global Match_failure/21!) [0: "" 1 49])))))
  (apply (field_mut 1 (global Toploop!)) "lazy_needs_partial"
    lazy_needs_partial/298))
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
  (guard_total/309 =
     (function param/386 : int
       (if (opaque 0) 1
         (let (*match*/387 =o (field_mut 0 param/386))
           (if (isint *match*/387) (if *match*/387 12 0)
             (raise (makeblock 0 (global Match_failure/21!) [0: "" 1 38])))))))
  (apply (field_mut 1 (global Toploop!)) "guard_total" guard_total/309))
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
  (guard_needs_partial/388 =
     (function param/390 : int
       (let (*match*/391 =o (field_mut 0 param/390))
         (catch (if (isint *match*/391) (if *match*/391 (exit 9) 0) (exit 9))
          with (9)
           (if (opaque 0) 1
             (if (isint *match*/391) 12
               (raise (makeblock 0 (global Match_failure/21!) [0: "" 1 46]))))))))
  (apply (field_mut 1 (global Toploop!)) "guard_needs_partial"
    guard_needs_partial/388))
val guard_needs_partial : bool t ref -> int = <fun>
|}];;
