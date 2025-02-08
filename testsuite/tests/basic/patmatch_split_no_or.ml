(* TEST
 flags = "-nostdlib -nopervasives -dlambda -dcanonical-ids";
 expect;
*)

(******************************************************************************)

(* Check that the extra split indeed happens when the last row is made of
   "variables" only *)

let last_is_anys = function
  | true, false -> 1
  | _, false -> 2
  | _, _ -> 3
;;
[%%expect{|
(let
  (last_is_anys =
     (function param : int
       (catch
         (if (field_imm 0 param) (if (field_imm 1 param) (exit 2) 1)
           (if (field_imm 1 param) (exit 2) 2))
        with (2) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars =
     (function param/1 : int
       (catch
         (if (field_imm 0 param/1) (if (field_imm 1 param/1) (exit 5) 1)
           (if (field_imm 1 param/1) (exit 5) 2))
        with (5) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars))
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

(* Check that the [| _, false, true -> 12] gets raised. *)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0
type t = ..
(let
  (A = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/26" A)
    (apply (field_mut 1 (global Toploop!)) "B/27" B)
    (apply (field_mut 1 (global Toploop!)) "C/28" C)))
type t += A | B of unit | C of bool * int
|}]

let f = function
  | A, true, _ -> 1
  | _, false, false -> 11
  | B _, true, _ -> 2
  | C _, true, _ -> 3
  | _, false, true -> 12
  | _ -> 4
;;
[%%expect{|
(let
  (C = (apply (field_mut 0 (global Toploop!)) "C/28")
   B = (apply (field_mut 0 (global Toploop!)) "B/27")
   A = (apply (field_mut 0 (global Toploop!)) "A/26")
   f =
     (function param/2 : int
       (let (*match* =a (field_imm 0 param/2))
         (catch
           (if (== *match* A) (if (field_imm 1 param/2) 1 (exit 11))
             (exit 11))
          with (11)
           (if (field_imm 1 param/2)
             (if (== (field_imm 0 *match*) B) 2
               (if (== (field_imm 0 *match*) C) 3 4))
             (if (field_imm 2 param/2) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f))
val f : t * bool * bool -> int = <fun>
|}]
