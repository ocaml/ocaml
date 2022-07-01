(* TEST
 flags = "-nostdlib -nopervasives -dlambda"
 * expect
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
  (last_is_anys/11 =
     (function param/13 : int
       (catch
         (if (field_imm 0 param/13) (if (field_imm 1 param/13) (exit 1) 1)
           (if (field_imm 1 param/13) (exit 1) 2))
        with (1) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/11))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/18 =
     (function param/22 : int
       (catch
         (if (field_imm 0 param/22) (if (field_imm 1 param/22) (exit 3) 1)
           (if (field_imm 1 param/22) (exit 3) 2))
        with (3) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/18))
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
  (A/26 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/27 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/28 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/26" A/26)
    (apply (field_mut 1 (global Toploop!)) "B/27" B/27)
    (apply (field_mut 1 (global Toploop!)) "C/28" C/28)))
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
  (C/28 = (apply (field_mut 0 (global Toploop!)) "C/28")
   B/27 = (apply (field_mut 0 (global Toploop!)) "B/27")
   A/26 = (apply (field_mut 0 (global Toploop!)) "A/26")
   f/29 =
     (function param/31 : int
       (let (*match*/32 =a (field_imm 0 param/31))
         (catch
           (if (== *match*/32 A/26) (if (field_imm 1 param/31) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/31)
             (if (== (field_imm 0 *match*/32) B/27) 2
               (if (== (field_imm 0 *match*/32) C/28) 3 4))
             (if (field_imm 2 param/31) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/29))
val f : t * bool * bool -> int = <fun>
|}]
