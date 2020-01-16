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
  (last_is_anys/12 =
     (function param/14 : int
       (catch
         (if (field 0 param/14) (if (field 1 param/14) (exit 1) 1)
           (if (field 1 param/14) (exit 1) 2))
        with (1) 3)))
  (apply (field 1 (global Toploop!)) "last_is_anys" last_is_anys/12))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/19 =
     (function param/23 : int
       (catch
         (if (field 0 param/23) (if (field 1 param/23) (exit 3) 1)
           (if (field 1 param/23) (exit 3) 2))
        with (3) 3)))
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/19))
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

(* Check that the [| _, false, true -> 12] gets raised. *)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0a
type t = ..
(let
  (A/27 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/28 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/29 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/27" A/27)
    (apply (field 1 (global Toploop!)) "B/28" B/28)
    (apply (field 1 (global Toploop!)) "C/29" C/29)))
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
  (C/29 = (apply (field 0 (global Toploop!)) "C/29")
   B/28 = (apply (field 0 (global Toploop!)) "B/28")
   A/27 = (apply (field 0 (global Toploop!)) "A/27")
   f/30 =
     (function param/32 : int
       (let (*match*/33 =a (field 0 param/32))
         (catch
           (if (== *match*/33 A/27) (if (field 1 param/32) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field 1 param/32)
             (if (== (field 0 *match*/33) B/28) 2
               (if (== (field 0 *match*/33) C/29) 3 4))
             (if (field 2 param/32) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/30))
val f : t * bool * bool -> int = <fun>
|}]
