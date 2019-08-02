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
  (last_is_anys/10 =
     (function param/11 : int
       (catch
         (if (field 0 param/11) (if (field 1 param/11) (exit 1) 1)
           (if (field 1 param/11) (exit 1) 2))
        with (1) 3)))
  (apply (field 1 (global Toploop!)) "last_is_anys" last_is_anys/10))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/16 =
     (function param/19 : int
       (catch
         (if (field 0 param/19) (if (field 1 param/19) (exit 3) 1) (exit 3))
        with (3) (if (field 1 param/19) 3 2))))
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/16))
val last_is_vars : bool * bool -> int = <fun>
|}]

(******************************************************************************)

type t = ..
type t += A | B of unit | C of bool * int;;
[%%expect{|
0a
type t = ..
(let
  (A/22 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/23 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/24 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/22" A/22)
    (apply (field 1 (global Toploop!)) "B/23" B/23)
    (apply (field 1 (global Toploop!)) "C/24" C/24)))
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
  (C/24 = (apply (field 0 (global Toploop!)) "C/24")
   B/23 = (apply (field 0 (global Toploop!)) "B/23")
   A/22 = (apply (field 0 (global Toploop!)) "A/22")
   f/25 =
     (function param/26 : int
       (let (*match*/27 =a (field 0 param/26))
         (catch
           (if (== *match*/27 A/22) (if (field 1 param/26) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field 1 param/26)
             (if (== (field 0 *match*/27) B/23) 2
               (if (== (field 0 *match*/27) C/24) 3 4))
             (if (field 2 param/26) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/25))
val f : t * bool * bool -> int = <fun>
|}]
