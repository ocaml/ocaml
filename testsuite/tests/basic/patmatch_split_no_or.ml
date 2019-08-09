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
         (if (field 0 param/19) (if (field 1 param/19) (exit 3) 1)
           (if (field 1 param/19) (exit 3) 2))
        with (3) 3)))
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/16))
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
  (A/23 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/24 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/25 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/23" A/23)
    (apply (field 1 (global Toploop!)) "B/24" B/24)
    (apply (field 1 (global Toploop!)) "C/25" C/25)))
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
  (C/25 = (apply (field 0 (global Toploop!)) "C/25")
   B/24 = (apply (field 0 (global Toploop!)) "B/24")
   A/23 = (apply (field 0 (global Toploop!)) "A/23")
   f/26 =
     (function param/27 : int
       (let (*match*/28 =a (field 0 param/27))
         (catch
           (if (== *match*/28 A/23) (if (field 1 param/27) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field 1 param/27)
             (if (== (field 0 *match*/28) B/24) 2
               (if (== (field 0 *match*/28) C/25) 3 4))
             (if (field 2 param/27) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/26))
val f : t * bool * bool -> int = <fun>
|}]
