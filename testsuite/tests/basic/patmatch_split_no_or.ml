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
     (function param/12 : int
       (catch
         (if (field 0 param/12) (if (field 1 param/12) (exit 1) 1)
           (if (field 1 param/12) (exit 1) 2))
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
  (last_is_vars/17 =
     (function param/21 : int
       (catch
         (if (field 0 param/21) (if (field 1 param/21) (exit 3) 1)
           (if (field 1 param/21) (exit 3) 2))
        with (3) 3)))
  (apply (field 1 (global Toploop!)) "last_is_vars" last_is_vars/17))
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
  (A/25 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/26 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/27 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field 1 (global Toploop!)) "A/25" A/25)
    (apply (field 1 (global Toploop!)) "B/26" B/26)
    (apply (field 1 (global Toploop!)) "C/27" C/27)))
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
  (C/27 = (apply (field 0 (global Toploop!)) "C/27")
   B/26 = (apply (field 0 (global Toploop!)) "B/26")
   A/25 = (apply (field 0 (global Toploop!)) "A/25")
   f/28 =
     (function param/30 : int
       (let (*match*/31 =a (field 0 param/30))
         (catch
           (if (== *match*/31 A/25) (if (field 1 param/30) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field 1 param/30)
             (if (== (field 0 *match*/31) B/26) 2
               (if (== (field 0 *match*/31) C/27) 3 4))
             (if (field 2 param/30) 12 11))))))
  (apply (field 1 (global Toploop!)) "f" f/28))
val f : t * bool * bool -> int = <fun>
|}]
