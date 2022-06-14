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
  (last_is_anys/30 =
     (function param/32 : int
       (catch
         (if (field_imm 0 param/32) (if (field_imm 1 param/32) (exit 1) 1)
           (if (field_imm 1 param/32) (exit 1) 2))
        with (1) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_anys" last_is_anys/30))
val last_is_anys : bool * bool -> int = <fun>
|}]

let last_is_vars = function
  | true, false -> 1
  | _, false -> 2
  | _x, _y -> 3
;;
[%%expect{|
(let
  (last_is_vars/37 =
     (function param/41 : int
       (catch
         (if (field_imm 0 param/41) (if (field_imm 1 param/41) (exit 3) 1)
           (if (field_imm 1 param/41) (exit 3) 2))
        with (3) 3)))
  (apply (field_mut 1 (global Toploop!)) "last_is_vars" last_is_vars/37))
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
  (A/45 = (makeblock 248 "A" (caml_fresh_oo_id 0))
   B/46 = (makeblock 248 "B" (caml_fresh_oo_id 0))
   C/47 = (makeblock 248 "C" (caml_fresh_oo_id 0)))
  (seq (apply (field_mut 1 (global Toploop!)) "A/45" A/45)
    (apply (field_mut 1 (global Toploop!)) "B/46" B/46)
    (apply (field_mut 1 (global Toploop!)) "C/47" C/47)))
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
  (C/47 = (apply (field_mut 0 (global Toploop!)) "C/47")
   B/46 = (apply (field_mut 0 (global Toploop!)) "B/46")
   A/45 = (apply (field_mut 0 (global Toploop!)) "A/45")
   f/48 =
     (function param/50 : int
       (let (*match*/51 =a (field_imm 0 param/50))
         (catch
           (if (== *match*/51 A/45) (if (field_imm 1 param/50) 1 (exit 8))
             (exit 8))
          with (8)
           (if (field_imm 1 param/50)
             (if (== (field_imm 0 *match*/51) B/46) 2
               (if (== (field_imm 0 *match*/51) C/47) 3 4))
             (if (field_imm 2 param/50) 12 11))))))
  (apply (field_mut 1 (global Toploop!)) "f" f/48))
val f : t * bool * bool -> int = <fun>
|}]
