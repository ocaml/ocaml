(* TEST
   * expect
*)

(*** Record ***)

(* Expressions *)

module M = struct
  type r = { lbl : int }
end
;;
[%%expect{|
module M : sig type r = { lbl : int; } end
|}]

let before_a : M.r =
  { lbl = 3 }
;;
[%%expect{|
val before_a : M.r = {M.lbl = 3}
|}]

let a =
  let x = ({ M.lbl = 3 } : M.r) in
  x.lbl
;;
[%%expect{|
val a : int = 3
|}]

let after_a =
  let x = ({ M.lbl = 3 } : M.r) in
  { x with lbl = 4 }
;;
[%%expect{|
Line _, characters 2-20:
    { x with lbl = 4 }
    ^^^^^^^^^^^^^^^^^^
Warning 23: all the fields are explicitly listed in this record:
the 'with' clause is useless.
val after_a : M.r = {M.lbl = 4}
|}]

let b =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x := { lbl = 4 }
;;
[%%expect{|
val b : unit = ()
|}, Principal{|
Line _, characters 7-18:
    x := { lbl = 4 }
         ^^^^^^^^^^^
Warning 18: this type-based record disambiguation is not principal.
val b : unit = ()
|}]

let c =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  !x.lbl
;;
[%%expect{|
val c : int = 3
|}]

let d =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x.contents <- { lbl = 4 }
;;
[%%expect{|
val d : unit = ()
|}]

let e =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  { x with contents = { lbl = 4 } }
;;
[%%expect{|
Line _, characters 24-27:
    { x with contents = { lbl = 4 } }
                          ^^^
Error: Unbound record field lbl
|}]

let f =
  let x = ({ contents = { M.lbl = 3 } } : M.r ref) in
  x.contents.lbl
;;
[%%expect{|
val f : int = 3
|}]

(* Patterns *)

let g (x : M.r) =
  match x with
  | { lbl = _ } -> ()
;;
[%%expect{|
val g : M.r -> unit = <fun>
|}]

let h x =
  match x with
  | (_ : M.r) -> ()
  | { lbl = _ } -> ()
;;
[%%expect{|
Line _, characters 4-15:
    | { lbl = _ } -> ()
      ^^^^^^^^^^^
Warning 11: this match case is unused.
val h : M.r -> unit = <fun>
|}, Principal{|
Line _, characters 6-9:
    | { lbl = _ } -> ()
        ^^^
Error: Unbound record field lbl
|}]

let i x =
  match x with
  | { lbl = _ } -> ()
  | (_ : M.r) -> ()
;;
[%%expect{|
Line _, characters 6-9:
    | { lbl = _ } -> ()
        ^^^
Error: Unbound record field lbl
|}]

let j x =
  match x with
  | (_ : M.r)
  | { lbl = _ } -> ()
;;
[%%expect{|
Line _, characters 4-15:
    | { lbl = _ } -> ()
      ^^^^^^^^^^^
Warning 12: this sub-pattern is unused.
val j : M.r -> unit = <fun>
|}]

let k x =
  match x with
  | { lbl = _ }
  | (_ : M.r) -> ()
;;
[%%expect{|
Line _, characters 6-9:
    | { lbl = _ }
        ^^^
Error: Unbound record field lbl
|}]

let l (x : M.r ref) =
  match x with
  | { contents = { lbl = _ } } -> ()
;;
[%%expect{|
val l : M.r ref -> unit = <fun>
|}]

let m x =
  match x with
  | { contents = { lbl = _ } } -> ()
;;
[%%expect{|
Line _, characters 19-22:
    | { contents = { lbl = _ } } -> ()
                     ^^^
Error: Unbound record field lbl
|}]

let n x =
  match x with
  | (_ : M.r ref) -> ()
  | { contents = { lbl = _ } } -> ()
;;
[%%expect{|
Line _, characters 4-30:
    | { contents = { lbl = _ } } -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 11: this match case is unused.
val n : M.r ref -> unit = <fun>
|}, Principal{|
Line _, characters 19-22:
    | { contents = { lbl = _ } } -> ()
                     ^^^
Error: Unbound record field lbl
|}]

let o x =
  match x with
  | { contents = { lbl = _ } } -> ()
  | (_ : M.r ref) -> ()
;;
[%%expect{|
Line _, characters 19-22:
    | { contents = { lbl = _ } } -> ()
                     ^^^
Error: Unbound record field lbl
|}]

let p x =
  match x with
  | (_ : M.r ref)
  | { contents = { lbl = _ } } -> ()
;;
[%%expect{|
Line _, characters 4-30:
    | { contents = { lbl = _ } } -> ()
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Warning 12: this sub-pattern is unused.
val p : M.r ref -> unit = <fun>
|}]

let q x =
  match x with
  | { contents = { lbl = _ } }
  | (_ : M.r ref) -> ()
;;
[%%expect{|
Line _, characters 19-22:
    | { contents = { lbl = _ } }
                     ^^^
Error: Unbound record field lbl
|}]

let r arg =
  match arg with
  | (x : M.r ref) ->
    !x.lbl
;;
[%%expect{|
val r : M.r ref -> int = <fun>
|}]

let s arg =
  match arg with
  | (x : M.r ref) ->
    x := { lbl = 4 }
;;
[%%expect{|
val s : M.r ref -> unit = <fun>
|}, Principal{|
Line _, characters 9-20:
      x := { lbl = 4 }
           ^^^^^^^^^^^
Warning 18: this type-based record disambiguation is not principal.
val s : M.r ref -> unit = <fun>
|}]

let t = function
  | ({ contents = { M.lbl = _ } } : M.r ref) as x ->
    x := { lbl = 4 }
;;
[%%expect{|
val t : M.r ref -> unit = <fun>
|}, Principal{|
Line _, characters 9-20:
      x := { lbl = 4 }
           ^^^^^^^^^^^
Warning 18: this type-based record disambiguation is not principal.
val t : M.r ref -> unit = <fun>
|}]

let u = function
  | ({ contents = { M.lbl = _ } } : M.r ref) as x ->
    !x.lbl
;;
[%%expect{|
val u : M.r ref -> int = <fun>
|}, Principal{|
Line _, characters 7-10:
      !x.lbl
         ^^^
Warning 18: this type-based field disambiguation is not principal.
val u : M.r ref -> int = <fun>
|}]
