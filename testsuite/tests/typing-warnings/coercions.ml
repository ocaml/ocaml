(* TEST
   flags = " -w +A -strict-sequence "
   * expect
*)

(* comment 9644 of PR#6000 *)

fun b -> if b then format_of_string "x" else "y"
[%%expect {|
- : bool -> ('a, 'b, 'c, 'd, 'd, 'a) format6 = <fun>
|}, Principal{|
Line 1, characters 45-48:
1 | fun b -> if b then format_of_string "x" else "y"
                                                 ^^^
Warning 18 [not-principal]: this coercion to format6 is not principal.
- : bool -> ('a, 'b, 'c, 'd, 'd, 'a) format6 = <fun>
|}]
;;

fun b -> if b then "x" else format_of_string "y"
[%%expect {|
Line 1, characters 28-48:
1 | fun b -> if b then "x" else format_of_string "y"
                                ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type
         ('a, 'b, 'c, 'd, 'd, 'a) format6 =
           ('a, 'b, 'c, 'd, 'd, 'a) CamlinternalFormatBasics.format6
       but an expression was expected of type string
|}]
;;

fun b : (_,_,_) format -> if b then "x" else "y"
[%%expect {|
- : bool -> ('a, 'b, 'a) format = <fun>
|}]
;;

(* PR#7135 *)

module PR7135 = struct
  module M : sig type t = private int end =  struct type t = int end
  include M

  let lift2 (f : int -> int -> int) (x : t) (y : t) =
    f (x :> int) (y :> int)
end;;
[%%expect {|
module PR7135 :
  sig
    module M : sig type t = private int end
    type t = M.t
    val lift2 : (int -> int -> int) -> t -> t -> int
  end
|}]

(* example of non-ground coercion *)

module Test1 = struct
  type t = private int
  let f x = let y = if true then x else (x:t) in (y :> int)
end;;
[%%expect {|
module Test1 : sig type t = private int val f : t -> int end
|}, Principal{|
Line 3, characters 49-59:
3 |   let f x = let y = if true then x else (x:t) in (y :> int)
                                                     ^^^^^^^^^^
Warning 18 [not-principal]: this ground coercion is not principal.
module Test1 : sig type t = private int val f : t -> int end
|}]
