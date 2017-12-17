(* TEST
   * expect
*)

(* PR#6394 *)

module rec X : sig
 type t = int * bool
end = struct
 type t = A | B
 let f = function A | B -> 0
end;;
[%%expect{|
Line _, characters 6-61:
  ......struct
   type t = A | B
   let f = function A | B -> 0
  end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = X.t = A | B val f : t -> int end
       is not included in
         sig type t = int * bool end
       Type declarations do not match:
         type t = X.t = A | B
       is not included in
         type t = int * bool
|}];;

type t = |;;
type g = C of t * t | D of t * int;;
let f (x:t) = match x with _ -> .;;
let f (x:g) = match x with C _ | D _ -> .;;
let f (x:g) = match x with _ -> . ;;
[%%expect{|
type t = |
type g = C of t * t | D of t * int
val f : t -> 'a = <fun>
val f : g -> 'a = <fun>
Line _, characters 27-28:
Error: This match case could not be refuted.
       Here is an example of a value that would reach it: _
|}]
