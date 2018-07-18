(* TEST
  ppxs="empty_ppx.ml"
  * expect
*)

type t = [%empty_polyvar];;

let f: 'a. t -> 'a = function #t -> . ;;
[%%expect{|
type t = [  ]
Line _, characters 31-32:
  let f: 'a. t -> 'a = function #t -> . ;;
                                 ^
Error: The type t
is not a variant type
|}]
