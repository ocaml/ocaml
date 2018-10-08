(* TEST
   * expect
*)

(* PR#6768 *)

type _ prod = Prod : ('a * 'y) prod;;

let f : type t. t prod -> _ = function Prod ->
  let module M =
    struct
      type d = d * d
    end
  in ()
;;
[%%expect{|
type _ prod = Prod : ('a * 'y) prod
Line 6, characters 6-20:
6 |       type d = d * d
          ^^^^^^^^^^^^^^
Error: The type abbreviation d is cyclic
|}];;
