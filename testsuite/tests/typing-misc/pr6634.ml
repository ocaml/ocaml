(* TEST
   * expect
 *)

type t = int
module M : sig type t end with type t = [`T of t] =
struct
  type t = [`T of t]
end;;

[%%expect{|
type t = int
Lines 3-5, characters 0-3:
3 | struct
4 |   type t = [`T of t]
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type t = [ `T of t ] end
       is not included in
         sig type t = [ `T of t ] end
       Type declarations do not match:
         type t = [ `T of t/2 ]
       is not included in
         type t = [ `T of t/1 ]
       Line 1, characters 0-12:
         Definition of type t/1
       Line 4, characters 2-20:
         Definition of type t/2
|}]
