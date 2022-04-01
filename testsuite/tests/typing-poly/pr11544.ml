(* TEST
   * expect
*)

module M = struct type t = T end
let poly3 : 'b. M.t -> 'b -> 'b =
  fun T x -> x
[%%expect {|
module M : sig type t = T end
val poly3 : M.t -> 'b -> 'b = <fun>
|}, Principal{|
module M : sig type t = T end
Line 3, characters 6-7:
3 |   fun T x -> x
          ^
Warning 18 [not-principal]: this type-based constructor disambiguation is not principal.
val poly3 : M.t -> 'b -> 'b = <fun>
|}];;
