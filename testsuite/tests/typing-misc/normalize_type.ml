(* TEST
   * expect
*)

(* #8907 *)

module M = struct
  type t = int
  let f (x : [< `Foo of t & int & string]) = ()
end;;
[%%expect{|
module M : sig type t = int val f : [< `Foo of t & int & string ] -> unit end
|}]

type t = int
let f (x : [< `Foo of t & int & string]) = () ;;
[%%expect{|
type t = int
val f : [< `Foo of t & int & string ] -> unit = <fun>
|}]
