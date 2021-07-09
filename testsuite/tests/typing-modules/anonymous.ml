(* TEST
   * expect
*)

module _ = struct end;;
[%%expect{|
|}];;

module rec A : sig
  type t = B.t
end = A
and _ : sig type t = A.t end = struct type t = A.t end
and B : sig type t end = B
;;
[%%expect{|
module rec A : sig type t = B.t end
and B : sig type t end
|}]

module type S = sig
  module _ : sig end

  module rec A : sig
    type t = B.t
  end
  and _ : sig type t = A.t end
  and B : sig type t end
end
;;
[%%expect{|
module type S =
  sig module rec A : sig type t = B.t end and B : sig type t end end
|}]

let f (module _ : S) = ()
;;
[%%expect{|
val f : (module S) -> unit = <fun>
|}]
