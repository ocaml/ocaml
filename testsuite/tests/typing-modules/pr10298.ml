(* TEST
   * expect
*)

module type S = sig type t end
module Res_ko =
  (functor (X : S) -> X)(struct type t = int end)
[%%expect{|
module type S = sig type t end
module Res_ko : sig type t = int end
|}]

module Res_ok2 =
  (functor (X : S) -> struct include X end) (struct type t = int end)
[%%expect{|
module Res_ok2 : sig type t = int end
|}]
module Res_ok3 =
  (functor (X : S) -> struct type t = X.t end) (struct type t = int end)
[%%expect{|
module Res_ok3 : sig type t = int end
|}]
