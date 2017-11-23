(* TEST
   * expect
*)

type t = Set.Make(String).t
[%%expect{|
type t = Set.Make(String).t
|} ]


(* Check the error messages of an ill-typed applicatived functor type. *)
module M = struct type t let equal = (=) end
[%%expect{|
module M : sig type t val equal : 'a -> 'a -> bool end
|} ]

type t = Set.Make(M).t
[%%expect{|
Line _, characters 9-22:
  type t = Set.Make(M).t
           ^^^^^^^^^^^^^
Error: Ill-typed functor application Set.Make(M)
|} ]


(* We would report the wrong error here if we didn't strengthen the
   type of the argument (type t wouldn't match). *)
module F(X : sig type t = M.t val equal : unit end)
  = struct type t end
[%%expect{|
module F :
  functor (X : sig type t = M.t val equal : unit end) -> sig type t end
|} ]

type t = F(M).t
[%%expect{|
Line _, characters 9-15:
  type t = F(M).t
           ^^^^^^
Error: Ill-typed functor application F(M)
|} ]


(* We can use generative functors as applicative (bug MPR#7611). *)
module Generative() = struct type t end
[%%expect{|
module Generative : functor () -> sig type t end
|}]

type t = Generative(M).t
[%%expect{|
type t = Generative(M).t
|}]
