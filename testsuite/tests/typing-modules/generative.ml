(* TEST
 expect;
*)

(* Using generative functors *)

(* Without type *)
module type S = sig val x : int end;;
let v = (module struct let x = 3 end : S);;
module F() = (val v);; (* ok *)
module G (X : sig end) : S = F ();; (* ok *)
module H (X : sig end) = (val v);; (* ok *)
[%%expect{|
module type S = sig val x : int end
val v : (module S) = <module>
module F : () -> S
module G : (X : sig end) -> S
module H : (X : sig end) -> S
|}];;

(* With type *)
module type S = sig type t val x : t end;;
let v = (module struct type t = int let x = 3 end : S);;
module F() = (val v);; (* ok *)
[%%expect{|
module type S = sig type t val x : t end
val v : (module S) = <module>
module F : () -> S
|}];;
module G (X : sig end) : S = F ();; (* fail *)
[%%expect{|
Line 1, characters 29-33:
1 | module G (X : sig end) : S = F ();; (* fail *)
                                 ^^^^
Error: This expression creates fresh types.
       It is not allowed inside applicative functors.
|}];;
module H() = F();; (* ok *)
[%%expect{|
module H : () -> S
|}];;

(* Alias *)
module U = struct end;;
module M1 = F();; (* ok *)
module M2 = F(struct end);; (* accepted with a warning *)
[%%expect{|
module U : sig end
module M1 : S
Line 3, characters 14-24:
3 | module M2 = F(struct end);; (* accepted with a warning *)
                  ^^^^^^^^^^
Warning 73 [generative-application-expects-unit]: A generative functor
  should be applied to "()"; using "(struct end)" is deprecated.

module M2 : S
|}];;
module M = F(U);; (* fail *)
[%%expect{|
Line 1, characters 11-12:
1 | module M = F(U);; (* fail *)
               ^
Error: This is a generative functor. It can only be applied to "()"
|}];;

(* Cannot coerce between applicative and generative *)
module F1 (X : sig end) = struct end;;
module F2 : () -> sig end = F1;; (* fail *)
[%%expect{|
module F1 : (X : sig end) -> sig end
Line 2, characters 28-30:
2 | module F2 : () -> sig end = F1;; (* fail *)
                                ^^
Error: Signature mismatch:
       Modules do not match:
         (X : sig end) -> ...
       is not included in
         () -> ...
       The functor was expected to be generative at this position
|}];;
module F3 () = struct end;;
module F4 : functor (X : sig end) -> sig end = F3;; (* fail *)
[%%expect{|
module F3 : () -> sig end
Line 2, characters 47-49:
2 | module F4 : functor (X : sig end) -> sig end = F3;; (* fail *)
                                                   ^^
Error: Signature mismatch:
       Modules do not match:
         () -> ...
       is not included in
         (X : sig end) -> ...
       The functor was expected to be applicative at this position
|}];;

(* tests for shortened functor notation () *)
module X (X: sig end) (Y: sig end) = functor (Z: sig end) -> struct end;;
module Y = functor (X: sig end) (Y:sig end) -> functor (Z: sig end) ->
  struct end;;
module Z = functor (_: sig end) (_:sig end) (_: sig end) -> struct end;;
module GZ : functor (X: sig end) () (Z: sig end) -> sig end
          = functor (X: sig end) () (Z: sig end) -> struct end;;
[%%expect{|
module X : (X : sig end) (Y : sig end) (Z : sig end) -> sig end
module Y : (X : sig end) (Y : sig end) (Z : sig end) -> sig end
module Z : sig end -> sig end -> sig end -> sig end
module GZ : (X : sig end) () (Z : sig end) -> sig end
|}];;

(* disabling warning 73 in the argument *)
module F5 () = struct end;;
module No_warn = F5 (struct end [@warning "-73"])

[%%expect{|
module F5 : () -> sig end
module No_warn : sig end
|}]
