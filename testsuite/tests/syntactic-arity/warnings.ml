(* TEST
expect;
*)

(* Locations of type errors when there is an error
 * partway through checking type parameters. *)

module type S = sig
  type t
  type _ u = t -> t
end

let f (module M : S) = ((fun z -> z) : _ M.u);;
[%%expect {|
module type S = sig type t type _ u = t -> t end
Line 6, characters 23-45:
6 | let f (module M : S) = ((fun z -> z) : _ M.u);;
                           ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) y = ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 25-47:
1 | let f (module M : S) y = ((fun z -> z) : _ M.u);;
                             ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) y : _ = ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 29-51:
1 | let f (module M : S) y : _ = ((fun z -> z) : _ M.u);;
                                 ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 21-54:
1 | let f (module M : S) (type a) = ((fun z -> z) : a M.u);;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) (type a) x = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 21-56:
1 | let f (module M : S) (type a) x = ((fun z -> z) : a M.u);;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a -> M.t -> M.t"
       but an expression was expected of type "'b"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) x (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 23-56:
1 | let f (module M : S) x (type a) = ((fun z -> z) : a M.u);;
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) x (type a) :> a M.u = function z -> z
[%%expect {|
Line 1, characters 23-58:
1 | let f (module M : S) x (type a) :> a M.u = function z -> z
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f (module M : S) x (type a) : a M.u = function z -> z
[%%expect {|
Line 1, characters 23-57:
1 | let f (module M : S) x (type a) : a M.u = function z -> z
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;
