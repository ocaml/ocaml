(* TEST
expect;
*)

(* Locations of type errors when there is an error
 * partway through checking type parameters. *)

module type S = sig
  type t
  type _ u = t -> t
end

let f ((module M) : (module S)) = ((fun z -> z) : _ M.u);;
[%%expect {|
module type S = sig type t type _ u = t -> t end
Line 6, characters 34-56:
6 | let f ((module M) : (module S)) = ((fun z -> z) : _ M.u);;
                                      ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) y = ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 36-58:
1 | let f ((module M) : (module S)) y = ((fun z -> z) : _ M.u);;
                                        ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) y : _ = ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 40-62:
1 | let f ((module M) : (module S)) y : _ = ((fun z -> z) : _ M.u);;
                                            ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 32-65:
1 | let f ((module M) : (module S)) (type a) = ((fun z -> z) : a M.u);;
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) (type a) x = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 32-67:
1 | let f ((module M) : (module S)) (type a) x = ((fun z -> z) : a M.u);;
                                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a -> M.t -> M.t"
       but an expression was expected of type "'b"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) x (type a) = ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 34-67:
1 | let f ((module M) : (module S)) x (type a) = ((fun z -> z) : a M.u);;
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) x (type a) :> a M.u = function z -> z
[%%expect {|
Line 1, characters 34-69:
1 | let f ((module M) : (module S)) x (type a) :> a M.u = function z -> z
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f ((module M) : (module S)) x (type a) : a M.u = function z -> z
[%%expect {|
Line 1, characters 34-68:
1 | let f ((module M) : (module S)) x (type a) : a M.u = function z -> z
                                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;
