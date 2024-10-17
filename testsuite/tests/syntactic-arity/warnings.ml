(* TEST
expect;
*)

(* Locations of type errors when there is an error
 * partway through checking type parameters. *)

module type S = sig
  type t
  type _ u = t -> t
end

let f : _ -> _ = fun (module M : S) -> ((fun z -> z) : _ M.u);;
[%%expect {|
module type S = sig type t type _ u = t -> t end
Line 6, characters 39-61:
6 | let f : _ -> _ = fun (module M : S) -> ((fun z -> z) : _ M.u);;
                                           ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) y -> ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 41-63:
1 | let f : _ -> _ = fun (module M : S) y -> ((fun z -> z) : _ M.u);;
                                             ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) y : _ -> ((fun z -> z) : _ M.u);;
[%%expect {|
Line 1, characters 45-67:
1 | let f : _ -> _ = fun (module M : S) y : _ -> ((fun z -> z) : _ M.u);;
                                                 ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) (type a) -> ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 36-70:
1 | let f : _ -> _ = fun (module M : S) (type a) -> ((fun z -> z) : a M.u);;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) (type a) x -> ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 36-72:
1 | let f : _ -> _ = fun (module M : S) (type a) x -> ((fun z -> z) : a M.u);;
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a -> M.t -> M.t"
       but an expression was expected of type "'b"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) x (type a) -> ((fun z -> z) : a M.u);;
[%%expect {|
Line 1, characters 38-72:
1 | let f : _ -> _ = fun (module M : S) x (type a) -> ((fun z -> z) : a M.u);;
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) x (type a) -> ((function z -> z) :> a M.u)
[%%expect {|
Line 1, characters 38-78:
1 | let f : _ -> _ = fun (module M : S) x (type a) -> ((function z -> z) :> a M.u)
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;

let f : _ -> _ = fun (module M : S) x (type a) : a M.u -> function z -> z
[%%expect {|
Line 1, characters 38-73:
1 | let f : _ -> _ = fun (module M : S) x (type a) : a M.u -> function z -> z
                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "M.t -> M.t"
       but an expression was expected of type "'a"
       The type constructor "M.t" would escape its scope
|}];;
