(* TEST
  * expect
*)



module type a
module type b
module type c

module type x = sig type x end
module type y = sig type y end
module type z = sig type z end


module type empty = sig end

module Empty = struct end
module X: x = struct type x end
module Y: y = struct type y end
module Z: z = struct type z end
module F(X:x)(Y:y)(Z:z) = struct end
[%%expect {|
module type a
module type b
module type c
module type x = sig type x end
module type y = sig type y end
module type z = sig type z end
module type empty = sig end
module Empty : sig end
module X : x
module Y : y
module Z : z
module F : functor (X : x) (Y : y) (Z : z) -> sig end
|}]


module M = F(X)(Z)
[%%expect {|
Line 1, characters 11-18:
1 | module M = F(X)(Z)
               ^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         X  Z
       do not match these parameters:
         functor (X : x) (Y : y) (Z : z) -> ...
  1. Module X matches the expected module type x
  2. An argument appears to be missing with module type y
  3. Module Z matches the expected module type z
|}]

module type f = functor (X:empty)(Y:empty) -> empty
module F: f =
  functor(X:empty)(Y:empty)(Z:empty) -> Empty
[%%expect {|
module type f = functor (X : empty) (Y : empty) -> empty
Line 3, characters 2-45:
3 |   functor(X:empty)(Y:empty)(Z:empty) -> Empty
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : empty) (Y : empty) (Z : empty) -> ...
       is not included in
         functor (X : empty) (Y : empty)  -> ...
  1. Module types empty and empty match
  2. Module types empty and empty match
  3. An extra argument is provided of module type empty
|}]

module type f = functor (X:a)(Y:b) -> c
module F:f = functor (X:a)(Y:b)(Z:c) -> Empty
[%%expect {|
module type f = functor (X : a) (Y : b) -> c
Line 2, characters 13-45:
2 | module F:f = functor (X:a)(Y:b)(Z:c) -> Empty
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : a) (Y : b) (Z : c) -> ...
       is not included in
         functor (X : a) (Y : b)  -> ...
  1. Module types a and a match
  2. Module types b and b match
  3. An extra argument is provided of module type c
|}]

module M : sig module F: functor (X:sig end) -> sig end end =
  struct
    module F(X:sig type t end) = struct end
  end
[%%expect {|
Lines 2-4, characters 2-5:
2 | ..struct
3 |     module F(X:sig type t end) = struct end
4 |   end
Error: Signature mismatch:
       Modules do not match:
         sig module F : functor (X : sig type t end) -> sig end end
       is not included in
         sig module F : functor (X : sig end) -> sig end end
       In module F:
       Modules do not match:
         functor (X : ...(X)) -> ...
       is not included in
         functor (X : sig end) -> ...
     Module types do not match:
       ...(X) = sig type t end
     does not include
       sig end
     The type `t' is required but not provided
|}]

module F(X:sig type t end) = struct end
module M = F(struct type x end)
[%%expect {|
module F : functor (X : sig type t end) -> sig end
Line 2, characters 11-31:
2 | module M = F(struct type x end)
               ^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         ...(S1)
       do not match these parameters:
         functor (X : ...(X)) -> ...
     Modules do not match:
       ...(S1) = struct type x end
     is not included in
       ...(X) = sig type t end
     The type `t' is required but not provided
|}]

module F(X:sig type x end)(Y:sig type y end)(Z:sig type z end) = struct
    type t = X of X.x | Y of Y.y | Z of Z.z
end
type u = F(X)(Z).t
[%%expect {|
module F :
  functor (X : sig type x end) (Y : sig type y end) (Z : sig type z end) ->
    sig type t = X of X.x | Y of Y.y | Z of Z.z end
Line 4, characters 9-18:
4 | type u = F(X)(Z).t
             ^^^^^^^^^
Error: The functor application F(X)(Z) is ill-typed.
       These arguments:
         X  Z
       do not match these parameters:
         functor (X : ...) (Y : ...(Y)) (Z : ...) -> ...
  1. Module X matches the expected module type
  2. An argument appears to be missing with module type
         ...(Y) = sig type y end
  3. Module Z matches the expected module type
|}]

module F()(X:sig type t end) = struct end
module M = F()()
[%%expect {|
module F : functor () (X : sig type t end) -> sig end
Line 2, characters 11-16:
2 | module M = F()()
               ^^^^^
Error: The functor application is ill-typed.
       These arguments:
         () ()
       do not match these parameters:
         functor () (X : ...(X)) -> ...
  1. Module () matches the expected module type
  2. the functor was expected to be applicative at this position
|}]

module M: sig
  module F: functor(X:sig type x end)(X:sig type y end) -> sig end
end = struct
 module F(X:sig type y end) = struct end
end
[%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |  module F(X:sig type y end) = struct end
5 | end
Error: Signature mismatch:
       Modules do not match:
         sig module F : functor (X : sig type y end) -> sig end end
       is not included in
         sig
           module F :
             functor (X : sig type x end) (X : sig type y end) -> sig end
         end
       In module F:
       Modules do not match:
         functor  (X : ...(X)) -> ...
       is not included in
         functor (X : ...(X)) (X : ...(X)) -> ...
  1. An argument appears to be missing with module type
         ...(X) = sig type x end
  2. Module types ...(X) and ...(X) match
|}]


module F(Ctx: sig
  module type t
  module type u
  module X:t
  module Y:u
end) = struct
  open Ctx
  module F(A:t)(B:u) = struct end
  module M = F(Y)(X)
end
[%%expect {|
Line 9, characters 13-20:
9 |   module M = F(Y)(X)
                 ^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         Ctx.Y Ctx.X
       do not match these parameters:
         functor (A : Ctx.t) (B : Ctx.u) -> ...
  1. Modules do not match: Ctx.Y : Ctx.u is not included in Ctx.t
  2. Modules do not match: Ctx.X : Ctx.t is not included in Ctx.u
|}]



(** Dependent types *)
(** Application side *)

module F
    (A:sig type x type y end)
    (B:sig type x = A.x end)
    (C:sig type y = A.y end)
= struct end
module K = struct include X include Y end
module M = F(K)(struct type x = K.x end)( (* struct type z = K.y end *) )
[%%expect {|
module F :
  functor (A : sig type x type y end) (B : sig type x = A.x end)
    (C : sig type y = A.y end) -> sig end
module K : sig type x = X.x type y = Y.y end
Line 10, characters 11-73:
10 | module M = F(K)(struct type x = K.x end)( (* struct type z = K.y end *) )
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         K ...(S2) ()
       do not match these parameters:
         functor (A : ...) (B : ...) (C : ...(C)) -> ...
  1. Module K matches the expected module type
  2. Module ...(S2) matches the expected module type
  3. the functor was expected to be applicative at this position
|}]

module M = F(K)(struct type y = K.y end)
[%%expect {|
Line 1, characters 11-40:
1 | module M = F(K)(struct type y = K.y end)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         K  ...(S3)
       do not match these parameters:
         functor (A : ...) (B : ...(B)) (C : ...) -> ...
  1. Module K matches the expected module type
  2. An argument appears to be missing with module type
         ...(B) = sig type x = A.x end
  3. Module ...(S3) matches the expected module type
|}]


(** FIXME *)
module M =
  F
    (struct include X include Y end)
    (struct type x = K.x end)
    (struct type yy = K.y end)
[%%expect {|
Lines 2-5, characters 2-30:
2 | ..F
3 |     (struct include X include Y end)
4 |     (struct type x = K.x end)
5 |     (struct type yy = K.y end)
Error: The functor application is ill-typed.
       These arguments:
         ...(S1) ...(S2) ...(S3)
       do not match these parameters:
         functor (A : ...) (B : ...) (C : ...(C)) -> ...
  1. Module ...(S1) matches the expected module type
  2. Module ...(S2) matches the expected module type
  3. Modules do not match:
       ...(S3) = struct type yy = K.y end
     is not included in
       ...(C) = sig type y = A.y end
     The type `y' is required but not provided
|}]

(** Inclusion side *)
module type f =
  functor(A:sig type x type y end)(B:sig type x = A.x end)(C:sig type y = A.y end)
    -> sig end
module F: f = functor (A:sig include x include y end)(Z:sig type y = A.y end) -> struct end
[%%expect {|
module type f =
  functor (A : sig type x type y end) (B : sig type x = A.x end)
    (C : sig type y = A.y end) -> sig end
Line 4, characters 14-91:
4 | module F: f = functor (A:sig include x include y end)(Z:sig type y = A.y end) -> struct end
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (A : ...(A))  (Z : ...(Z)) -> ...
       is not included in
         functor (A : ...(A)) (B : ...(B)) (C : ...(C)) -> ...
  1. Module types ...(A) and ...(A) match
  2. An argument appears to be missing with module type
         ...(B) = sig type x = A.x end
  3. Module types ...(Z) and ...(C) match
|}]


module type f =
  functor(B:sig type x type y type u=x type v=y end)(Y:sig type yu = Y of B.u end)(Z:sig type zv = Z of B.v end)
    -> sig end
module F: f = functor (X:sig include x include y end)(Z:sig type zv = Z of X.y end) -> struct end
[%%expect {|
module type f =
  functor (B : sig type x type y type u = x type v = y end)
    (Y : sig type yu = Y of B.u end) (Z : sig type zv = Z of B.v end) ->
    sig end
Line 4, characters 14-97:
4 | module F: f = functor (X:sig include x include y end)(Z:sig type zv = Z of X.y end) -> struct end
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : ...(X))  (Z : ...(Z)) -> ...
       is not included in
         functor (B : ...(B)) (Y : ...(Y)) (Z : ...(Z)) -> ...
  1. Module types ...(X) and ...(B) match
  2. An argument appears to be missing with module type
         ...(Y) = sig type yu = Y of B.u end
  3. Module types ...(Z) and ...(Z) match
|}]


(** Module type equalities *)

module M: sig
  module type S = sig type t end
end = struct
  module type S = sig type s type t end
end;;
[%%expect {|
Lines 5-7, characters 6-3:
5 | ......struct
6 |   module type S = sig type s type t end
7 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig type s type t end end
       is not included in
         sig module type S = sig type t end end
       Module type declarations do not match:
         module type S = sig type s type t end
       does not match
         module type S = sig type t end
       The second module type is not included in the first
       At position module type S = <here>
       Module types do not match:
         sig type t end
       is not equal to
         sig type s type t end
       At position module type S = <here>
       The type `s' is required but not provided
|}]

module M: sig
  module type S = sig type t type u end
end = struct
  module type S = sig type t end
end;;
  [%%expect {|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   module type S = sig type t end
5 | end..
Error: Signature mismatch:
       Modules do not match:
         sig module type S = sig type t end end
       is not included in
         sig module type S = sig type t type u end end
       Module type declarations do not match:
         module type S = sig type t end
       does not match
         module type S = sig type t type u end
       The first module type is not included in the second
       At position module type S = <here>
       Module types do not match:
         sig type t end
       is not equal to
         sig type t type u end
       At position module type S = <here>
       The type `u' is required but not provided
|}]


(** Name collision test *)

module F(X:x)(B:b)(Y:y) = struct type t end
module M = struct
  module type b
  module G(P: sig module B:b end) = struct
    open P
    module U = F(struct type x end)(B)(struct type w end)
  end
end
[%%expect {|
module F : functor (X : x) (B : b) (Y : y) -> sig type t end
Line 8, characters 15-57:
8 |     module U = F(struct type x end)(B)(struct type w end)
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         ...(S1) P.B ...(S3)
       do not match these parameters:
         functor (X : x) (B : b) (Y : y) -> ...
  1. Module ...(S1) matches the expected module type x
  2. Modules do not match:
       P.B : b/1
     is not included in
       b/2
      Line 5, characters 2-15:
        Definition of module type b/1
      Line 2, characters 0-13:
        Definition of module type b/2
  3. Modules do not match:
       ...(S3) = struct type w end
     is not included in
       y
     The type `y' is required but not provided
|}]

module F(X:a) = struct type t end
module M = struct
  module type a
  module G(P: sig module X:a end) = struct
    open P
    type t = F(X).t
  end
end
[%%expect {|
module F : functor (X : a) -> sig type t end
Line 6, characters 13-19:
6 |     type t = F(X).t
                 ^^^^^^
Error: The functor application F(X) is ill-typed.
       These arguments:
         P.X
       do not match these parameters:
         functor (X : a/2) -> ...
     Modules do not match:
       P.X : a/1
     is not included in
       a/2
      Line 3, characters 2-15:
        Definition of module type a/1
      Line 1, characters 0-13:
        Definition of module type a/2
|}]



module M: sig module F: functor(X:a)(Y:a) -> sig end end =
 struct
  module type aa = a
  module type a
  module F(X:aa)(Y:a) = struct end
end
[%%expect {|
Lines 2-6, characters 1-3:
2 | .struct
3 |   module type aa = a
4 |   module type a
5 |   module F(X:aa)(Y:a) = struct end
6 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module type aa = a
           module type a
           module F : functor (X : aa) (Y : a) -> sig end
         end
       is not included in
         sig module F : functor (X : a) (Y : a) -> sig end end
       In module F:
       Modules do not match:
         functor (X : aa) (Y : a/1) -> ...
       is not included in
         functor (X : a/2) (Y : a/2) -> ...
  1. Module types aa and a/2 match
  2. Module types do not match:
       a/1
     does not include
       a/2
      Line 4, characters 2-15:
        Definition of module type a/1
      Line 1, characters 0-13:
        Definition of module type a/2
|}]

module X: functor ( X: sig end) -> sig end = functor(X: Set.OrderedType) -> struct end
[%%expect {|
Line 1, characters 45-86:
1 | module X: functor ( X: sig end) -> sig end = functor(X: Set.OrderedType) -> struct end
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : Set.OrderedType) -> ...
       is not included in
         functor (X : sig end) -> ...
     Module types do not match:
       Set.OrderedType
     does not include
       sig end
     The type `t' is required but not provided
     File "set.mli", line 49, characters 4-10: Expected declaration
     The value `compare' is required but not provided
     File "set.mli", line 52, characters 4-31: Expected declaration
|}]

(** Deeply nested errors *)


module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type ybb end) -> sig end
      )
      (Z:
         functor (A: sig type za end)(B:sig type zbb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 15-27, characters 6-3:
15 | ......struct
16 |   module F
17 |       (X:
18 |          functor (A: sig type xa end)(B:sig type xz end) -> sig end
19 |       )
...
24 |          functor (A: sig type za end)(B:sig type zbb end) -> sig end
25 |       )
26 |   = struct end
27 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor
               (X : functor (A : sig type xa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type ybb end) ->
                      sig end)
               (Z : functor (A : sig type za end) (B : sig type zbb end) ->
                      sig end)
               -> sig end
         end
       is not included in
         sig
           module F :
             functor
               (X : functor (A : sig type xa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type yb end) ->
                      sig end)
               (Z : functor (A : sig type za end) (B : sig type zb end) ->
                      sig end)
               -> sig end
         end
       In module F:
       Modules do not match:
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) -> ...
       is not included in
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) -> ...
  1. Module types ...(X) and ...(X) match
  2. Module types do not match:
       ...(Y) =
       functor (A : sig type ya end) (B : sig type ybb end) -> sig end
     does not include
       ...(Y) =
       functor (A : sig type ya end) (B : sig type yb end) -> sig end
     Modules do not match:
       functor (A : ...(A)) (B : ...(B)) -> ...
     is not included in
       functor (A : ...(A)) (B : ...(B)) -> ...
     1. Module types ...(A) and ...(A) match
     2. Module types do not match:
          ...(B) = sig type yb end
        does not include
          ...(B) = sig type ybb end
        The type `yb' is required but not provided
  3. Module types do not match:
       ...(Z) =
       functor (A : sig type za end) (B : sig type zbb end) -> sig end
     does not include
       ...(Z) =
       functor (A : sig type za end) (B : sig type zb end) -> sig end
     Modules do not match:
       functor (A : ...(A)) (B : ...(B)) -> ...
     is not included in
       functor (A : ...(A)) (B : ...(B)) -> ...
|}]


module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type yb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 12-21, characters 6-3:
12 | ......struct
13 |   module F
14 |       (X:
15 |          functor (A: sig type xa end)(B:sig type xz end) -> sig end
16 |       )
17 |       (Y:
18 |          functor (A: sig type ya end)(B:sig type yb end) -> sig end
19 |       )
20 |   = struct end
21 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor
               (X : functor (A : sig type xa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type yb end) ->
                      sig end)
               -> sig end
         end
       is not included in
         sig
           module F :
             functor
               (X : functor (A : sig type xa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type yb end) ->
                      sig end)
               (Z : functor (A : sig type za end) (B : sig type zb end) ->
                      sig end)
               -> sig end
         end
       In module F:
       Modules do not match:
         functor (X : ...(X)) (Y : ...(Y))  -> ...
       is not included in
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) -> ...
  1. Module types ...(X) and ...(X) match
  2. Module types ...(Y) and ...(Y) match
  3. An argument appears to be missing with module type
         ...(Z) =
         functor (A : sig type za end) (B : sig type zb end) -> sig end
|}]

module M: sig
  module F: functor
      (X:
         functor(A: sig type xa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor(A: sig type ya end)(B:sig type yb end) -> sig end
      )
      (Z:
         functor(A: sig type za end)(B:sig type zb end) -> sig end
      ) -> sig end
end = struct
  module F
      (X:
         functor (A: sig type xaa end)(B:sig type xz end) -> sig end
      )
      (Y:
         functor (A: sig type ya end)(B:sig type ybb end) -> sig end
      )
      (Z:
         functor (A: sig type za end)(B:sig type zbb end) -> sig end
      )
  = struct end
end
[%%expect {|
Lines 12-24, characters 6-3:
12 | ......struct
13 |   module F
14 |       (X:
15 |          functor (A: sig type xaa end)(B:sig type xz end) -> sig end
16 |       )
...
21 |          functor (A: sig type za end)(B:sig type zbb end) -> sig end
22 |       )
23 |   = struct end
24 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor
               (X : functor (A : sig type xaa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type ybb end) ->
                      sig end)
               (Z : functor (A : sig type za end) (B : sig type zbb end) ->
                      sig end)
               -> sig end
         end
       is not included in
         sig
           module F :
             functor
               (X : functor (A : sig type xa end) (B : sig type xz end) ->
                      sig end)
               (Y : functor (A : sig type ya end) (B : sig type yb end) ->
                      sig end)
               (Z : functor (A : sig type za end) (B : sig type zb end) ->
                      sig end)
               -> sig end
         end
       In module F:
       Modules do not match:
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) -> ...
       is not included in
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) -> ...
  1. Module types do not match:
       ...(X) =
       functor (A : sig type xaa end) (B : sig type xz end) -> sig end
     does not include
       ...(X) =
       functor (A : sig type xa end) (B : sig type xz end) -> sig end
     Modules do not match:
       functor (A : ...(A)) (B : ...(B)) -> ...
     is not included in
       functor (A : ...(A)) (B : ...(B)) -> ...
     1. Module types do not match:
          ...(A) = sig type xa end
        does not include
          ...(A) = sig type xaa end
        The type `xa' is required but not provided
     2. Module types ...(B) and ...(B) match
  2. Module types do not match:
       ...(Y) =
       functor (A : sig type ya end) (B : sig type ybb end) -> sig end
     does not include
       ...(Y) =
       functor (A : sig type ya end) (B : sig type yb end) -> sig end
     Modules do not match:
       functor (A : ...(A)) (B : ...(B)) -> ...
     is not included in
       functor (A : ...(A)) (B : ...(B)) -> ...
  3. Module types do not match:
       ...(Z) =
       functor (A : sig type za end) (B : sig type zbb end) -> sig end
     does not include
       ...(Z) =
       functor (A : sig type za end) (B : sig type zb end) -> sig end
     Modules do not match:
       functor (A : ...(A)) (B : ...(B)) -> ...
     is not included in
       functor (A : ...(A)) (B : ...(B)) -> ...
|}]

module A: sig
  module B: sig
    module C: sig
      module D: sig
        module E: sig
          module F: sig type x end -> sig type y end
          -> sig type z end -> sig type w end -> sig end
        end
      end
    end
  end
end = struct
  module B = struct
    module C = struct
      module D = struct
        module E = struct
          module F(X:sig type x end)(Y:sig type y' end)
            (W:sig type w end) = struct end
        end
      end
    end
  end
end
[%%expect {|
Lines 12-23, characters 6-3:
12 | ......struct
13 |   module B = struct
14 |     module C = struct
15 |       module D = struct
16 |         module E = struct
...
20 |       end
21 |     end
22 |   end
23 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module B :
             sig
               module C :
                 sig
                   module D :
                     sig
                       module E :
                         sig
                           module F :
                             functor (X : sig type x end)
                               (Y : sig type y' end) (W : sig type w end) ->
                               sig end
                         end
                     end
                 end
             end
         end
       is not included in
         sig
           module B :
             sig
               module C :
                 sig
                   module D :
                     sig
                       module E :
                         sig
                           module F :
                             sig type x end -> sig type y end ->
                               sig type z end -> sig type w end -> sig end
                         end
                     end
                 end
             end
         end
       In module B:
       Modules do not match:
         sig module C = B.C end
       is not included in
         sig
           module C :
             sig
               module D :
                 sig
                   module E :
                     sig
                       module F :
                         sig type x end -> sig type y end ->
                           sig type z end -> sig type w end -> sig end
                     end
                 end
             end
         end
       In module B.C:
       Modules do not match:
         sig module D = B.C.D end
       is not included in
         sig
           module D :
             sig
               module E :
                 sig
                   module F :
                     sig type x end -> sig type y end -> sig type z end ->
                       sig type w end -> sig end
                 end
             end
         end
       In module B.C.D:
       Modules do not match:
         sig module E = B.C.D.E end
       is not included in
         sig
           module E :
             sig
               module F :
                 sig type x end -> sig type y end -> sig type z end ->
                   sig type w end -> sig end
             end
         end
       In module B.C.D.E:
       Modules do not match:
         sig module F = B.C.D.E.F end
       is not included in
         sig
           module F :
             sig type x end -> sig type y end -> sig type z end ->
               sig type w end -> sig end
         end
       In module B.C.D.E.F:
       Modules do not match:
         functor (X : ...(X)) (Y : ...(Y))  (W : ...(W)) -> ...
       is not included in
         functor ...(T1) ...(T2) ...(T3) ...(T4) -> ...
  1. Module types ...(X) and ...(T1) match
  2. Module types do not match:
       ...(Y) = sig type y' end
     does not include
       ...(T2) = sig type y end
     The type `y'' is required but not provided
  3. An argument appears to be missing with module type
         ...(T3) = sig type z end
  4. Module types ...(W) and ...(T4) match
|}]


(** Ugly cases *)

module type Arg = sig
    module type A
    module type Honorificabilitudinitatibus
    module X:   Honorificabilitudinitatibus
    module Y:   A
end

module F(A:Arg)
= struct
  open A
  module G(X:A)(Y:A)(_:A)(Z:A) = struct end
  type u = G(X)(Y)(X)(Y)(X).t
end;;
[%%expect {|
module type Arg =
  sig
    module type A
    module type Honorificabilitudinitatibus
    module X : Honorificabilitudinitatibus
    module Y : A
  end
Line 14, characters 11-29:
14 |   type u = G(X)(Y)(X)(Y)(X).t
                ^^^^^^^^^^^^^^^^^^
Error: The functor application G(X)(Y)(X)(Y)(X) is ill-typed.
       These arguments:
         A.X A.Y A.X A.Y A.X
       do not match these parameters:
         functor (X : A.A) (Y : A.A)  A.A (Z : A.A) -> ...
  1. Modules do not match:
       A.X : A.Honorificabilitudinitatibus
     is not included in
       A.A

  2. Module A.Y matches the expected module type A.A
  3. The following extra argument is provided
         A.X : A.Honorificabilitudinitatibus
  4. Module A.Y matches the expected module type A.A
  5. Modules do not match:
       A.X : A.Honorificabilitudinitatibus
     is not included in
       A.A

|}]


module type s = functor
  (X: sig type when_ type shall type we type three type meet type again end)
  (Y:sig type in_ val thunder:in_ val lightning: in_ type rain end)
  (Z:sig type when_ type the type hurlyburly's type done_  end)
  (Z:sig type when_ type the type battle's type lost type and_ type won end)
  (W:sig type that type will type be type ere type the_ type set type of_ type sun end)
  (S: sig type where type the type place end)
  (R: sig type upon type the type heath end)
-> sig end
module F: s = functor
  (X: sig type when_ type shall type we type tree type meet type again end)
  (Y:sig type in_ val thunder:in_ val lightning: in_ type pain end)
  (Z:sig type when_ type the type hurlyburly's type gone  end)
  (Z:sig type when_ type the type battle's type last type and_ type won end)
  (W:sig type that type will type be type the type era type set type of_ type sun end)
  (S: sig type where type the type lace end)
  (R: sig type upon type the type heart end)
  -> struct end
[%%expect {|
module type s =
  functor
    (X : sig
           type when_
           type shall
           type we
           type three
           type meet
           type again
         end)
    (Y : sig type in_ val thunder : in_ val lightning : in_ type rain end)
    (Z : sig type when_ type the type hurlyburly's type done_ end)
    (Z : sig
           type when_
           type the
           type battle's
           type lost
           type and_
           type won
         end)
    (W : sig
           type that
           type will
           type be
           type ere
           type the_
           type set
           type of_
           type sun
         end)
    (S : sig type where type the type place end)
    (R : sig type upon type the type heath end) -> sig end
Lines 10-18, characters 14-15:
10 | ..............functor
11 |   (X: sig type when_ type shall type we type tree type meet type again end)
12 |   (Y:sig type in_ val thunder:in_ val lightning: in_ type pain end)
13 |   (Z:sig type when_ type the type hurlyburly's type gone  end)
14 |   (Z:sig type when_ type the type battle's type last type and_ type won end)
15 |   (W:sig type that type will type be type the type era type set type of_ type sun end)
16 |   (S: sig type where type the type lace end)
17 |   (R: sig type upon type the type heart end)
18 |   -> struct end
Error: Signature mismatch:
       Modules do not match:
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) (Z : ...(Z))
         (W : ...(W)) (S : ...(S)) (R : ...(R)) -> ...
       is not included in
         functor (X : ...(X)) (Y : ...(Y)) (Z : ...(Z)) (Z : ...(Z))
         (W : ...(W)) (S : ...(S)) (R : ...(R)) -> ...
  1. Module types do not match:
       ...(X) =
       sig type when_ type shall type we type tree type meet type again end
     does not include
       ...(X) =
       sig type when_ type shall type we type three type meet type again end
     The type `tree' is required but not provided
  2. Module types do not match:
       ...(Y) =
       sig type in_ val thunder : in_ val lightning : in_ type pain end
     does not include
       ...(Y) =
       sig type in_ val thunder : in_ val lightning : in_ type rain end
     The type `pain' is required but not provided
  3. Module types do not match:
       ...(Z) = sig type when_ type the type hurlyburly's type gone end
     does not include
       ...(Z) = sig type when_ type the type hurlyburly's type done_ end
     The type `gone' is required but not provided
  4. Module types do not match:
       ...(Z) =
       sig type when_ type the type battle's type last type and_ type won end
     does not include
       ...(Z) =
       sig type when_ type the type battle's type lost type and_ type won end
     The type `last' is required but not provided
  5. Module types do not match:
       ...(W) =
       sig
         type that
         type will
         type be
         type the
         type era
         type set
         type of_
         type sun
       end
     does not include
       ...(W) =
       sig
         type that
         type will
         type be
         type ere
         type the_
         type set
         type of_
         type sun
       end
     The type `the' is required but not provided
     The type `era' is required but not provided
  6. Module types do not match:
       ...(S) = sig type where type the type lace end
     does not include
       ...(S) = sig type where type the type place end
     The type `lace' is required but not provided
  7. Module types do not match:
       ...(R) = sig type upon type the type heart end
     does not include
       ...(R) = sig type upon type the type heath end
     The type `heart' is required but not provided
|}]
