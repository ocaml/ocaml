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
         X Z
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
Line 3, characters 9-45:
3 |   functor(X:empty)(Y:empty)(Z:empty) -> Empty
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : empty) (Y : empty) (Z : empty) -> ...
       is not included in
         functor (X : empty) (Y : empty) -> ...
       1. Module types empty and empty match
       2. Module types empty and empty match
       3. An extra argument is provided of module type empty
|}]

module type f = functor (X:a)(Y:b) -> c
module F:f = functor (X:a)(Y:b)(Z:c) -> Empty
[%%expect {|
module type f = functor (X : a) (Y : b) -> c
Line 2, characters 21-45:
2 | module F:f = functor (X:a)(Y:b)(Z:c) -> Empty
                         ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : a) (Y : b) (Z : c) -> ...
       is not included in
         functor (X : a) (Y : b) -> ...
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
         functor (X : $S1) -> ...
       is not included in
         functor (X : sig end) -> ...
       Module types do not match:
         $S1 = sig type t end
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
Error: Modules do not match: sig type x end is not included in sig type t end
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
         X Z
       do not match these parameters:
         functor (X : ...) (Y : $T2) (Z : ...) -> ...
       1. Module X matches the expected module type
       2. An argument appears to be missing with module type
              $T2 = sig type y end
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
         functor () (X : $T2) -> ...
       1. Module () matches the expected module type
       2. The functor was expected to be applicative at this position
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
         functor (X : $S2) -> ...
       is not included in
         functor (X : $T1) (X : $T2) -> ...
       1. An argument appears to be missing with module type
              $T1 = sig type x end
       2. Module types $S2 and $T2 match
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

(** Too many arguments *)
module Ord = struct type t = unit let compare _ _ = 0 end
module M = Map.Make(Ord)(Ord)
[%%expect {|
module Ord : sig type t = unit val compare : 'a -> 'b -> int end
Line 2, characters 11-29:
2 | module M = Map.Make(Ord)(Ord)
               ^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         Ord Ord
       do not match these parameters:
         functor (Ord : Map.OrderedType) -> ...
       1. The following extra argument is provided
              Ord : sig type t = unit val compare : 'a -> 'b -> int end
       2. Module Ord matches the expected module type Map.OrderedType
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
         K $S2 ()
       do not match these parameters:
         functor (A : ...) (B : ...) (C : $T3) -> ...
       1. Module K matches the expected module type
       2. Module $S2 matches the expected module type
       3. The functor was expected to be applicative at this position
|}]

module M = F(K)(struct type y = K.y end)
[%%expect {|
Line 1, characters 11-40:
1 | module M = F(K)(struct type y = K.y end)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         K $S3
       do not match these parameters:
         functor (A : ...) (B : $T2) (C : ...) -> ...
       1. Module K matches the expected module type
       2. An argument appears to be missing with module type
              $T2 = sig type x = A.x end
       3. Module $S3 matches the expected module type
|}]


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
         $S1 $S2 $S3
       do not match these parameters:
         functor (A : ...) (B : ...) (C : $T3) -> ...
       1. Module $S1 matches the expected module type
       2. Module $S2 matches the expected module type
       3. Modules do not match:
            $S3 : sig type yy = K.y end
          is not included in
            $T3 = sig type y = A.y end
          The type `y' is required but not provided
|}]


module M = struct
  module N = struct
    type x
    type y
  end
end


module Defs = struct
  module X = struct type x = M.N.x end
  module Y = struct type y = M.N.y end
end
module Missing_X = F(M.N)(Defs.Y)
[%%expect {|
module M : sig module N : sig type x type y end end
module Defs :
  sig module X : sig type x = M.N.x end module Y : sig type y = M.N.y end end
Line 13, characters 19-33:
13 | module Missing_X = F(M.N)(Defs.Y)
                        ^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         M.N Defs.Y
       do not match these parameters:
         functor (A : ...) (B : $T2) (C : ...) -> ...
       1. Module M.N matches the expected module type
       2. An argument appears to be missing with module type
              $T2 = sig type x = A.x end
       3. Module Defs.Y matches the expected module type
|}]

module Too_many_Xs = F(M.N)(Defs.X)(Defs.X)(Defs.Y)
[%%expect {|
Line 1, characters 21-51:
1 | module Too_many_Xs = F(M.N)(Defs.X)(Defs.X)(Defs.Y)
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         M.N Defs.X Defs.X Defs.Y
       do not match these parameters:
         functor (A : ...) (B : ...) (C : ...) -> ...
       1. Module M.N matches the expected module type
       2. The following extra argument is provided
              Defs.X : sig type x = M.N.x end
       3. Module Defs.X matches the expected module type
       4. Module Defs.Y matches the expected module type
|}]


module X = struct type x = int end
module Y = struct type y = float end
module Missing_X_bis = F(struct type x = int type y = float end)(Y)
[%%expect {|
module X : sig type x = int end
module Y : sig type y = float end
Line 3, characters 23-67:
3 | module Missing_X_bis = F(struct type x = int type y = float end)(Y)
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         $S1 Y
       do not match these parameters:
         functor (A : ...) (B : $T2) (C : ...) -> ...
       1. Module $S1 matches the expected module type
       2. An argument appears to be missing with module type
              $T2 = sig type x = A.x end
       3. Module Y matches the expected module type
|}]

module Too_many_Xs_bis = F(struct type x = int type y = float end)(X)(X)(Y)
[%%expect {|
Line 1, characters 25-75:
1 | module Too_many_Xs_bis = F(struct type x = int type y = float end)(X)(X)(Y)
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         $S1 X X Y
       do not match these parameters:
         functor (A : ...) (B : ...) (C : ...) -> ...
       1. Module $S1 matches the expected module type
       2. The following extra argument is provided X : sig type x = int end
       3. Module X matches the expected module type
       4. Module Y matches the expected module type
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
Line 4, characters 22-91:
4 | module F: f = functor (A:sig include x include y end)(Z:sig type y = A.y end) -> struct end
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (A : $S1) (Z : $S3) -> ...
       is not included in
         functor (A : $T1) (B : $T2) (C : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. An argument appears to be missing with module type
              $T2 = sig type x = A.x end
       3. Module types $S3 and $T3 match
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
Line 4, characters 22-97:
4 | module F: f = functor (X:sig include x include y end)(Z:sig type zv = Z of X.y end) -> struct end
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Signature mismatch:
       Modules do not match:
         functor (X : $S1) (Z : $S3) -> ...
       is not included in
         functor (B : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. An argument appears to be missing with module type
              $T2 = sig type yu = Y of B.u end
       3. Module types $S3 and $T3 match
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
         $S1 P.B $S3
       do not match these parameters:
         functor (X : x) (B : b/2) (Y : y) -> ...
       1. Module $S1 matches the expected module type x
       2. Modules do not match:
            P.B : b/1
          is not included in
            b/2
          Line 5, characters 2-15:
            Definition of module type b/1
          Line 2, characters 0-13:
            Definition of module type b/2
       3. Modules do not match: $S3 : sig type w end is not included in y
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
Error: Modules do not match: a/1 is not included in a/2
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
Line 1, characters 52-86:
1 | module X: functor ( X: sig end) -> sig end = functor(X: Set.OrderedType) -> struct end
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
       File "set.mli", line 52, characters 4-10: Expected declaration
       The value `compare' is required but not provided
       File "set.mli", line 55, characters 4-31: Expected declaration
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
         functor (X : $S1) (Y : $S2) (Z : $S3) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. Module types do not match:
            $S2 =
            functor (A : sig type ya end) (B : sig type ybb end) -> sig end
          does not include
            $T2 =
            functor (A : sig type ya end) (B : sig type yb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
          1. Module types $S1 and $T1 match
          2. Module types do not match:
               $S2 = sig type yb end
             does not include
               $T2 = sig type ybb end
             The type `yb' is required but not provided
       3. Module types do not match:
            $S3 =
            functor (A : sig type za end) (B : sig type zbb end) -> sig end
          does not include
            $T3 =
            functor (A : sig type za end) (B : sig type zb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
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
         functor (X : $S1) (Y : $S2) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. Module types $S2 and $T2 match
       3. An argument appears to be missing with module type
              $T3 =
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
         functor (X : $S1) (Y : $S2) (Z : $S3) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types do not match:
            $S1 =
            functor (A : sig type xaa end) (B : sig type xz end) -> sig end
          does not include
            $T1 =
            functor (A : sig type xa end) (B : sig type xz end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
          1. Module types do not match:
               $S1 = sig type xa end
             does not include
               $T1 = sig type xaa end
             The type `xa' is required but not provided
          2. Module types $S2 and $T2 match
       2. Module types do not match:
            $S2 =
            functor (A : sig type ya end) (B : sig type ybb end) -> sig end
          does not include
            $T2 =
            functor (A : sig type ya end) (B : sig type yb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
       3. Module types do not match:
            $S3 =
            functor (A : sig type za end) (B : sig type zbb end) -> sig end
          does not include
            $T3 =
            functor (A : sig type za end) (B : sig type zb end) -> sig end
          Modules do not match:
            functor (A : $S1) (B : $S2) -> ...
          is not included in
            functor (A : $T1) (B : $T2) -> ...
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
         functor (X : $S1) (Y : $S3) (W : $S4) -> ...
       is not included in
         functor $T1 $T2 $T3 $T4 -> ...
       1. Module types $S1 and $T1 match
       2. An argument appears to be missing with module type
              $T2 = sig type y end
       3. Module types do not match:
            $S3 = sig type y' end
          does not include
            $T3 = sig type z end
       4. Module types $S4 and $T4 match
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
         functor (X : A.A) (Y : A.A) A.A (Z : A.A) -> ...
       1. The following extra argument is provided
              A.X : A.Honorificabilitudinitatibus
       2. Module A.Y matches the expected module type A.A
       3. Modules do not match:
            A.X : A.Honorificabilitudinitatibus
          is not included in
            A.A
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
Lines 11-18, characters 2-15:
11 | ..(X: sig type when_ type shall type we type tree type meet type again end)
12 |   (Y:sig type in_ val thunder:in_ val lightning: in_ type pain end)
13 |   (Z:sig type when_ type the type hurlyburly's type gone  end)
14 |   (Z:sig type when_ type the type battle's type last type and_ type won end)
15 |   (W:sig type that type will type be type the type era type set type of_ type sun end)
16 |   (S: sig type where type the type lace end)
17 |   (R: sig type upon type the type heart end)
18 |   -> struct end
Error: Signature mismatch:
       Modules do not match:
         functor (X : $S1) (Y : $S2) (Z : $S3) (Z : $S4) (W : $S5) (S : $S6)
         (R : $S7) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) (Z : $T4) (W : $T5) (S : $T6)
         (R : $T7) -> ...
       1. Module types do not match:
            $S1 =
            sig
              type when_
              type shall
              type we
              type tree
              type meet
              type again
            end
          does not include
            $T1 =
            sig
              type when_
              type shall
              type we
              type three
              type meet
              type again
            end
          The type `tree' is required but not provided
       2. Module types do not match:
            $S2 =
            sig type in_ val thunder : in_ val lightning : in_ type pain end
          does not include
            $T2 =
            sig type in_ val thunder : in_ val lightning : in_ type rain end
       3. Module types do not match:
            $S3 = sig type when_ type the type hurlyburly's type gone end
          does not include
            $T3 = sig type when_ type the type hurlyburly's type done_ end
       4. Module types do not match:
            $S4 =
            sig
              type when_
              type the
              type battle's
              type last
              type and_
              type won
            end
          does not include
            $T4 =
            sig
              type when_
              type the
              type battle's
              type lost
              type and_
              type won
            end
       5. Module types do not match:
            $S5 =
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
            $T5 =
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
       6. Module types do not match:
            $S6 = sig type where type the type lace end
          does not include
            $T6 = sig type where type the type place end
       7. Module types do not match:
            $S7 = sig type upon type the type heart end
          does not include
            $T7 = sig type upon type the type heath end
|}]


(** Abstract module type woes *)


module F(X:sig type witness module type t module M:t end) = X.M

module PF = struct
  type witness
  module type t = module type of F
  module M = F
end

module U = F(PF)(PF)(PF)
[%%expect {|
module F :
  functor (X : sig type witness module type t module M : t end) -> X.t
module PF :
  sig
    type witness
    module type t =
      functor (X : sig type witness module type t module M : t end) -> X.t
    module M = F
  end
module U : PF.t
|}]

module W = F(PF)(PF)(PF)(PF)(PF)(F)
[%%expect {|
Line 1, characters 11-35:
1 | module W = F(PF)(PF)(PF)(PF)(PF)(F)
               ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         PF PF PF PF PF F
       do not match these parameters:
         functor (X : ...) (X : ...) (X : ...) (X : ...) (X : ...) (X : $T6)
         -> ...
       1. Module PF matches the expected module type
       2. Module PF matches the expected module type
       3. Module PF matches the expected module type
       4. Module PF matches the expected module type
       5. Module PF matches the expected module type
       6. Modules do not match:
            F :
            functor (X : sig type witness module type t module M : t end) ->
              X.t
          is not included in
            $T6 = sig type witness module type t module M : t end
          Modules do not match:
            functor (X : $S1) -> ...
          is not included in
            functor  -> ...
          An extra argument is provided of module type
              $S1 = sig type witness module type t module M : t end
|}]

(** Divergent arities *)
module type arg = sig type arg end
module A = struct type arg end

module Add_one' = struct
  module M(_:arg) = A
  module type t = module type of M
end

module Add_one = struct type witness include Add_one' end

module Add_three' = struct
  module M(_:arg)(_:arg)(_:arg) = A
  module type t = module type of M
end

module Add_three = struct
  include Add_three'
  type witness
end


module Wrong_intro = F(Add_three')(A)(A)(A)
[%%expect {|
module type arg = sig type arg end
module A : sig type arg end
module Add_one' :
  sig
    module M : arg -> sig type arg = A.arg end
    module type t = arg -> sig type arg = A.arg end
  end
module Add_one :
  sig type witness module M = Add_one'.M module type t = Add_one'.t end
module Add_three' :
  sig
    module M : arg -> arg -> arg -> sig type arg = A.arg end
    module type t = arg -> arg -> arg -> sig type arg = A.arg end
  end
module Add_three :
  sig module M = Add_three'.M module type t = Add_three'.t type witness end
Line 22, characters 21-43:
22 | module Wrong_intro = F(Add_three')(A)(A)(A)
                          ^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         Add_three' A A A
       do not match these parameters:
         functor (X : $T1) arg arg arg -> ...
       1. Modules do not match:
            Add_three' :
            sig module M = Add_three'.M module type t = Add_three'.t end
          is not included in
            $T1 = sig type witness module type t module M : t end
          The type `witness' is required but not provided
       2. Module A matches the expected module type arg
       3. Module A matches the expected module type arg
       4. Module A matches the expected module type arg
|}]

module Choose_one = F(Add_one')(Add_three)(A)(A)(A)
[%%expect {|
Line 1, characters 20-51:
1 | module Choose_one = F(Add_one')(Add_three)(A)(A)(A)
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         Add_one' Add_three A A A
       do not match these parameters:
         functor (X : ...) arg arg arg -> ...
       1. The following extra argument is provided
              Add_one' :
              sig module M = Add_one'.M module type t = Add_one'.t end
       2. Module Add_three matches the expected module type
       3. Module A matches the expected module type arg
       4. Module A matches the expected module type arg
       5. Module A matches the expected module type arg
|}]

(** Known lmitation: we choose the wrong environment without the
    error on Add_one
**)
module Mislead_chosen_one = F(Add_one)(Add_three)(A)(A)(A)
[%%expect {|
Line 1, characters 28-58:
1 | module Mislead_chosen_one = F(Add_one)(Add_three)(A)(A)(A)
                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         Add_one Add_three A A A
       do not match these parameters:
         functor (X : ...) arg arg arg -> ...
       1. The following extra argument is provided
              Add_one :
              sig
                type witness = Add_one.witness
                module M = Add_one'.M
                module type t = Add_one.t
              end
       2. Module Add_three matches the expected module type
       3. Module A matches the expected module type arg
       4. Module A matches the expected module type arg
       5. Module A matches the expected module type arg
|}]






(** Hide your arity from the world *)

module M: sig
  module F:
    functor (X:sig
               type x
               module type t =
                 functor
                   (Y:sig type y end)
                   (Z:sig type z end)
                   -> sig end
             end) -> X.t
end
= struct
  module F(X:sig type x end)(Z:sig type z end) = struct end
end
[%%expect {|
Lines 14-16, characters 2-3:
14 | ..struct
15 |   module F(X:sig type x end)(Z:sig type z end) = struct end
16 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor (X : sig type x end) (Z : sig type z end) -> sig end
         end
       is not included in
         sig
           module F :
             functor
               (X : sig
                      type x
                      module type t =
                        functor (Y : sig type y end) (Z : sig type z end) ->
                          sig end
                    end)
               -> X.t
         end
       In module F:
       Modules do not match:
         functor (X : $S1) (Z : $S3) -> ...
       is not included in
         functor (X : $T1) (Y : $T2) (Z : $T3) -> ...
       1. Module types $S1 and $T1 match
       2. An argument appears to be missing with module type
              $T2 = sig type y end
       3. Module types $S3 and $T3 match
|}]


module M: sig
  module F(X: sig
      module type T
      module type t = T -> T -> T
      module M: t
    end
          )(_:X.T)(_:X.T): X.T
end = struct
  module F (Wrong: sig type wrong end)
      (X: sig
         module type t
         module M: t
       end)  = (X.M : X.t)
end
[%%expect {|
Lines 8-14, characters 6-3:
 8 | ......struct
 9 |   module F (Wrong: sig type wrong end)
10 |       (X: sig
11 |          module type t
12 |          module M: t
13 |        end)  = (X.M : X.t)
14 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             functor (Wrong : sig type wrong end)
               (X : sig module type t module M : t end) -> X.t
         end
       is not included in
         sig
           module F :
             functor
               (X : sig
                      module type T
                      module type t = T -> T -> T
                      module M : t
                    end)
               -> X.T -> X.T -> X.T
         end
       In module F:
       Modules do not match:
         functor (Wrong : $S1) (X : $S2) X.T X.T -> ...
       is not included in
         functor (X : $T2) X.T X.T -> ...
       1. An extra argument is provided of module type
              $S1 = sig type wrong end
       2. Module types $S2 and $T2 match
       3. Module types X/3.T and X/2.T match
       4. Module types X/3.T and X/2.T match
|}]


module M: sig
  module F(_:sig end)(X:
           sig
             module type T
             module type inner = sig
               module type t
               module M: t
             end
             module F(X: inner)(_:T -> T->T):
             sig module type res = X.t end
             module Y: sig
               module type t = T -> T -> T
               module M(X:T)(Y:T): T
             end
           end):
    X.F(X.Y)(X.Y.M).res
end = struct
  module F(_:sig type wrong end) (X:
             sig  module type T end
          )(Res: X.T)(Res: X.T)(Res: X.T) = Res
end
[%%expect {|
Lines 17-21, characters 6-3:
17 | ......struct
18 |   module F(_:sig type wrong end) (X:
19 |              sig  module type T end
20 |           )(Res: X.T)(Res: X.T)(Res: X.T) = Res
21 | end
Error: Signature mismatch:
       Modules do not match:
         sig
           module F :
             sig type wrong end ->
               functor (X : sig module type T end) (Res : X.T) (Res :
                 X.T) (Res : X.T)
               -> X.T
         end
       is not included in
         sig
           module F :
             sig end ->
               functor
                 (X : sig
                        module type T
                        module type inner =
                          sig module type t module M : t end
                        module F :
                          functor (X : inner) -> (T -> T -> T) ->
                            sig module type res = X.t end
                        module Y :
                          sig
                            module type t = T -> T -> T
                            module M : functor (X : T) (Y : T) -> T
                          end
                      end)
               -> X.F(X.Y)(X.Y.M).res
         end
       In module F:
       Modules do not match:
         functor (Arg : $S1) (X : $S2) (Res : X.T) (Res : X.T) (Res :
         X.T) -> ...
       is not included in
         functor (sig end) (X : $T2) X.T X.T -> ...
       1. Module types do not match:
            $S1 = sig type wrong end
          does not include
            sig end
          The type `wrong' is required but not provided
       2. Module types $S2 and $T2 match
       3. An extra argument is provided of module type X/2.T
       4. Module types X/2.T and X/2.T match
       5. Module types X/2.T and X/2.T match
|}]


(** The price of Gluttony: gready update of environment leads to a non-optimal edit distance. *)

module F(X:sig type t end)(Y:sig type t = Y of X.t end)(Z:sig type t = Z of X.t end) = struct end

module X = struct type t = U end
module Y = struct type t = Y of int end
module Z = struct type t = Z of int end

module Error=F(X)(struct type t = int end)(Y)(Z)
[%%expect {|
module F :
  functor (X : sig type t end) (Y : sig type t = Y of X.t end)
    (Z : sig type t = Z of X.t end) -> sig end
module X : sig type t = U end
module Y : sig type t = Y of int end
module Z : sig type t = Z of int end
Line 9, characters 13-48:
9 | module Error=F(X)(struct type t = int end)(Y)(Z)
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application is ill-typed.
       These arguments:
         X ... Y Z
       do not match these parameters:
         functor (X : ...) (Y : $T3) (Z : $T4) -> ...
       1. Module X matches the expected module type
       2. The following extra argument is provided ... : sig type t = int end
       3. Modules do not match:
            Y : sig type t = Y.t = Y of int end
          is not included in
            $T3 = sig type t = Y of X/2.t end
          Type declarations do not match:
            type t = Y.t = Y of int
          is not included in
            type t = Y of X.t
          Constructors do not match:
            Y of int
          is not the same as:
            Y of X.t
          The type int is not equal to the type X.t
       4. Modules do not match:
            Z : sig type t = Z.t = Z of int end
          is not included in
            $T4 = sig type t = Z of X/2.t end
          Type declarations do not match:
            type t = Z.t = Z of int
          is not included in
            type t = Z of X.t
          Constructors do not match:
            Z of int
          is not the same as:
            Z of X.t
          The type int is not equal to the type X.t
|}]

(** Final state in the presence of extensions
    Test provided by Leo White in
    https://github.com/ocaml/ocaml/pull/9331#pullrequestreview-492359720
*)

module type A = sig type a end
module A = struct type a end
module type B = sig type b end
module B = struct type b end

module type ty = sig type t end
module TY = struct type t end

module type Ext = sig module type T module X : T end

module AExt = struct module type T = A module X = A end
module FiveArgsExt = struct
  module type T = ty -> ty -> ty -> ty -> ty -> sig end
  module X : T =
    functor (_ : ty) (_ : ty) (_ : ty) (_ : ty) (_ : ty) -> struct end
end

module Bar (W : A) (X : Ext) (Y : B) (Z : Ext) = Z.X

type fine = Bar(A)(FiveArgsExt)(B)(AExt).a
[%%expect{|
module type A = sig type a end
module A : sig type a end
module type B = sig type b end
module B : sig type b end
module type ty = sig type t end
module TY : sig type t end
module type Ext = sig module type T module X : T end
module AExt : sig module type T = A module X = A end
module FiveArgsExt :
  sig module type T = ty -> ty -> ty -> ty -> ty -> sig end module X : T end
module Bar : functor (W : A) (X : Ext) (Y : B) (Z : Ext) -> Z.T
type fine = Bar(A)(FiveArgsExt)(B)(AExt).a
|}]

type broken1 = Bar(B)(FiveArgsExt)(B)(AExt).a
[%%expect{|
Line 1, characters 15-45:
1 | type broken1 = Bar(B)(FiveArgsExt)(B)(AExt).a
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application Bar(B)(FiveArgsExt)(B)(AExt) is ill-typed.
       These arguments:
         B FiveArgsExt B AExt
       do not match these parameters:
         functor (W : A) (X : Ext) (Y : B) (Z : Ext) -> ...
       1. Modules do not match:
            B : sig type b = B.b end
          is not included in
            A
          The type `a' is required but not provided
       2. Module FiveArgsExt matches the expected module type Ext
       3. Module B matches the expected module type B
       4. Module AExt matches the expected module type Ext
|}]

type broken2 = Bar(A)(FiveArgsExt)(TY)(TY)(TY)(TY)(TY).a
[%%expect{|
Line 1, characters 15-56:
1 | type broken2 = Bar(A)(FiveArgsExt)(TY)(TY)(TY)(TY)(TY).a
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The functor application Bar(A)(FiveArgsExt)(TY)(TY)(TY)(TY)(TY) is ill-typed.
       These arguments:
         A FiveArgsExt TY TY TY TY TY
       do not match these parameters:
         functor (W : A) (X : Ext) (Y : B) (Z : Ext) ty ty ty ty ty -> ...
       1. Module A matches the expected module type A
       2. An argument appears to be missing with module type Ext
       3. An argument appears to be missing with module type B
       4. Module FiveArgsExt matches the expected module type Ext
       5. Module TY matches the expected module type ty
       6. Module TY matches the expected module type ty
       7. Module TY matches the expected module type ty
       8. Module TY matches the expected module type ty
       9. Module TY matches the expected module type ty
|}]

module Shape_arg = struct
  module M1 (Arg1 : sig end) = struct
    module type S1 = sig
      type t
    end
  end

  module type S2 = sig
    module Make (Arg2 : sig end) : M1(Arg2).S1
  end

  module M2 : S2 = struct
    module Make (Arg3 : sig end) = struct
      type t = T
    end
  end

  module M3 (Arg4 : sig end) = struct
    module type S3 = sig
      type t = M2.Make(Arg4).t
    end
  end

  module M4 (Arg5 : sig end) : M3(Arg5).S3 = struct
    module M5 = M2.Make (Arg5)

    type t = M5.t
  end
end
[%%expect{|
module Shape_arg :
  sig
    module M1 :
      functor (Arg1 : sig end) -> sig module type S1 = sig type t end end
    module type S2 =
      sig module Make : functor (Arg2 : sig end) -> M1(Arg2).S1 end
    module M2 : S2
    module M3 :
      functor (Arg4 : sig end) ->
        sig module type S3 = sig type t = M2.Make(Arg4).t end end
    module M4 : functor (Arg5 : sig end) -> M3(Arg5).S3
  end
|}]
