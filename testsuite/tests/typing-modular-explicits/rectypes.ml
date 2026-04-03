(* TEST
  flags = " -rectypes ";
  expect;
*)


module type T = sig
  type t
end

[%%expect{|
module type T = sig type t end
|}]

let f (x : (module M : T) -> (module M : T) -> 'a as 'a) =
  (x : ((module N : T) -> 'b) as 'b)

[%%expect{|
Line 1, characters 53-55:
1 | let f (x : (module M : T) -> (module M : T) -> 'a as 'a) =
                                                         ^^
Error: This alias is bound to type "(module M : T) -> (module M : T) -> 'a"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside
       "(module M : T) -> (module M : T) -> 'a"
|}, Principal.Rectypes{|
val f :
  ((module M : T) -> (module M : T) -> 'a as 'a) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}, Rectypes{|
val f : ((module N : T) -> 'a as 'a) -> 'a = <fun>
|}]


let f (x : (module M : T) -> ((module M : T) -> 'a as 'a)) =
  (x : ((module N : T) -> 'b) as 'b)

[%%expect{|
Line 1, characters 54-56:
1 | let f (x : (module M : T) -> ((module M : T) -> 'a as 'a)) =
                                                          ^^
Error: This alias is bound to type "(module M : T) -> 'a"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "(module M : T) -> 'a"
|}, Principal.Rectypes{|
val f :
  ((module M : T) -> ((module M : T) -> 'a as 'a)) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}, Rectypes{|
val f : ((module M : T) -> ((module N : T) -> 'a as 'a)) -> 'a = <fun>
|}]

let f (module M : T) (x : (module M : T) -> 'a as 'a) =
  x (module M) (module M) (module M) (module M) (module M)

[%%expect{|
Line 1, characters 50-52:
1 | let f (module M : T) (x : (module M : T) -> 'a as 'a) =
                                                      ^^
Error: This alias is bound to type "(module M : T) -> 'a"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "(module M : T) -> 'a"
|}, Principal.Rectypes{|
val f :
  (module T) -> ((module M : T) -> 'a as 'a) -> ((module M : T) -> 'b as 'b) =
  <fun>
|}, Rectypes{|
val f : (module T) -> ((module M : T) -> 'a as 'a) -> 'a = <fun>
|}]

let f (x : (module M : T) -> ((M.t * ((module N : T) -> 'a)) as 'a)) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
Line 1, characters 64-66:
1 | let f (x : (module M : T) -> ((M.t * ((module N : T) -> 'a)) as 'a)) =
                                                                    ^^
Error: This alias is bound to type "M.t * ((module N : T) -> 'a)"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "M.t * ((module N : T) -> 'a)"
|}, Rectypes{|
Line 2, characters 3-4:
2 |   (x : ((module O : T) -> O.t * 'b) as 'b)
       ^
Error: The value "x" has type
         "(module O : T) -> (O.t * ((module N : T) -> 'a) as 'a)"
       but an expression was expected of type
         "(module O : T) -> O.t * 'b as 'b"
       The module "O" would escape its scope
|}]

let f (x : (module M : T with type t = int) ->
              (M.t * ((module N : T with type t = int) -> 'a) as 'a)) =
  (x : ((module O : T with type t = int) -> O.t * 'b) as 'b)

[%%expect{|
Line 2, characters 65-67:
2 |               (M.t * ((module N : T with type t = int) -> 'a) as 'a)) =
                                                                     ^^
Error: This alias is bound to type
         "M.t * ((module N : T with type t = int) -> 'a)"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside
       "M.t * ((module N : T with type t = int) -> 'a)"
|}, Principal.Rectypes{|
val f :
  ((module M : T with type t = int) ->
   (M.t * ((module N : T with type t = int) -> 'a) as 'a)) ->
  ((module O : T with type t = int) -> O.t * 'b as 'b) = <fun>
|}, Rectypes{|
val f :
  ((module M : T with type t = int) ->
   (M.t * ((module N : T with type t = int) -> 'a) as 'a)) ->
  ((module O : T with type t = int) -> int * 'b as 'b) = <fun>
|}]

let f (x : (module M : T) -> (M.t * ((module N : T) -> (N.t * 'a) as 'a))) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
Line 1, characters 69-71:
1 | let f (x : (module M : T) -> (M.t * ((module N : T) -> (N.t * 'a) as 'a))) =
                                                                         ^^
Error: This alias is bound to type "(module N : T) -> N.t * 'a"
       but is used as an instance of type "'a"
       The type variable "'a" occurs inside "(module N : T) -> N.t * 'a"
|}, Principal.Rectypes{|
val f :
  ((module M : T) -> M.t * ((module N : T) -> N.t * 'a as 'a)) ->
  ((module O : T) -> O.t * 'b as 'b) = <fun>
|}, Rectypes{|
val f : ((module M : T) -> M.t * ((module O : T) -> O.t * 'a as 'a)) -> 'a =
  <fun>
|}]
