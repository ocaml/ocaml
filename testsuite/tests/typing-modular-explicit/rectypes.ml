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
val f : ((module N : T) -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f :
  ((module M : T) -> (module M : T) -> 'a as 'a) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}]


let f (x : (module M : T) -> ((module M : T) -> 'a as 'a)) =
  (x : ((module N : T) -> 'b) as 'b)

[%%expect{|
val f : ((module M : T) -> ((module N : T) -> 'a as 'a)) -> 'a = <fun>
|}, Principal{|
val f :
  ((module M : T) -> ((module M : T) -> 'a as 'a)) ->
  ((module N : T) -> 'b as 'b) = <fun>
|}]

let f (module M : T) (x : (module M : T) -> 'a as 'a) =
  x (module M) (module M) (module M) (module M) (module M)

[%%expect{|
val f : (module T) -> ((module M : T) -> 'a as 'a) -> 'a = <fun>
|}, Principal{|
val f :
  (module T) -> ((module M : T) -> 'a as 'a) -> ((module M : T) -> 'b as 'b) =
  <fun>
|}]

let f (x : (module M : T) -> (M.t * ((module N : T) -> 'a) as 'a)) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
Line 2, characters 3-4:
2 |   (x : ((module O : T) -> O.t * 'b) as 'b)
       ^
Error: This expression has type
         "(module O : T) -> (O.t * ((module N : T) -> 'a) as 'a)"
       but an expression was expected of type
         "(module O : T) -> O.t * 'b as 'b"
       The module "O" would escape its scope
|}]

let f (x : (module M : T with type t = int) ->
              (M.t * ((module N : T with type t = int) -> 'a) as 'a)) =
  (x : ((module O : T with type t = int) -> O.t * 'b) as 'b)

[%%expect{|
val f :
  ((module M : T with type t = int) ->
   (M.t * ((module N : T with type t = int) -> 'a) as 'a)) ->
  ((module O : T with type t = int) -> int * 'b as 'b) = <fun>
|}, Principal{|
val f :
  ((module M : T with type t = int) ->
   (M.t * ((module N : T with type t = int) -> 'a) as 'a)) ->
  ((module O : T with type t = int) -> O.t * 'b as 'b) = <fun>
|}]

let f (x : (module M : T) -> (M.t * ((module N : T) -> (N.t * 'a) as 'a))) =
  (x : ((module O : T) -> O.t * 'b) as 'b)

[%%expect{|
val f : ((module M : T) -> M.t * ((module O : T) -> O.t * 'a as 'a)) -> 'a =
  <fun>
|}, Principal{|
val f :
  ((module M : T) -> M.t * ((module N : T) -> N.t * 'a as 'a)) ->
  ((module O : T) -> O.t * 'b as 'b) = <fun>
|}]
