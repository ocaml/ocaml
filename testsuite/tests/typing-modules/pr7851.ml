(* TEST
 expect;
*)

(* Leo's version *)
module F(X : sig type t end) = struct
  type x = X.t
  type y = X.t
  type t = E of x
  type u = t = E of y
end;;

module M = F(struct type t end);;

module type S = module type of M;;
[%%expect{|
module F :
  (X : sig type t end) ->
    sig type x = X.t type y = X.t type t = E of x type u = t = E of y end
module M : sig type x type y type t = E of x type u = t = E of y end
module type S = sig type x type y type t = E of x type u = t = E of y end
|}]

module rec M1 : S with type x = int and type y = bool = M1;;
[%%expect{|
Line 1, characters 0-58:
1 | module rec M1 : S with type x = int and type y = bool = M1;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "M1.t"
       Constructors do not match:
         "E of M1.x"
       is not the same as:
         "E of M1.y"
       The type "M1.x" = "int" is not equal to the type "M1.y" = "bool"
|}]

let bool_of_int x =
  let (E y : M1.u) = (E x : M1.t) in
  y;;

bool_of_int 3;;
[%%expect{|
Line 2, characters 28-32:
2 |   let (E y : M1.u) = (E x : M1.t) in
                                ^^^^
Error: Unbound module "M1"
|}]

(* Also check the original version *)
type (_,_) eq = Eq : ('a,'a) eq
module F(X : Set.OrderedType) = struct
type x = Set.Make(X).t and y = Set.Make(X).t
type t = E of (x,x) eq
type u = t = E of (x,y) eq
end;;
module M = F(struct type t let compare = compare end);;
module type S = module type of M;;
[%%expect{|
type (_, _) eq = Eq : ('a, 'a) eq
module F :
  (X : Set.OrderedType) ->
    sig
      type x = Set.Make(X).t
      and y = Set.Make(X).t
      type t = E of (x, x) eq
      type u = t = E of (x, y) eq
    end
module M :
  sig type x and y type t = E of (x, x) eq type u = t = E of (x, y) eq end
module type S =
  sig type x and y type t = E of (x, x) eq type u = t = E of (x, y) eq end
|}]
module rec M1 : S with type x = int and type y = bool = M1;;
let (E eq : M1.u) = (E Eq : M1.t);;
let cast : type a b. (a,b) eq -> a -> b = fun Eq x -> x;;
cast eq 3;;
[%%expect{|
Line 1, characters 0-58:
1 | module rec M1 : S with type x = int and type y = bool = M1;;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type "M1.t"
       Constructors do not match:
         "E of (M1.x, M1.x) eq"
       is not the same as:
         "E of (M1.x, M1.y) eq"
       The type "(M1.x, M1.x) eq" is not equal to the type "(M1.x, M1.y) eq"
       Type "M1.x" = "int" is not equal to type "M1.y" = "bool"
|}]
