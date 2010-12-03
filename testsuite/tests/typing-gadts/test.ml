module Exp = 
  struct

    type _ t = 
      | IntLit of int : int t
      | BoolLit of bool : bool t
      | Pair of 'a t * 'b t : ('a * 'b) t
      | App of ('a -> 'b) t * 'a t : 'b t
      | Abs of ('a -> 'b) : ('a -> 'b) t 


    let rec eval : type s . s t -> s = 
      function
	| IntLit x -> x
	| BoolLit y -> y
	| Pair (x,y) -> (eval x,eval y)
	| App (f,a) ->
	  (eval f) (eval a)
	| Abs f -> f 
  end
;;

module List = 
  struct
    type zero
    type _ t = 
      | Nil : zero t
      | Cons of 'a * 'b t : ('a * 'b) t
    let head =
      function
	| Cons (a,b) -> a
    let tail =
      function
	| Cons (a,b) -> b
    let rec length : type a . a t -> int = 
      function
	| Nil -> 0
	| Cons (a,b) -> length b
  end
;;

module Nonexhaustive = 
  struct
    type 'a u = 
      | C1 of int : int u 
      | C2 of bool : bool u
	    
    type 'a v = 
      | C1 of int : int v

    let unexhaustive : type s . s u -> s = 
      function
	| C2 x -> x


    module M : sig type t type u end = 
      struct
        type t = int
        type u = bool
      end	    
    type 'a t = 
      | Foo of M.t : M.t t 
      | Bar of M.u : M.u t
    let same_type : type s . s t * s t -> bool  =
      function
	| Foo _ , Foo _ -> true
	| Bar _, Bar _ -> true
  end
;;

module Exhaustive = 
  struct
    type t = int
    type u = bool
    type 'a v = 
      | Foo of t : t v 
      | Bar of u : u v

    let same_type : type s . s v * s v -> bool  =
      function
	| Foo _ , Foo _ -> true
	| Bar _, Bar _ -> true    
  end
;;

module Existential_escape = 
  struct
    type _ t = C of int : int t
    type u = D of 'a t : u
    let eval (D x) = x
  end
;;

module Rectype = 
  struct
    type (_,_) t = C : ('a,'a) t 
    let _ = 
      fun (type s) ->
	let a : (s, s * s) t = failwith "foo" in 
	match a with
	  C ->
	    ()
  end
;;

module Or_patterns = 
struct
      type _ t = 
      | IntLit of int : int t
      | BoolLit of bool : bool t

    let rec eval : type s . s t -> unit = 
      function
	| (IntLit _ | BoolLit _) -> ()

end
;;

module Polymorphic_variants = 
  struct
      type _ t = 
      | IntLit of int : int t
      | BoolLit of bool : bool t

    let rec eval : type s . [`A] * s t -> unit = 
      function
	| `A, IntLit _ -> ()
	| `A, BoolLit _ -> ()
  end    
;;

module Propagation = 
  struct
 type _ t = 
     IntLit of int : int t
   | BoolLit of bool : bool t

let check : type s. s t -> s = function
  | IntLit n -> n
  | BoolLit b -> b
;;
let check : type s. s t -> s = fun x ->
  let r = match x with
  | IntLit n -> (n : s )
  | BoolLit b -> b
  in r
;;
end
;;
