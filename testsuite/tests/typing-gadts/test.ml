module Exp = 
  struct

    type _ t = 
      | IntLit : int -> int t
      | BoolLit : bool -> bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | App : ('a -> 'b) t * 'a t -> 'b t
      | Abs : ('a -> 'b) -> ('a -> 'b) t 


    let rec eval : type s . s t -> s = 
      function
	| IntLit x -> x
	| BoolLit y -> y
	| Pair (x,y) ->
            (eval x,eval y)
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
      | Cons : 'a * 'b t -> ('a * 'b) t
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
      | C1 : int -> int u 
      | C2 : bool -> bool u
	    
    type 'a v = 
      | C1 : int -> int v

    let unexhaustive : type s . s u -> s = 
      function
	| C2 x -> x


    module M : sig type t type u end = 
      struct
        type t = int
        type u = bool
      end	    
    type 'a t = 
      | Foo : M.t -> M.t t 
      | Bar : M.u -> M.u t
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
      | Foo : t -> t v 
      | Bar : u -> u v

    let same_type : type s . s v * s v -> bool  =
      function
	| Foo _ , Foo _ -> true
	| Bar _, Bar _ -> true    
  end
;;

module Existential_escape = 
  struct
    type _ t = C : int -> int t
    type u = D : 'a t -> u
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
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . s t -> unit = 
      function
	| (IntLit _ | BoolLit _) -> ()

end
;;

module Polymorphic_variants = 
  struct
      type _ t = 
      | IntLit : int -> int t
      | BoolLit : bool -> bool t

    let rec eval : type s . [`A] * s t -> unit = 
      function
	| `A, IntLit _ -> ()
	| `A, BoolLit _ -> ()
  end    
;;

module Propagation = struct
  type _ t = 
      IntLit : int -> int t
    | BoolLit : bool -> bool t

  let check : type s. s t -> s = function
    | IntLit n -> n
    | BoolLit b -> b

  let check : type s. s t -> s = fun x ->
    let r = match x with
    | IntLit n -> (n : s )
    | BoolLit b -> b
    in r
end
;;

type _ t = Int : int t ;;

let ky x y = ignore (x = y); x ;;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky (1 : a) 1
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky 1 (1 : a)
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> (1 : a)
  in r (* fails too *)
;;
let test : type a. a t -> a = fun x ->
  let r : a = match x with Int -> 1
  in r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r = ref None in
  begin match x with Int -> r := Some (1 : a) end;
  !r (* normalized to int option *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1 end;
  !r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* fail *)
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> u := Some 1; r := !u end;
  !u
;; (* fail *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* fail *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let a =
    let r : a option ref = ref None in
    begin match x with Int -> r := Some 1; u := !r end;
    !u
  in a
;; (* fail *)
