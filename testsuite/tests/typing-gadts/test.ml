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

    let discern : type a. a t -> _ = function
        IntLit _ -> 1
      | BoolLit _ -> 2
      | Pair _ -> 3
      | App _ -> 4
      | Abs _ -> 5
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

module Normal_constrs = struct
  type a = A
  type b = B

  let f = function A -> 1 | B -> 2
end;;

type _ t = Int : int t ;;

let ky x y = ignore (x = y); x ;;

let test : type a. a t -> a =
  function Int -> ky (1 : a) 1
;;

let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky (1 : a) 1  (* fails *)
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> ky 1 (1 : a)  (* fails *)
  in r
;;
let test (type a) x =
  let r = match (x : a t) with Int -> ky 1 1
  in r
;;
let test : type a. a t -> a = fun x ->
  let r = match x with Int -> (1 : a)       (* ok! *)
  in r
;;
let test : type a. a t -> _ = fun x ->
  let r = match x with Int -> 1       (* ok! *)
  in r
;;
let test : type a. a t -> a = fun x ->
  let r : a = match x with Int -> 1
  in r (* ok *)
;;
let test2 : type a. a t -> a option = fun x ->
  let r = ref None in
  begin match x with Int -> r := Some (1 : a) end;
  !r (* ok *)
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
;; (* ok (u non-ambiguous) *)
let test2 : type a. a t -> a option = fun x ->
  let r : a option ref = ref None in
  let u = ref None in
  begin match x with Int -> u := Some 1; r := !u end;
  !u
;; (* fails because u : (int | a) option ref *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let r : a option ref = ref None in
  begin match x with Int -> r := Some 1; u := !r end;
  !u
;; (* ok *)
let test2 : type a. a t -> a option = fun x ->
  let u = ref None in
  let a =
    let r : a option ref = ref None in
    begin match x with Int -> r := Some 1; u := !r end;
    !u
  in a
;; (* ok *)
let either = ky
let we_y1x (type a) (x : a) (v : a t) =
  match v with Int -> let y = either 1 x in y
;; (* fail *)

(* Effect of external consraints *)
let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> (y : a) in (* ok *)
  r
;;
let f (type a) (x : a t) y =
  let r = match x with Int -> (y : a) in
  ignore (y : a); (* ok *)
  r
;;
let f (type a) (x : a t) y =
  ignore (y : a);
  let r = match x with Int -> y in (* ok *)
  r
;;
let f (type a) (x : a t) y =
  let r = match x with Int -> y in
  ignore (y : a); (* ok *)
  r
;;
let f (type a) (x : a t) (y : a) =
  match x with Int -> y (* returns 'a *)
;;

(* Combination with local modules *)

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = a let z = (y : b) end
    in M.z
;; (* fails because of aliasing... *)

let f (type a) (x : a t) y =
  match x with Int ->
    let module M = struct type b = int let z = (y : b) end
    in M.z
;; (* ok *)

(* Pattern matching *)

type 'a t =
    A of int | B of bool | C of float | D of 'a

type _ ty =
  | TE : 'a ty -> 'a array ty
  | TA : int ty
  | TB : bool ty
  | TC : float ty
  | TD : string -> bool ty

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
  | TD "bye", D false -> 13
  | TD "hello", D true -> 12
 (* | TB, D z -> if z then 1 else 2 *)
  | TC, D z -> truncate z
  | _, D _ -> 0
;;

let f : type a. a ty -> a t -> int = fun x y ->
  match x, y with
  | _, A z -> z
  | _, B z -> if z then 1 else 2
  | _, C z -> truncate z
  | TE TC, D [|1.0|] -> 14
  | TA, D 0 -> -1
  | TA, D z -> z
;; (* warn *)

let f : type a. a ty -> a t -> int = fun x y ->
  match y, x with
  | A z, _ -> z
  | B z, _ -> if z then 1 else 2
  | C z, _ -> truncate z
  | D [|1.0|], TE TC -> 14
  | D 0, TA -> -1
  | D z, TA -> z
;; (* fail *)

type ('a,'b) pair = {right:'a; left:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* fail *)

type ('a,'b) pair = {left:'a; right:'b}

let f : type a. a ty -> a t -> int = fun x y ->
  match {left=x; right=y} with
  | {left=_; right=A z} -> z
  | {left=_; right=B z} -> if z then 1 else 2
  | {left=_; right=C z} -> truncate z
  | {left=TE TC; right=D [|1.0|]} -> 14
  | {left=TA; right=D 0} -> -1
  | {left=TA; right=D z} -> z
;; (* ok *)
