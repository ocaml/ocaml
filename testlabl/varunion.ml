type t = private [> ];;
type u = private [> ] ~ [t];;
type v = [t | u];;
let f x = (x : t :> v);;

(* bad *)
module Mix(X: sig type t = private [> ] end)
    (Y: sig type t = private [> ] end) =
  struct type t = [X.t | Y.t] end;;

(* bad *)
module Mix(X: sig type t = private [> `A of int ] end)
    (Y: sig type t = private [> `A of bool] ~ [X.t] end) =
  struct type t = [X.t | Y.t] end;;

(* ok *)
module Mix(X: sig type t = private [> `A of int ] end)
    (Y: sig type t = private [> `A of int] ~ [X.t] end) =
  struct type t = [X.t | Y.t] end;;

(* bad *)
module Mix(X: sig type t = private [> `A of int ] end)
    (Y: sig type t = private [> `B of bool] ~ [X.t] end) =
  struct type t = [X.t | Y.t] end;;

(* ok *)
module Mix(X: sig type t = private [> `A of int ] ~ [`B of bool] end)
    (Y: sig type t = private [> `B of bool] ~ [X.t] end) =
  struct type t = [X.t | Y.t] end;;

(* ok *)
module Mix(X: sig type t = private [> `A of int ] ~ [~`B] end)
    (Y: sig type t = private [> `B of bool] ~ [X.t] end) =
  struct type t = [X.t | Y.t] let is_t = function #t -> true | _ -> false end;;

module Mix(X: sig type t = private [> `A of int ] ~ [~`B] end)
    (Y: sig type t = private [> `B of bool] ~ [X.t] end) =
  struct
    type t = [X.t | Y.t]
    let which = function #X.t -> `X | #Y.t -> `Y
  end;;

(* ok *)
module M =
  Mix(struct type t = [`A of int | `C of char] end)
    (struct type t = [`B of bool | `C of char] end);;

(* bad *)
module M =
  Mix(struct type t = [`A of int | `B of bool] end)
    (struct type t = [`B of bool | `C of char] end);;

(* ok *)
module M1 = struct type t = [`A of int | `C of char] end
module M2 = struct type t = [`B of bool | `C of char] end
module M = Mix(M1)(M2) ;;

let c = (`C 'c' : M.t) ;;

module M(X : sig type t = private [> `A] end) = 
  struct let f (#X.t as x) = x end;;

type t = private [> `A ] ~ [`B];;
match `B with #t -> 1 | `B -> 2;;

(* expression *)
module Mix(X:sig type t = private [> ] val show: t -> string end)
    (Y:sig type t = private [> ] ~ [X.t] val show: t -> string end) =
  struct
    type t = [X.t | Y.t]
    let show : t -> string = function
        #X.t as x -> X.show x
      | #Y.t as y -> Y.show y
  end;;

module EStr = struct
  type t = [`Str of string]
  let show (`Str s) = s
end
module EInt = struct
  type t = [`Int of int]
  let show (`Int i) = string_of_int i
end
module M = Mix(EStr)(EInt);;

module type T = sig type t = private [> ] val show: t -> string end
module Mix(X:T)(Y:T with type t = private [> ] ~ [X.t]) :
    T with type t = [X.t | Y.t] =
  struct
    type t = [X.t | Y.t]
    let show = function
        #X.t as x -> X.show x
      | #Y.t as y -> Y.show y
  end;;
module M = Mix(EStr)(EInt);;

(* deep *)
module M : sig type t = private [> ] ~ [`A] end = struct type t = [`A] end
module M' : sig type t = private [> ] end = struct type t = [M.t | `A] end;;

(* parameters *)
module type T = sig
  type t = private [> ] ~ [ `A of int ]
  type ('a,'b) u = private [> ] ~ [ `A of 'a; `A of 'b; `B of 'b ]
  type v = private [> ] ~ [ `A of int; `A of bool ]
end
module F(X:T) = struct
  let h = function
      `A _ -> true
    | #X.t -> false
  let f = function
      `A _ | `B _ -> true
    | #X.u -> false
  let g = function
      `A _ -> true
    | #X.v -> false
end

(* ... *)

(* bad *)
type t = private [> ]
type u = private [> `A of int] ~ [t] ;;

(* ok *)
type t = private [> `A of int]
type u = private [> `A of int] ~ [t] ;;

module F(X: sig
  type t = private [> ] ~ [~`A;~`B;~`C;~`D]
  type u = private [> `A|`B|`C] ~ [t; `D]
end) : sig type v = private [> ] ~ [X.t; X.u] end = struct
  open X
  let f = function #u -> 1 | #t -> 2 | `D -> 3
  let g = function #u|#t|`D -> 2 
  type v = [t|u|`D]
end

(* ok *)
module M = struct type t = private [> `A] end;;
module M' : sig type t = private [> ] ~ [`A] end = M;;

(* ok *)
module type T = sig type t = private [> ] ~ [`A] end;;
module type T' = T with type t = private [> `A];;

(* ok *)
type t = private [> ] ~ [`A of int]
let f = function `A x -> x | #t -> 0
type t' = private [< `A of int | t];;

(* should be ok *)
module F(X:sig end) :
    sig type t = private [> ] type u = private [> ] ~ [t] end =
  struct type t = [ `A] type u = [`B] end
module M = F(String)
let f = function #M.t -> 1 | #M.u -> 2
let f = function #M.t -> 1 | _ -> 2
type t = [M.t | M.u]
let f = function #t -> 1 | _ -> 2
(* bad *)
let f = function #F(String).t -> 1 | _ -> 2;;


(* Expression Problem: functorial form *)

type num = [ `Num of int ]

module type Exp = sig
  type t = private [> num]
  val eval : t -> t
  val show : t -> string
end

module Num(X : Exp) = struct
  type t = num
  let eval (`Num _ as x) : X.t = x
  let show (`Num n) = string_of_int n
end

type 'a add = [ `Add of 'a * 'a ]

module Add(X : Exp with type t = private [> num | 'a add] as 'a) = struct
  type t = X.t add
  let show (`Add(e1, e2) : t) = "("^ X.show e1 ^"+"^ X.show e2 ^")"
  let eval (`Add(e1, e2) : t) =
    let e1 = X.eval e1 and e2 = X.eval e2 in
    match e1, e2 with
      `Num n1, `Num n2 -> `Num (n1+n2)
    | `Num 0, e | e, `Num 0 -> e
    | e12 -> `Add e12
end 

type 'a mul = [`Mul of 'a * 'a]

module Mul(X : Exp with type t = private [> num | 'a mul] as 'a) = struct
  type t = X.t mul
  let show (`Mul(e1, e2) : t) = "("^ X.show e1 ^"*"^ X.show e2 ^")"
  let eval (`Mul(e1, e2) : t) =
    let e1 = X.eval e1 and e2 = X.eval e2 in
    match e1, e2 with
      `Num n1, `Num n2 -> `Num (n1*n2)
    | `Num 0, e | e, `Num 0 -> `Num 0
    | `Num 1, e | e, `Num 1 -> e
    | e12 -> `Mul e12
end

module Ext(X : sig type t = private [> ] end)(Y : sig type t end) = struct
  module type S =
    sig
      type t = private [> ] ~ [ X.t ]
      val eval : t -> Y.t
      val show : t -> string
    end
end

module Mix(E : Exp)(E1 : Ext(E)(E).S)(E2 : Ext(E1)(E).S) =
  struct
    type t = [E1.t | E2.t]
    let eval = function
        #E1.t as x -> E1.eval x
      | #E2.t as x -> E2.eval x
    let show = function
        #E1.t as x -> E1.show x
      | #E2.t as x -> E2.show x
  end

module rec EAdd : (Exp with type t = [num | EAdd.t add]) =
    Mix(EAdd)(Num(EAdd))(Add(EAdd))

(* A bit heavy: one must pass E to everybody *)
module rec E : Exp with type t = [num | E.t add | E.t mul] =
    Mix(E)(Mix(E)(Num(E))(Add(E)))(Mul(E))

let e = E.eval (`Add(`Mul(`Num 2,`Num 3),`Num 1))

(* Alternatives *)
(* Direct approach, no need of Mix *)
module rec E : (Exp with type t = [num | E.t add | E.t mul]) =
  struct
    module E1 = Num(E)
    module E2 = Add(E)
    module E3 = Mul(E)
    type t = E.t
    let show = function
      | #num as x -> E1.show x
      | #add as x -> E2.show x
      | #mul as x -> E3.show x
    let eval = function
      | #num as x -> E1.eval x
      | #add as x -> E2.eval x
      | #mul as x -> E3.eval x
  end

(* Do functor applications in Mix *)
module type T = sig type t = private [> num] end

module Ext(E : T)(X : sig type t = private [> ] end) = struct
  module type S = functor (Y : Exp with type t = E.t) ->
    sig
      type t = private [> ] ~ [ X.t ]
      val eval : t -> Y.t
      val show : t -> string
    end
end

module Mix(E : Exp)(F1 : Ext(E)(E).S)(F2 : Ext(E)(F1(E)).S) =
  struct
    module E1 = F1(E)
    module E2 = F2(E)
    type t = [E1.t | E2.t]
    let eval = function
        #E1.t as x -> E1.eval x
      | #E2.t as x -> E2.eval x
    let show = function
        #E1.t as x -> E1.show x
      | #E2.t as x -> E2.show x
  end

module Join(E : Exp)(F1 : Ext(E)(E).S)(F2 : Ext(E)(F1(E)).S)
    (E' : Exp with type t = E.t) =
  Mix(E)(F1)(F2)

module rec EAdd : (Exp with type t = [num | EAdd.t add]) =
  Mix(EAdd)(Num)(Add)

module rec EMul : (Exp with type t = [num | EMul.t mul]) =
  Mix(EMul)(Num)(Mul)

module rec E : (Exp with type t = [num | E.t add | E.t mul]) =
  Mix(E)(Join(E)(Num)(Add))(Mul)

(* Linear extension by the end: not so nice *)
module LExt(X : sig type t = private [> ] end) = struct
  module type S =
    sig
      type t = private [> ] ~ [X.t]
      val eval : t -> X.t
      val show : t -> string
    end
end
module LNum(E: Exp)(X : LExt(E).S) =
  struct
    type t = [num | X.t]
    let show = function
        `Num n -> string_of_int n
      | #X.t as x -> X.show x
    let eval = function
        #num as x -> x
      | #X.t as x -> X.eval x
  end
module LAdd(E : Exp with type t = private [> num | 'a add] as 'a)
    (X : LExt(E).S) =
  LNum(E)
    (struct
      type t = [E.t add | X.t]
      let show = function
          `Add(e1,e2) -> "("^ E.show e1 ^"+"^ E.show e2 ^")"
        | #X.t as x -> X.show x
      let eval = function
          `Add(e1,e2) ->
            let e1 = E.eval e1 and e2 = E.eval e2 in
            begin match e1, e2 with
              `Num n1, `Num n2 -> `Num (n1+n2)
            | `Num 0, e | e, `Num 0 -> e
            | e12 -> `Add e12
            end
        | #X.t as x -> X.eval x
    end)
module LEnd = struct
  type t = [`Dummy]
  let show `Dummy = ""
  let eval `Dummy = `Dummy
end
module rec L : Exp with type t = [num | L.t add | `Dummy] = LAdd(L)(LEnd)
