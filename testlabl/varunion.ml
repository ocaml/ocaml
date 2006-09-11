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
