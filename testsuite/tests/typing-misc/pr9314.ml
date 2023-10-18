(* TEST
   expect;
 *)

module type T = sig
  type 'a gamma = 'b constraint 'a = < gamma : 'b >
  val create : < gamma : 'a gamma > as 'a
end
[%%expect{|
module type T =
  sig
    type 'a gamma = 'b constraint 'a = < gamma : 'b >
    val create : < gamma : 'a >
  end
|}]

type 'a gamma = 'b constraint 'a = < gamma : 'b >
let o : < gamma : 'a gamma > as 'a = object method gamma = 1 end
[%%expect{|
type 'a gamma = 'b constraint 'a = < gamma : 'b >
val o : < gamma : int > = <obj>
|}]

(* By Octachron *)
module type T = sig
 type 'a alpha = 'b constraint 'a = < alpha : 'b >
  type 'a beta = 'b constraint 'a = < beta : 'b >
  type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
  type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
  type 'a alpha_of_gamma = 'a gamma alpha
  type 'a beta_of_delta = 'a delta beta
  type ('a, 'b) w = W
  type ('a, 'just_alpha) x = { field : ('a beta, 'just_alpha) w; }
  type 'a t = A of ('a alpha_of_gamma, 'a beta_of_delta) w

  val create :
  ((< delta : < beta : 'a alpha_of_gamma >;
      gamma : < alpha : 'a beta_of_delta > >
    as 'a)
   delta, 'a alpha_of_gamma)
  x -> 'a t
end
[%%expect{|
module type T =
  sig
    type 'a alpha = 'b constraint 'a = < alpha : 'b >
    type 'a beta = 'b constraint 'a = < beta : 'b >
    type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
    type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
    type 'a alpha_of_gamma = < alpha : 'c > alpha
      constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
    type 'a beta_of_delta = < beta : 'b > beta
      constraint 'a = < delta : < beta : 'b >; gamma : 'c >
    type ('a, 'b) w = W
    type ('a, 'just_alpha) x = { field : ('a beta, 'just_alpha) w; }
      constraint 'a = < beta : 'b >
    type 'a t = A of ('a alpha_of_gamma, 'a beta_of_delta) w
      constraint 'a = < delta : < beta : 'b >; gamma : < alpha : 'c > >
    val create :
      (< beta : 'a >,
       < delta : < beta : 'a >; gamma : < alpha : 'a > > alpha_of_gamma)
      x -> < delta : < beta : 'a >; gamma : < alpha : 'a > > t
  end
|}]

(* Original by smuenzel-js *)
type 'a alpha = 'b constraint 'a = < alpha : 'b>
type 'a beta = 'b constraint 'a = < beta : 'b>

type 'a gamma = 'b constraint 'a = < gamma : 'b; delta : _>
type 'a delta = 'b constraint 'a = < delta : 'b; gamma : _>

type 'a alpha_of_gamma = 'a gamma alpha
type 'a beta_of_delta = 'a delta beta

type ('a, 'b) alphabeta

module Alphabeta = struct
  type ('contains_beta, 'just_alpha) t = { alphabeta : ('just_alpha, 'contains_beta beta) alphabeta }
end

type 'a t =
  { other : int
  ; alphabeta : ('a alpha_of_gamma, 'a beta_of_delta) alphabeta
  }

let create
      (input : ('a delta, 'a alpha_of_gamma) Alphabeta.t)
  : 'a t
  =
  let t =
    { other = 0
    ; alphabeta = input.alphabeta
    }
  in
  t
;;
[%%expect{|
type 'a alpha = 'b constraint 'a = < alpha : 'b >
type 'a beta = 'b constraint 'a = < beta : 'b >
type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
type 'a alpha_of_gamma = < alpha : 'c > alpha
  constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
type 'a beta_of_delta = < beta : 'b > beta
  constraint 'a = < delta : < beta : 'b >; gamma : 'c >
type ('a, 'b) alphabeta
module Alphabeta :
  sig
    type ('a, 'just_alpha) t = {
      alphabeta : ('just_alpha, 'a beta) alphabeta;
    } constraint 'a = < beta : 'b >
  end
type 'a t = {
  other : int;
  alphabeta : ('a alpha_of_gamma, 'a beta_of_delta) alphabeta;
} constraint 'a = < delta : < beta : 'b >; gamma : < alpha : 'c > >
val create :
  (< beta : 'a >,
   < delta : < beta : 'a >; gamma : < alpha : 'b > > alpha_of_gamma)
  Alphabeta.t -> < delta : < beta : 'a >; gamma : < alpha : 'b > > t = <fun>
|}]
