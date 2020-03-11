(* TEST
flags = "-rectypes"
   * expect
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

module M = struct
  type 'a gamma = 'b constraint 'a = < gamma : 'b >
  let o : < gamma : 'a gamma > as 'a = object method gamma = 1 end
end
[%%expect{|
module M :
  sig
    type 'a gamma = 'b constraint 'a = < gamma : 'b >
    val o : < gamma : int >
  end
|}]

(* needs -rectypes *)
module M = struct
  type 'a t = 'b constraint 'a = 'b list
  let f (x : 'a t list) (y : 'a) = (x = y)
end
[%%expect{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f : 'a list t list -> 'a list -> bool
  end
|}]

module M = struct
  type 'a t = 'b constraint 'a = 'b list
  let f (x : 'a list t) (y : 'a) = (x = y)
end
[%%expect{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f : 'a list t -> 'a -> bool
  end
|}]
module type T = sig
  type 'a alpha = 'b constraint 'a = < alpha : 'b >
  type 'a beta = 'b constraint 'a = < beta : 'b >
  type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
  type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
  type 'a alpha_of_gamma = 'a gamma alpha
  type 'a beta_of_delta = 'a delta beta

  val create :
    < delta : < beta : 'a alpha_of_gamma >;
      gamma : < alpha : 'a beta_of_delta > >
    as 'a
end
[%%expect{|
module type T =
  sig
    type 'a alpha = 'b constraint 'a = < alpha : 'b >
    type 'a beta = 'b constraint 'a = < beta : 'b >
    type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
    type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
    type 'a alpha_of_gamma = 'a gamma alpha
      constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
    type 'a beta_of_delta = 'a delta beta
      constraint 'a = < delta : < beta : 'b >; gamma : 'c >
    val create : < delta : < beta : 'a >; gamma : < alpha : 'a > >
  end
|}]

module type T = sig
  type 'a alpha = 'b constraint 'a = < alpha : 'b >
  type 'a gamma = 'b constraint 'a = < gamma : 'b >
  type 'a alpha_of_gamma = 'a gamma alpha

  val create : < gamma : < alpha : 'a alpha_of_gamma > > as 'a
end
[%%expect{|
module type T =
  sig
    type 'a alpha = 'b constraint 'a = < alpha : 'b >
    type 'a gamma = 'b constraint 'a = < gamma : 'b >
    type 'a alpha_of_gamma = 'a gamma alpha
      constraint 'a = < gamma : < alpha : 'b > >
    val create : < gamma : < alpha : 'a > >
  end
|}]

module type T = sig
  type 'a gamma = 'b constraint 'a = < gamma : 'b >
  type 'a alpha = 'b constraint 'a gamma = < alpha : 'b >

  val create : < gamma : < alpha : 'a alpha > > as 'a
end
[%%expect{|
module type T =
  sig
    type 'a gamma = 'b constraint 'a = < gamma : 'b >
    type 'a alpha = 'b constraint 'a = < gamma : < alpha : 'b > >
    val create : < gamma : < alpha : 'a > >
  end
|}]

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

module M = struct
  type 'a alpha = 'b constraint 'a = < alpha : 'b>
  type 'a beta = 'b constraint 'a = < beta : 'b>

  type 'a gamma = 'b constraint 'a = < gamma : 'b; delta : _>
  type 'a delta = 'b constraint 'a = < delta : 'b; gamma : _>

  type 'a alpha_of_gamma = 'a gamma alpha
  type 'a beta_of_delta = 'a delta beta

  type ('a, 'b) alphabeta

  module Alphabeta = struct
    type ('contains_beta, 'just_alpha) t =
        { alphabeta : ('just_alpha, 'contains_beta beta) alphabeta }
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
end
[%%expect{|
module M :
  sig
    type 'a alpha = 'b constraint 'a = < alpha : 'b >
    type 'a beta = 'b constraint 'a = < beta : 'b >
    type 'a gamma = 'b constraint 'a = < delta : 'c; gamma : 'b >
    type 'a delta = 'b constraint 'a = < delta : 'b; gamma : 'c >
    type 'a alpha_of_gamma = 'a gamma alpha
      constraint 'a = < delta : 'b; gamma : < alpha : 'c > >
    type 'a beta_of_delta = 'a delta beta
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
      (< delta : < beta : 'a >; gamma : < alpha : 'b > > delta,
       < delta : < beta : 'a >; gamma : < alpha : 'b > > alpha_of_gamma)
      Alphabeta.t -> < delta : < beta : 'a >; gamma : < alpha : 'b > > t
  end
|}]
