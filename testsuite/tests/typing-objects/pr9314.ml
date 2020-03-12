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
  let f1 (x : 'a t list) (y : 'a) = (x = y)
  let f2 (x : 'a t list) (y : 'a) = (y = x)
end
[%%expect{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f1 : 'a list t list -> 'a list -> bool
    val f2 : 'a list t list -> 'a list -> bool
  end
|}]

module M = struct
  type 'a t = 'b constraint 'a = 'b list
  let f1 (x : 'a list t) (y : 'a) = (x = y)
  let f2 (x : 'a list t) (y : 'a) = (y = x)
end
[%%expect{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f1 : 'a list t -> 'a -> bool
    val f2 : 'a list t -> 'a -> bool
  end
|}]

module M = struct
  type 'a u = 'a list
  type 'a t = 'b constraint 'a = 'b u
  let f1 (x : 'a t u) (y : 'a) = (x = y)
  let f2 (x : 'a t u) (y : 'a) = (y = x)
end
[%%expect{|
module M :
  sig
    type 'a u = 'a list
    type 'a t = 'b constraint 'a = 'b u
    val f1 : 'a u t u -> 'a u -> bool
    val f2 : 'a u t u -> 'a u -> bool
  end
|}]

module M = struct
  type 'a t = 'b constraint 'a = 'b list
  let f1 (x : ('c list as 'a) t list as 'c) (y : 'a) = (x = y)
  let f2 (x : ('c list as 'a) t list as 'c) (y : 'a) = (y = x)
end
[%%expect{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f1 : ('a list as 'a) -> 'a -> bool
    val f2 : ('a list as 'a) -> 'a -> bool
  end
|}, Principal{|
module M :
  sig
    type 'a t = 'b constraint 'a = 'b list
    val f1 : ('a list t list as 'a) -> ('b list t list as 'b) -> bool
    val f2 : ('a list t list as 'a) -> ('b list t list as 'b) -> bool
  end
|}]

(* beware of recursive types *)

let f1 (x : <a : <a : 'a> as 'b > as 'a) (y : 'b) = (x = y)
let f2 (x : <a : <a : 'a> as 'b > as 'a) (y : 'b) = (y = x)
[%%expect{|
val f1 : (< a : 'a > as 'a) -> 'a -> bool = <fun>
val f2 : (< a : 'a > as 'a) -> 'a -> bool = <fun>
|}, Principal{|
val f1 : (< a : < a : 'a > > as 'a) -> (< a : < a : 'b > > as 'b) -> bool =
  <fun>
val f2 : (< a : < a : 'a > > as 'a) -> (< a : < a : 'b > > as 'b) -> bool =
  <fun>
|}]


(* Original report *)
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
    val create :
      < delta : < beta : 'a alpha_of_gamma >; gamma : < alpha : 'b > > as 'a
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
|}, Principal{|
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
      (< delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                          beta_of_delta >;
         gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                           alpha_of_gamma > >
       delta,
       < delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                          beta_of_delta >;
         gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                           alpha_of_gamma > >
       alpha_of_gamma)
      Alphabeta.t ->
      < delta : < beta : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                         beta_of_delta >;
        gamma : < alpha : < delta : < beta : 'a >; gamma : < alpha : 'b > >
                          alpha_of_gamma > >
      t
  end
|}]
