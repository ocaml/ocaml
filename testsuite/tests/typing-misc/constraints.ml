type 'a t = [`A of 'a t t] as 'a;; (* fails *)

type 'a t = [`A of 'a t t];; (* fails *)

type 'a t = [`A of 'a t t] constraint 'a = 'a t;;

type 'a t = [`A of 'a t] constraint 'a = 'a t;;

type 'a t = [`A of 'a] as 'a;;

type 'a v = [`A of u v] constraint 'a = t and t = u and u = t;; (* fails *)

type 'a t = 'a;;
let f (x : 'a t as 'a) = ();; (* fails *)

let f (x : 'a t) (y : 'a) = x = y;;
