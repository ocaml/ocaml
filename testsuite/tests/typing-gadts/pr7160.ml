type _ t =
  Int : int -> int t | String : string -> string t | Same : 'l t -> 'l t;;
let rec f = function Int x -> x | Same s -> f s;;
type 'a tt = 'a t =
  Int : int -> int tt | String : string -> string tt | Same : 'l1 t -> 'l2 tt;;
