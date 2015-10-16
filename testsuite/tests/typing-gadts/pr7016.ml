type (_, _) t =
  | Nil : ('tl, 'tl) t
  | Cons : 'a * ('b, 'tl) t -> ('a * 'b, 'tl) t;;

let get1 (Cons (x, _) : (_ * 'a, 'a) t) = x ;; (* warn, cf PR#6993 *)

let get1' = function
  | (Cons (x, _) : (_ * 'a, 'a) t) -> x
  | Nil -> assert false ;; (* ok *)
