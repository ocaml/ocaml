(* List operations *)

val length : 'a list -> int
val hd : 'a list -> 'a
val tl : 'a list -> 'a list
val rev : 'a list -> 'a list
val flatten : 'a list list -> 'a list
val iter : ('a -> 'b) -> 'a list -> unit
val map : ('a -> 'b) -> 'a list -> 'b list
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
val iter2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> unit
val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
val for_all : ('a -> bool) -> 'a list -> bool
val exists : ('a -> bool) -> 'a list -> bool
val mem : 'a -> 'a list -> bool
val assoc : 'a -> ('a * 'b) list -> 'b
val mem_assoc : 'a -> ('a * 'b) list -> bool
val assq : 'a -> ('a * 'b) list -> 'b
val split : ('a * 'b) list -> 'a list * 'b list
val combine : 'a list * 'b list -> ('a * 'b) list

