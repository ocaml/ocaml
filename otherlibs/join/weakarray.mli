type 'a t;;
val create: int -> 'a t;;
val copy: 'a t -> int -> 'a t;;
val add : 'a t -> 'a option -> int;;
val set : 'a t -> int -> 'a option -> unit;;
val get: 'a t -> int -> 'a option;;
val free: 'a t -> int -> unit;;
val length : 'a t -> int;;
        (* [Weak.length ar] returns the length (number of elements) of
           [ar].
         *)

val ref_add : 'a t ref -> 'a option -> int
