type ('a, 'b) t = {
  size : int;
  table : ('a, int) Hashtbl.t;
  mutable array : 'b Weakarray.t ref;
} 
val create : int -> ('a, 'b) t
val add : ('a, 'b) t -> 'a -> 'b option -> unit
val find : ('a, 'b) t -> 'a -> 'b option
val remove : ('a, 'b) t -> 'a -> unit
val h_to_list : ('a, 'b) Hashtbl.t -> ('a * 'b) list
val h_from_list : int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
val iter : ('a -> 'b option -> unit) -> ('a, 'b) t -> unit
val to_list : ('a, 'b) t -> ('a * 'b option) list
val from_list : int -> ('a * 'b option) list -> ('a, 'b) t
val clear : ('a, 'b) t -> unit
val clean : ('a, 'b) t -> unit
val find_direct : ('a, 'b) t -> int -> 'b option
val add_direct : ('a, 'b) t -> 'a -> 'b option -> int
val get_direct : ('a, 'b) t -> 'a -> int
