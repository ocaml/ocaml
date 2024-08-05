module type Key = sig
    type 'a t

    val make : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a t

    val get : 'a t -> 'a

    val set : 'a t -> 'a -> unit

    type key_value = KV : 'a t * 'a -> key_value
    val get_initial_keys : unit -> key_value list
    val set_initial_keys : key_value list -> unit

    val at_exit : (unit -> unit) -> unit
    val do_at_exit : unit -> unit
end

module DLS : Key
module TLS : Key
