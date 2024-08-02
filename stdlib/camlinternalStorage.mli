module type Storage_interface = sig
    type 'a key

    val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key

    val get : 'a key -> 'a

    val set : 'a key -> 'a -> unit

    type key_value = KV : 'a key * 'a -> key_value
    val get_initial_keys : unit -> key_value list
    val set_initial_keys : key_value list -> unit
end

module DLS : Storage_interface
