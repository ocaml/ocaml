
type 'a t;;

external create: int -> 'a t = "weakset_create";;
external add: 'a t -> 'a -> 'a t = "weakset_add";;
external to_array: 'a t -> 'a array = "weakset_array";;
external remove: 'a t -> 'a -> bool = "weakset_remove";;
external info: 'a t -> int * int = "weakset_info";;
