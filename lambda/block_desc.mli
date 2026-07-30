(* Descriptors definition and construction *)

type approx = Introspect.Desc.approx

type t = Introspect.Desc.t

val dump : (string -> unit) -> t -> unit
val compare : t -> t -> int
val format : Format.formatter -> t -> unit
val mask : int

val index : t -> int

val empty : t
val simple_ref : t
val make_array : approx -> t
val make_tuple : int -> string -> (string * approx) array -> t
val make_record : int -> string -> (string * approx) array -> t
val register_polymorphic_variant : string -> unit
val make_polymorphic_variant : string -> t

(* Manage collections of descriptors *)

val pending_descriptors : unit -> t list

type library = private t list
val empty_library : library
val iter_library : (t -> unit) -> library -> unit

val emit : unit -> library
val reset : unit -> unit

val link : library list -> library
