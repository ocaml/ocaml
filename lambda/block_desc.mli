(* Descriptors definition and construction *)

type approx = Obj.Tag_descriptor.approx =
  | Any
  | Char
  | Int
  | Constants of string array
  | Polymorphic_variants

type t = Obj.Tag_descriptor.t =
  | Unknown
  | Array of approx
  | Tuple  of { name: string; tag: int; fields: approx array }
  | Record of { name: string; tag: int; fields: (string * approx) array }
  | Polymorphic_variant
  | Polymorphic_variant_constant of string

val dump : (string -> unit) -> t -> unit
val compare : t -> t -> int
val format : Format.formatter -> t -> unit
val mask : int

val index : t -> int

val empty : t
val make_array : approx -> t
val make_tuple : int -> string -> approx array -> t
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
