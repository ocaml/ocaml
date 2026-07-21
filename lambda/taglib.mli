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

val mask : int

val index : t -> int

val library : t list ref

val make_array : approx -> t
val make_tuple : int -> string -> approx array -> t
val make_record : int -> string -> (string * approx) array -> t
val register_polymorphic_variant : string -> unit
val make_polymorphic_variant : string -> t

val default : t

val emit_tags : unit -> t list
val reset_tags : unit -> unit
