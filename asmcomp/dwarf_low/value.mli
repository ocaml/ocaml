type t

include Emittable.S with type t := t

val as_four_byte_int : int -> t
val as_four_byte_int_from_label : string -> t
val as_two_byte_int : int -> t
val as_byte : int -> t
val as_uleb128 : int -> t
val as_leb128 : int -> t
val as_string : string -> t
val as_code_address_from_label : string -> t
val as_code_address_from_label_diff : string -> string -> t
val as_code_address : Int64.t -> t
