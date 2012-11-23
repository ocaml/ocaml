type t

include Emittable.S with type t := t

val create : abbreviation_code:Abbreviation_code.t
  -> tag:Tag.t
  -> attributes:Attribute.t list
  -> t
