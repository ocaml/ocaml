type t

include Emittable with type t := t

val create :
     tags_with_attribute_values:
       (int * string * Tag.t * (Attribute_value.t list)) list
  -> t

val to_abbreviations_table : t -> Abbreviations_table.t
