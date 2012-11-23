type t

include Emittable with type t := t

val create : label_name:string
  -> abbreviation_code:Abbreviation_code.t
  -> tag:Tag.t
  -> attribute_values:Attribute_value.t list
  -> t

val create_null : unit -> t

val to_abbreviations_table_entry : t -> Abbreviations_table_entry.t option
