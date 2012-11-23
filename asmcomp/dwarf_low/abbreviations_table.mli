type t

include Emittable with type t := t

val create : Abbreviations_table_entry.t list -> t
