type t

include Emittable.S with type t := t

val create : Abbreviations_table_entry.t list -> t
