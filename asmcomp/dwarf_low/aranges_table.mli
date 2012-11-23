type t

include Emittable.S with type t := t

val create : start_of_code_label:string
  -> end_of_code_label:string
  -> t
