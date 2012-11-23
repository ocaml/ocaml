type t

include Emittable with type t := t

val create : externally_visible_functions:string list
  -> debug_info:Debug_info_section.t
  -> t
