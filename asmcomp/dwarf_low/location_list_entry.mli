type t

include Emittable.S with type t := t

val create_location_list_entry : start_of_code_label:string
  -> first_address_when_in_scope:string
  -> first_address_when_not_in_scope:string
  -> location_expression:Location_expression.t
  -> t

val create_base_address_selection_entry : base_address_label:string
  -> t
