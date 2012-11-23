type t

include Emittable.S with type t := t

val create_low_pc : address_label:string -> t
val create_high_pc : address_label:string -> t
val create_producer : producer_name:string -> t
val create_name : source_file_path:string -> t
val create_comp_dir : directory:string -> t
val create_stmt_list : section_offset_label:string -> t
val create_external : is_visible_externally:bool -> t
val create_location : location_list_label:string -> t
val create_type : label_name:string -> t
val create_encoding : encoding:Encoding_attribute.t -> t
val create_byte_size : byte_size:int -> t

val attribute : t -> Attribute.t
