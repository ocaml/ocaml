
module StringSet : Set.S with type elt = string

val free_structure_names : StringSet.t ref

val add_use_file : StringSet.t -> Parsetree.toplevel_phrase list -> unit

val add_structure : StringSet.t -> Parsetree.structure -> unit

val add_signature : StringSet.t -> Parsetree.signature -> unit

val addmodule : StringSet.t -> Longident.t -> unit
