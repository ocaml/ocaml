(* $Id$ *)

(* Unix utilities *)

val get_files_in_directory : string -> string list
val is_directory : string -> bool
val get_directories_in_files : path:string -> string list -> string list
val subshell : cmd:string -> string list
