(* Miscellaneous useful types and functions *)

val fatal_error: string -> 'a
exception Fatal_error

val map_end: ('a -> 'b) -> 'a list -> 'b list -> 'b list
val for_all2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool

val find_in_path: string list -> string -> string
        (* Search a file in a list of directories. *)
val remove_file: string -> unit
        (* Delete the given file if it List.exists. Never raise an error. *)
val temp_file: string -> string -> string
        (* Return the name of a non-existent temporary file in [/tmp]. *)

val create_hashtable: int -> ('a * 'b) list -> ('a, 'b) Hashtbl.t
        (* Create a hashtable of the given size and fills it with the
           given bindings. *)

val capitalize: string -> string
val lowercase: string -> string

val copy_file: in_channel -> out_channel -> unit
        (* [copy_file ic oc] reads the contents of file [ic] and copies
           them to [oc]. It stops when encountering EOF on [ic]. *)
val copy_file_chunk: in_channel -> out_channel -> int -> unit
        (* [copy_file_chunk ic oc n] reads [n] bytes from [ic] and copies
           them to [oc]. It raises [End_of_file] when encountering
           EOF on [ic]. *)
