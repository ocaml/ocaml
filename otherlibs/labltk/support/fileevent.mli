open Unix

val   add_fileinput : fd:file_descr -> callback:(unit -> unit) -> unit
val   remove_fileinput: fd:file_descr -> unit
val   add_fileoutput : fd:file_descr -> callback:(unit -> unit) -> unit
val   remove_fileoutput: fd:file_descr -> unit
      (* see [tk] module *)
