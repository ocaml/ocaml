(* System configuration *)

val version: string
        (* The current version number of the system *)

val standard_library: string
        (* The directory containing the standard libraries *)
val c_compiler: string
        (* The C compiler to use for custom runtime mode *)
val c_libraries: string
        (* The C libraries to link with custom runtimes *)

val load_path: string list ref
        (* Directories in the search path for .cmi and .cmo files *)

val exec_magic_number: string
        (* Magic number for bytecode executable files *)
val cmi_magic_number: string
        (* Magic number for compiled interface files *)
val cmo_magic_number: string
        (* Magic number for object bytecode files *)
val cma_magic_number: string
        (* Magic number for archive files *)
val cmx_magic_number: string
        (* Magic number for compilation unit descriptions *)
val cmxa_magic_number: string
        (* Magic number for libraries of compilation unit descriptions *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a block. *)
