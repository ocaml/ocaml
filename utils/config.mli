(***********************************************************************)
(*                                                                     *)
(*                         Caml Special Light                          *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1995 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id$ *)

(* System configuration *)

val version: string
        (* The current version number of the system *)

val standard_library: string
        (* The directory containing the standard libraries *)
val bytecomp_c_compiler: string
        (* The C compiler to use for the custom runtime mode of the
           bytecode compiler *)
val native_c_compiler: string
        (* The C compiler to use for the native code compiler *)
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
val ast_intf_magic_number: string
        (* Magic number for file holding an interface syntax tree *)
val ast_impl_magic_number: string
        (* Magic number for file holding an implementation syntax tree *)

val max_tag: int
        (* Biggest tag that can be stored in the header of a block. *)
val max_young_wosize: int
        (* Maximal size of arrays that are directly allocated in the
           minor heap *)
val architecture: string
        (* Name of processor type for the native-code compiler *)
val model: string
        (* Name of processor submodel for the native-code compiler *)
val system: string
        (* Name of operating system for the native-code compiler *)

val ext_obj: string
        (* Extension for object files, e.g. [.o] under Unix. *)
val ext_asm: string
        (* Extension for assembler files, e.g. [.s] under Unix. *)
val ext_lib: string
        (* Extension for library files, e.g. [.a] under Unix. *)
