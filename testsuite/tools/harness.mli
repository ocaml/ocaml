(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            David Allsopp, University of Cambridge & Tarides            *)
(*                                                                        *)
(*   Copyright 2025 David Allsopp Ltd.                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Test harness support functions and types. *)

(** Types (opened in other modules) *)
module Import : sig
  (** Mechanism used by a tendered bytecode image to find a runtime. *)
  type launch_mode =
  | Header_exe     (** Executable launcher (see {v stdlib/header*.c v}). *)
  | Header_shebang (** Shebang ({v #! v}) interpreter line. *)

  (** Kinds of executable *)
  type executable =
  | Tendered of {header: launch_mode; dlls: bool; runtime: string}
      (** Tendered bytecode image. Executable uses the given mechanism to locate
          a suitable runtime to execute the image. [dlls] is [true] if the
          bytecode image requires additional C libraries to be loaded. [runtime]
          is the runtime system which it ultimately attempts to exec. *)
  | Custom
      (** {v -custom v} bytecode executable. The executable {e is} the OCaml
          bytecode runtime, with the bytecode image appended to the file. *)
  | Vanilla
      (** Normal executable, produced by a linker and not further altered. *)

  (** Test harness phases. *)
  type phase =
  | Original (* Compiler installed in its original configured prefix. *)
  | Renamed  (* Compiler moved to a different prefix from its configuration. *)

  (* Tooling modes. *)
  type mode =
  | Bytecode
      (* Bytecode OCaml. *)
  | Native
      (* Native OCaml. *)

  (** Compiler installation's configuration *)
  type config = {
    has_ocamlnat: bool;
      (** {v [$(INSTALL_OCAMLNAT)] v} - {v Makefile.build_config v} *)
    has_ocamlopt: bool;
      (** {v [$(NATIVE_COMPILER)] v} - {v Makefile.config v} *)
    has_relative_libdir: string option;
      (** Not implemented; always None. *)
    has_runtime_search: bool option;
      (** Not implemented; always None. *)
    launcher_searches_for_ocamlrun: bool;
      (** Indicates whether bytecode executables in the compiler distribution
          use a launcher that is capable of searching PATH to find ocamlrun. At
          present, only native Windows has this behaviour. *)
    target_launcher_searches_for_ocamlrun: bool;
      (** Indicates whether the executable launcher used by ocamlc is capable of
          searching PATH to find ocamlrun. At present, only native Windows has
          this behaviour. *)
    bytecode_shebangs_by_default: bool;
      (** True if ocamlc uses a shebang-style header rather than an executable
          header for tendered bytecode executables. *)
    libraries: string list list
      (** Sorted list of basenames of libraries to test.
          Derived from {v [$(OTHERLIBRARIES)] v} - {v Makefile.config v} *)
  }
end

open Import

val exe : string -> string
(** [exe "foo"] is ["foo"] on Linux (and Cygwin) and ["foo.exe"] on Windows. *)

val no_caml_executable_name : bool
(** [true] if the [caml_executable_name] runtime function is not implemented on
    this platform, which means that it is not guaranteed to be possible to work
    out the absolute path of the currently-running executable. *)

val erase_file : string -> unit
(** [erase_file file] calls [Sys.remove file], wrapped with the usual dances
    required on Windows to try to be sure that the file is actually deleted. *)

val lib : mode -> string -> string
(** [lib mode file] appends [".cma"] if [mode = Bytecode] or [".cmxa"] if
    [mode = Native]. *)

val files_for :
  ?source_and_cmi:bool -> mode -> string -> string list -> string list
(** [files_for ~source_and_cmi mode name files] adds filenames to [files] for
    the {i name}{v .ml v} and {i name}{v .cmi v} if [~source_and_cmi:true] and
    also {i name}{v .cmo v} or {i name}{v .cmx v} depending on [mode]. If [mode]
    is [Native], then the assembled object is also added. *)

val fail_because : ('a, unit, string, 'b) format4 -> 'a
(** [fail_because fmt] displays a formatted message on [stderr], followed by a
    new line and then aborts the harness with code 1. *)

val ocamlc_fails_after_rename : config -> bool
(** [true] if the {v ocamlc v} command will not execute after the prefix has
    been renamed. *)

val pp_path:
  prefix:string -> bindir_suffix:string -> libdir_suffix:string
  -> test_root:string -> Format.formatter -> string -> unit
(** [pp_path ~prefix ~bindir_suffix ~libdir_suffix ~test_root f path] jumps
    through some mildly convoluted hoops to create slightly more readable output
    by substituting some recognised paths with shell-like variables. It applies
    the following transformations:

     - ["$bindir"] or ["$libdir"] if [path] is exactly [~bindir_suffix] or
       [~libdir_suffix] (this captures passing those two variabes to the test
       programs)
     - if [path] begins with [~prefix] then the text is replaced with
       ["$prefix"] (which can create ["$prefix.new/"], etc.). Additionally, if
       the next part of [path] after the following directory separator is
       [bindir_suffix] or [libdir_suffix] then this is replaced with ["$bindir"]
       or ["$libdir"] (i.e. this can generate ["$prefix.new/$bindir"] but not
       ["$prefix.new/foo/$bindir"]
     - if [path] begins [~test_root] (i.e. the current directory) then this
       is replaced with ["$PWD"] but unlike [~prefix] either nothing must follow
       or the next character must be a directory separator. (i.e. it generates
       ["$PWD/"] but never ["$PWDnew/"])
   Both simpler and more convoluted ways of doing this are available. On
   Windows, the comparisons treat forward and back slashes as being the same. *)
