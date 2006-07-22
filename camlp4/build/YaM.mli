(*
 *
 * Copyright (C) 2003-2004 Damien Pous
 * 
 * YaM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 * 
 * YaM is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with YaM; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *)


(** 

@author Damien Pous, (<a href="mailto:Damien.Pous_AT_ens-lyon.fr"><small>Damien.Pous_AT_ens-lyon.fr</small></a>)
@version 1.0 

*)


(** {6 Environment / options} *)

type options_t = {
  ocaml:              string ref; (** OCaml interpreter *)
  ocamlc:             string ref; (** OCaml compiler *)
  ocamlopt:           string ref; (** OCaml native compiler *)
  ocamldep:           string ref; (** OCaml dependencies generator *)
  ocamldoc:           string ref; (** OCaml documentation generator *)
  ocamlyacc:          string ref; (** OCaml parser generator *)
  ocamllex:           string ref; (** OCaml lexer generator *)
  ocamlglade:         string ref; (** OCaml glade compiler *)
  ocaml_P4:           string ref; (** Preprocessor to use *)
  ocaml_P4_opt:       string ref; (** Preprocessor to use `ocamlopt -c` *)
  ocaml_Flags:        string ref; (** Flags (c, byte, native and link modes) *)
  ocaml_OptFlags:     string ref; (** Flags for `ocamlopt -c' *)
  ocaml_ByteFlags:    string ref; (** Flags for `ocamlc -c' *)
  ocaml_LinkFlags:    string ref; (** Flags for linking *)
  ocaml_ForPack:      string ref;
  ocaml_Includes:     string list ref; (** Directories to include (with dependencies) *)
  ocaml_ExtIncludes:  string list ref; (** External directories to include (without dependencies) *)
  ocaml_ExtLibraries: string list ref; (** External Libraries to use *)
}
(** The type of environments. *)

val options: options_t ref
(** The current environment. 

Initial compiler names (ocamlc, ocamldep...) are taken in the environment (OCAMLC, OCAMLDEP...), 
or defaults to standard values ("ocamlc", "ocamldep"...).

Initial flags are empty.
*)

val new_scope: 'a Lazy.t -> 'a
(** Forcing of a lazy value inside a `fresh' environment. *)



(** {6 Compilation units}

The way to specify environment and flags is redundant: 
 - the [?o] argument defaults to the current environment ([!options]).
 - the other optional arguments are merged with this value.
*)


type unit_t 
(** The type of compilation units. *)


val ocaml_Module: 
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> 
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for a single, non interfaced OCaml module. *)

val ocaml_IModule:
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> 
  ?impl_flags: string ->
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for an OCaml module, with its interface. *)

val ocaml_Interface: 
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> 
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for a pure OCaml interface. *)

val c_Module: 
  ?o: options_t -> ?flags: string -> ?source_deps: string list -> 
  string -> unit_t
(** Creates a compilation unit for a C module. 

  [source_deps] are the additional source dependencies.
*)

val ocaml_Lexer: 
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> ?lex_flags: string ->
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for an OCaml lexer. *)

val ocaml_Parser:
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> ?yacc_flags: string ->
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for an OCaml parser. *)

val ocaml_Glade:
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> ?glade_flags: string ->
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for an OCaml glade GUI. *)

val ocaml_Package:    
  ?o: options_t ->
  string -> unit_t list -> unit_t
(** Creates a compilation unit for an OCaml `package'.

  The objects defined by the given list of units are packed together, 
  using the `-pack' OCaml option.
*)

val ocaml_PackageDir: 
  ?o: options_t ->
  string -> unit_t list Lazy.t -> unit_t
(** Like {!YaM.ocaml_Package}, but assumes that all the files reside in a directory.

  [ocaml_PackageDir name list] will add the `-I name' flag, and prepend each filename inside
  `list' with `name/'.
*)

val ocaml_Library: 
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string ->
  ?includes: string list -> ?libraries: string list -> 
  ?default: [`Native | `Byte] ->
  string -> unit_t list -> unit_t
(** Creates a compilation unit for an OCaml Library. *)

val ocaml_Program: 
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string ->
  ?includes: string list -> ?libraries: string list -> 
  ?default: [`Native | `Byte] ->
  string -> unit_t list -> unit_t
(** Creates a compilation unit for an OCaml Program. *)

val generic_ocaml_Module_extension: 
  string ->
  (string -> string -> string -> string) ->
  ?o: options_t ->
  ?flags: string -> ?byte_flags: string -> ?opt_flags: string -> 
  ?cmd_flags: string ->
  ?pp: string -> ?includes: string list -> ?ext_includes: string list -> 
  string -> unit_t
(** Creates a compilation unit for a single, non interfaced OCaml module. *)

val phony_unit: ?depends: string list -> ?command: string -> string -> unit_t
(** creates a phony unit which depends on [depends], and is built with [command]. *)


val fold_units_sources : unit_t list -> (string -> string list -> ('a -> 'a) -> 'a -> 'a) -> 'a -> 'a


(** {6 Entry point} *)

val main: ?rebuild: string -> ?deps: string list -> unit_t list -> unit
(** default entry point (command line parsing). 

- [rebuild] is the command to use in order to compile YaM whenever `Makefile.ml' changed. 
(defaults to "ocaml build.ml")

- [deps] is the set of files YaM depends on.
(defaults to ["Makefile.ml"])
*)



(** {6 Lower level functions} *)


type project_t
(** The abstract type of projects. *)

val project: 
  ?rebuild: string -> 
  ?deps: string list -> 
  unit_t list 
  -> project_t
(** Creates a project using the given list of units.

- [rebuild] is the command to use in order to compile YaM whenever `Makefile.ml' changed. 
(defaults to "ocaml build.ml")

- [deps] is the set of files YaM depends on.
(defaults to ["Makefile.ml"])
*)

val print_deps: bool ref
(** Controls wether to print dependencies commands. *)

val print_cmds: bool ref
(** Controls wether to print commands. *)

val build: ?target:string -> project_t -> unit
(** Build the specified target, or the whole project if not specified. *)

val clean: project_t -> unit
(** Clean all targets reachable from the project. *)

val sources_of_project: project_t -> string list
(** Return all the source files of the project *)


val dir: string ref
(** The current directory. (used by ocaml_PackageDir) *)

val generic_unit: 
  name: string ->
  ?sources: string list -> targets: string list -> ?trash: string list -> ?auto_targets: string list ->
  ?sub_units: unit_t list -> ?pregenerated: string list -> ?objects: (string*string) ->  
  dependencies: (native: bool -> string -> string list) ->
  ?dep_files:   (string -> string list) ->
  compile_cmd:  (string -> string * string list) ->
  unit -> unit_t
(** Creates a generic unit.
  - [sources] is the set of source files
  - [targets] is the set of targets handled by the unit
  - [trash] is the set of trash files generated by the unit
  - [auto_targets] is the subset of targets whose building is done, when YaM is called without arguments
  - [sub_units] is a set of "encapsulated" units
  - [pregenerated] is a set of file to "pre-create" automatically if they don't exists (useful for generated sources files and ocamldep)
  - [objects] are the objects to link when building an OCaml package/library/program. It is a pair since linking can be done 
  either in native mode or bytecode mode : [(nat_obj, byte_obj)].
  - [dependencies ~native target] must list the files [target] depends on, in mode specified by [native].
  - [dep_files target] are the files the previous call depends on (for dependencies caching).
  - [compile_cmd target] must return a couple [(cmd,out)], where [cmd] is the command to use for building [target], and [out] is 
  the list of files that this command may create or modify.

Default values are :
  - [sources = auto_targets = sub_units = pregenerated = []]
  - [trash = targets]
  - [objects = None] (no object to link)
  - [depfiles = (fun _ -> [])] (no caching)

When using this function with inside [ocaml_PackageDir], you must set the current directory, 
using [Filename.concat !dir].
*)



(** {6 Simple facilities} *)

val (^^) :  string -> string -> string
(** Concatenates two strings, inserting a space when nessesary. *)

val (^=) :  string ref -> string  -> unit
(** Adds a string to the beginning of a string ref using {!YaM.(^^)}. *)

val (+=) : 'a list ref -> 'a      -> unit 
(** Head insertion. *)

val (@=) : 'a list ref -> 'a list -> unit 
(** Tail insertion. *)

val string_of_list: ('a -> string) -> 'a list -> string
(** Prints a list into a string, using {!YaM.(^^)}. *)

val flatten: string list -> string
(** Flatten a string list, using using {!YaM.(^^)}. *)

val best: (string * string) list -> string

val getenv: string -> string -> string

val which : string -> string
