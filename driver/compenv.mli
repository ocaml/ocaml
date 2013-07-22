(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*      Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2013 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

val check_unit_name : Format.formatter -> string -> string -> unit

val output_prefix : string -> string
val extract_output : string option -> string
val default_output : string option -> string

val print_version_and_library : string -> 'a
val print_version_string : unit -> 'a
val print_standard_library : unit -> 'a
val fatal : string -> 'a

val first_ccopts : string list ref
val first_ppx : string list ref
val first_include_dirs : string list ref
val last_include_dirs : string list ref
val implicit_modules : string list ref

(* return the list of objfiles, after OCAMLPARAM and List.rev *)
val get_objfiles : unit -> string list

type readenv_position =
  Before_args | Before_compile | Before_link

val readenv : Format.formatter -> readenv_position -> unit
