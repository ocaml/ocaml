(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Zane Hambly                               *)
(*                                                                        *)
(*   Copyright 2026 Zane Hambly                                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Optional instruction set extensions a backend may use.

    A backend names the extensions it knows how to emit and groups them
    into levels, such as [x86-64-v3] on amd64. The default set comes from
    configure, through [Config.target_features], and the command line can
    replace it with a level or turn single extensions on and off. Nothing
    here knows about any particular instruction set. *)

type t

val create :
  arch:string ->
  features:(string * string) list ->
  levels:(string * string list) list ->
  string -> t
(** [create ~arch ~features ~levels default] makes the feature set of one
    backend. [features] pairs each extension name with the description
    used in [-help], and [levels] pairs each level name with the
    extensions it includes. [default] is a comma-separated list in which
    a level name resets the set to that level, a feature name adds it and
    [no-feature] removes it, read left to right, with spaces around items
    ignored. Raises [Misc.Fatal_error] if a level names an extension
    missing from [features]. *)

val enabled : t -> string -> bool
(** [enabled t name] is whether code may use the extension [name]. The
    set starts from the configured default, then takes any
    [target-features] items given in OCAMLPARAM before the arguments,
    then the level chosen with [-ftarget-level], then the [-f] and [-fno-]
    options, whatever order those came in, and finally any
    [target-features] items given in OCAMLPARAM after the arguments.
    Raises [Location.Error] if the default or OCAMLPARAM names an unknown
    extension, so the mistake is reported when code is compiled and
    options such as [-version] still work. Raises [Misc.Fatal_error] if
    [name] itself is unknown. *)

val command_line_options : t -> (string * Arg.spec * string) list
(** The [-ftarget-level] option and a [-f]/[-fno-] pair for each
    extension, for the backend's [Arch.command_line_options]. *)
