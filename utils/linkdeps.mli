(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                              Hugo Heuzard                              *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t
(** The state of the linking check.
    It keeps track of compilation units provided and required so far. *)

type compunit = string

type filename = string

val create : complete:bool -> t
(** [create ~complete] returns an empty state. If [complete] is
   [true], missing compilation units will be treated as errors.  *)

val add : t
  -> filename:filename -> compunit:compunit
  -> provides:compunit list -> requires:compunit list -> unit
(** [add t ~filename ~compunit ~provides ~requires] registers the
    compilation unit [compunit] found in [filename] to [t].
    - [provides] are units and sub-units provided by [compunit]
    - [requires] are units required by [compunit]

    [add] should be called in reverse topological order. *)

val required : t -> compunit -> bool
(** [required t compunit] returns [true] if [compunit] is a dependency of
    previously added compilation units. *)

type compunit_and_source = {
  compunit : compunit;
  filename : filename;
}

type error =
  | Missing_implementations of (compunit * compunit_and_source list) list
  | Wrong_link_order of (compunit_and_source * compunit_and_source list) list
  | Multiple_definitions of (compunit * filename list) list

val check : t -> error option
(** [check t] should be called once all the compilation units to be linked
    have been added.  It returns some error if:
    - There are some missing implementations
      and [complete] is [true]
    - Some implementation appear
      before their dependencies *)


val report_error :
  print_filename:string Format_doc.printer -> error Format_doc.format_printer
val report_error_doc :
  print_filename:string Format_doc.printer -> error Format_doc.printer
