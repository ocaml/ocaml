(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The name of a "compilation unit" along with any "-for-pack" prefix that
    was specified when the unit was compiled.  By "compilation unit" we
    usually mean the code and data associated with the compilation of a
    single .ml source file.  However some compilation units may correspond to
    other "virtual" groupings of code and data, for example those forming
    part of the runtime, or external libraries. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The type of compilation unit names. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Create a compilation unit with the given [name] (which is not encoded or
    mangled in any way) and an optional "-for-pack" prefix. *)
val create : ?for_pack_prefix:string -> name:string -> t

(** Whether the specified compilation unit is that of the current unit
    being compiled.  An exception will be raised if [set_current] has not
    previously been called. *)
val is_current_exn : t -> bool

(** Record that the given value of type [t] is that of the current unit being
    compiled. *)
val set_current : t -> unit

(** Get the value of type [t] corresponding to the current unit being
    compiled.  An exception will be raised if [set_current] has not
    previously been called. *)
val get_current_exn : unit -> t

(** Like [get_current_exn], but returns an option instead of raising an
    exception. *)
val get_current : unit -> t option

(** The name of the compilation unit, excluding any [for_pack_prefix]. *)
val name : t -> string

(** Any [for_pack_prefix] specified to [create]. *)
val for_pack_prefix : t -> string option

(** The compilation unit for entities defined in the startup file for
    an executable. *)
val startup : t

(** The compilation unit for entities defined in the startup file for
    a shared_library. *)
val shared_startup : t

(** Returns [true] iff the supplied compilation unit corresponds either to the
    startup or shared startup file. *)
val is_startup_or_shared_startup : t -> bool

(** The compilation unit for entities defined in the C runtime code or other
    external libraries. *)
val runtime_and_external_libs : t

(** The compilation unit for predefined exception values. *)
val predefined_exn : t
