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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Middle-end symbols: names of statically-allocated constants.  These
    symbols are used inside Closure and Flambda.  They always point at
    well-formed OCaml values in a data section, never at code. *)

(** The type of middle-end symbols. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Like [print], but for values of type [t option]. *)
val print_opt : Format.formatter -> t option -> unit

(** Create a symbol to correspond to a pre-existing Flambda variable. *)
val of_variable : Variable.t -> t

(** Create a symbol to correspond to the given identifier, which must be
    persistent, in the current (or given) compilation unit. *)
val of_global : ?compilation_unit:Compilation_unit.t -> Ident.t -> t

(** Return the "base symbol" for the given compilation unit.  This takes into
    account any "-for-pack" prefix.  The base symbol will be the same as the
    symbol used for the compilation unit's toplevel module block; and will
    form the prefix of other symbols (for example those of functions and
    data) within the compilation unit. *)
val base_symbol_for_unit : Compilation_unit.t -> t

(** Adjust the given symbol's compilation unit to that specified by [pack]. *)
(* CR mshinwell: check usage of this *)
val import_for_pack : t -> pack:Compilation_unit.t -> t

(** The compilation unit in which the given symbol's definition lies. *)
val compilation_unit : t -> Compilation_unit.t

(** Whether the given symbol is that of a predefined exception. *)
val is_predefined_exn : t -> bool

(** The name of the symbol as to be used for construction of [Backend_sym.t]
    values. *)
val name_for_backend : t -> string
