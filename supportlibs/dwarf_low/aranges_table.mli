(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Tables for faster lookup of program entities by address
    (.debug_aranges, DWARF-4 standard section 6.1.2).
*)

type t

include Dwarf_emittable.S with type t := t

val create : start_of_code_symbol:Symbol.t
  -> end_of_code_symbol:Symbol.t
  -> debug_info_label:Linearize.label
  -> t
