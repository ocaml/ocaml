(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** DWARF language codes.

    These codes identify the source language of the compilation unit.
    See DWARF 4 specification section 7.12. *)

type t =
  | DW_LANG_C89
  | DW_LANG_C
  | DW_LANG_Ada83
  | DW_LANG_C_plus_plus
  | DW_LANG_Cobol74
  | DW_LANG_Cobol85
  | DW_LANG_Fortran77
  | DW_LANG_Fortran90
  | DW_LANG_Pascal83
  | DW_LANG_Modula2
  | DW_LANG_Java
  | DW_LANG_C99
  | DW_LANG_Ada95
  | DW_LANG_Fortran95
  | DW_LANG_PLI
  | DW_LANG_ObjC
  | DW_LANG_ObjC_plus_plus
  | DW_LANG_UPC
  | DW_LANG_D
  | DW_LANG_Python
  | DW_LANG_OCaml
  | DW_LANG_C11
  | DW_LANG_Swift
  | DW_LANG_Julia
  | DW_LANG_Dylan
  | DW_LANG_C_plus_plus_14
  | DW_LANG_Fortran03
  | DW_LANG_Fortran08
  | DW_LANG_RenderScript
  | DW_LANG_BLISS
  | DW_LANG_Rust
  | DW_LANG_C_plus_plus_11
  | DW_LANG_C_plus_plus_17

(** Convert a language to its numeric code *)
val to_code : t -> int

(** Convert a language to a human-readable string *)
val to_string : t -> string

(** The language code for OCaml *)
val ocaml : t

(** Pretty-printer for languages *)
val print : Format.formatter -> t -> unit
