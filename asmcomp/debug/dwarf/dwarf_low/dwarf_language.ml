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

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

(* DWARF 4 specification section 7.12 *)
let to_code = function
  | DW_LANG_C89 -> 0x0001
  | DW_LANG_C -> 0x0002
  | DW_LANG_Ada83 -> 0x0003
  | DW_LANG_C_plus_plus -> 0x0004
  | DW_LANG_Cobol74 -> 0x0005
  | DW_LANG_Cobol85 -> 0x0006
  | DW_LANG_Fortran77 -> 0x0007
  | DW_LANG_Fortran90 -> 0x0008
  | DW_LANG_Pascal83 -> 0x0009
  | DW_LANG_Modula2 -> 0x000a
  | DW_LANG_Java -> 0x000b
  | DW_LANG_C99 -> 0x000c
  | DW_LANG_Ada95 -> 0x000d
  | DW_LANG_Fortran95 -> 0x000e
  | DW_LANG_PLI -> 0x000f
  | DW_LANG_ObjC -> 0x0010
  | DW_LANG_ObjC_plus_plus -> 0x0011
  | DW_LANG_UPC -> 0x0012
  | DW_LANG_D -> 0x0013
  | DW_LANG_Python -> 0x0014
  | DW_LANG_C11 -> 0x001d
  | DW_LANG_Swift -> 0x001e
  | DW_LANG_Julia -> 0x001f
  | DW_LANG_Dylan -> 0x0020
  | DW_LANG_C_plus_plus_14 -> 0x0021
  | DW_LANG_Fortran03 -> 0x0022
  | DW_LANG_Fortran08 -> 0x0023
  (* OCaml uses vendor extension range (>= 0x8000) to avoid conflicts *)
  | DW_LANG_OCaml -> 0x8001
  | DW_LANG_RenderScript -> 0x0024
  | DW_LANG_BLISS -> 0x0025
  | DW_LANG_Rust -> 0x001c
  | DW_LANG_C_plus_plus_11 -> 0x001a
  | DW_LANG_C_plus_plus_17 -> 0x002a

let to_string = function
  | DW_LANG_C89 -> "DW_LANG_C89"
  | DW_LANG_C -> "DW_LANG_C"
  | DW_LANG_Ada83 -> "DW_LANG_Ada83"
  | DW_LANG_C_plus_plus -> "DW_LANG_C_plus_plus"
  | DW_LANG_Cobol74 -> "DW_LANG_Cobol74"
  | DW_LANG_Cobol85 -> "DW_LANG_Cobol85"
  | DW_LANG_Fortran77 -> "DW_LANG_Fortran77"
  | DW_LANG_Fortran90 -> "DW_LANG_Fortran90"
  | DW_LANG_Pascal83 -> "DW_LANG_Pascal83"
  | DW_LANG_Modula2 -> "DW_LANG_Modula2"
  | DW_LANG_Java -> "DW_LANG_Java"
  | DW_LANG_C99 -> "DW_LANG_C99"
  | DW_LANG_Ada95 -> "DW_LANG_Ada95"
  | DW_LANG_Fortran95 -> "DW_LANG_Fortran95"
  | DW_LANG_PLI -> "DW_LANG_PLI"
  | DW_LANG_ObjC -> "DW_LANG_ObjC"
  | DW_LANG_ObjC_plus_plus -> "DW_LANG_ObjC_plus_plus"
  | DW_LANG_UPC -> "DW_LANG_UPC"
  | DW_LANG_D -> "DW_LANG_D"
  | DW_LANG_Python -> "DW_LANG_Python"
  | DW_LANG_OCaml -> "DW_LANG_OCaml"
  | DW_LANG_C11 -> "DW_LANG_C11"
  | DW_LANG_Swift -> "DW_LANG_Swift"
  | DW_LANG_Julia -> "DW_LANG_Julia"
  | DW_LANG_Dylan -> "DW_LANG_Dylan"
  | DW_LANG_C_plus_plus_14 -> "DW_LANG_C_plus_plus_14"
  | DW_LANG_Fortran03 -> "DW_LANG_Fortran03"
  | DW_LANG_Fortran08 -> "DW_LANG_Fortran08"
  | DW_LANG_RenderScript -> "DW_LANG_RenderScript"
  | DW_LANG_BLISS -> "DW_LANG_BLISS"
  | DW_LANG_Rust -> "DW_LANG_Rust"
  | DW_LANG_C_plus_plus_11 -> "DW_LANG_C_plus_plus_11"
  | DW_LANG_C_plus_plus_17 -> "DW_LANG_C_plus_plus_17"

let ocaml = DW_LANG_OCaml

let print ppf lang =
  Format.fprintf ppf "%s" (to_string lang)
