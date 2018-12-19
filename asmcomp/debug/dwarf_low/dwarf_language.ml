(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | C89
  | C
  | Ada83
  | C_plus_plus
  | Cobol74
  | Cobol85
  | Fortran77
  | Fortran90
  | Pascal83
  | Modula2
  | Java
  | C99
  | Ada95
  | Fortran95
  | PLI
  | ObjC
  | ObjC_plus_plus
  | UPC
  | D
  | Python
  | OpenCL
  | Go
  | Modula3
  | Haskell
  | C_plus_plus_03
  | C_plus_plus_11
  | OCaml
  | Rust
  | C11
  | Swift
  | Julia
  | Dylan
  | C_plus_plus_14
  | Fortran03
  | Fortran08
  | RenderScript
  | BLISS

let name t =
  match t with
  | C89 -> "C89"
  | C -> "C"
  | Ada83 -> "Ada83"
  | C_plus_plus -> "C_plus_plus"
  | Cobol74 -> "Cobol74"
  | Cobol85 -> "Cobol85"
  | Fortran77 -> "Fortran77"
  | Fortran90 -> "Fortran90"
  | Pascal83 -> "Pascal83"
  | Modula2 -> "Modula2"
  | Java -> "Java"
  | C99 -> "C99"
  | Ada95 -> "Ada95"
  | Fortran95 -> "Fortran95"
  | PLI -> "PLI"
  | ObjC -> "ObjC"
  | ObjC_plus_plus -> "ObjC_plus_plus"
  | UPC -> "UPC"
  | D -> "D"
  | Python -> "Python"
  | OpenCL -> "OpenCL"
  | Go -> "Go"
  | Modula3 -> "Modula3"
  | Haskell -> "Haskell"
  | C_plus_plus_03 -> "C_plus_plus_03"
  | C_plus_plus_11 -> "C_plus_plus_11"
  | OCaml -> "OCaml"
  | Rust -> "Rust"
  | C11 -> "C11"
  | Swift -> "Swift"
  | Julia -> "Julia"
  | Dylan -> "Dylan"
  | C_plus_plus_14 -> "C_plus_plus_14"
  | Fortran03 -> "Fortran03"
  | Fortran08 -> "Fortran08"
  | RenderScript -> "RenderScript"
  | BLISS -> "BLISS"

let encode = function
  | C89 -> 0x01
  | C -> 0x02
  | Ada83 -> 0x03
  | C_plus_plus -> 0x04
  | Cobol74 -> 0x05
  | Cobol85 -> 0x06
  | Fortran77 -> 0x07
  | Fortran90 -> 0x08
  | Pascal83 -> 0x09
  | Modula2 -> 0x0a
  | Java -> 0x0b
  | C99 -> 0x0c
  | Ada95 -> 0x0d
  | Fortran95 -> 0x0e
  | PLI -> 0x0f
  | ObjC -> 0x10
  | ObjC_plus_plus -> 0x11
  | UPC -> 0x12
  | D -> 0x13
  | Python -> 0x14
  | OpenCL -> 0x15
  | Go -> 0x16
  | Modula3 -> 0x17
  | Haskell -> 0x18
  | C_plus_plus_03 -> 0x19
  | C_plus_plus_11 -> 0x1a
  | OCaml -> 0x1b
  | Rust -> 0x1c
  | C11 -> 0x1d
  | Swift -> 0x1e
  | Julia -> 0x1f
  | Dylan -> 0x20
  | C_plus_plus_14 -> 0x21
  | Fortran03 -> 0x22
  | Fortran08 -> 0x23
  | RenderScript -> 0x24
  | BLISS -> 0x25

let as_dwarf_value t =
  Dwarf_value.int8 ~comment:(name t) (Numbers.Int8.of_int_exn (encode t))
