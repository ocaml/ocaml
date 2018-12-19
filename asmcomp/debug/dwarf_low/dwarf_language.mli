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

(** DWARF languages (DWARF-5 specification page 62, table 3.1). *)

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

val as_dwarf_value : t -> Dwarf_value.t
