(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Joel Reymont                                     *)
(*                                                                        *)
(*   Copyright 2024 Joel Reymont                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Variable location tracking for DWARF.

    Variables can be in different locations at different points in the
    program (registers, stack, optimized away). This module tracks
    these locations. *)

(** Variable scope - where a variable is visible *)
type scope = {
  start_address : Code_address.t;
  end_address : Code_address.t;
}

(** Location kind *)
type location_kind =
  | Register of int                      (* In register N *)
  | Stack_offset of int                  (* On stack at offset *)
  | Frame_offset of int                  (* Relative to frame pointer *)
  | Optimized_away                       (* Optimized out *)
  | Constant of int                      (* Compile-time constant *)
  | Expression of Dwarf_operator.t list  (* Complex DWARF expression *)

(** A variable location valid for a specific range *)
type location = {
  scope : scope;
  kind : location_kind;
}

(** Variable information *)
type variable = {
  name : string;
  type_ref : int option;  (* Reference to type DIE, if known *)
  locations : location list;
  is_parameter : bool;    (* Function parameter vs. local *)
  is_artificial : bool;   (* Compiler-generated *)
}

(** Create a variable *)
val create_variable :
  name:string ->
  ?type_ref:int ->
  ?is_parameter:bool ->
  ?is_artificial:bool ->
  unit ->
  variable

(** Add a location to a variable *)
val add_location : variable -> location -> variable

(** Get active location at an address *)
val location_at_address : variable -> Code_address.t -> location_kind option

(** Check if variable is visible at address *)
val is_visible_at : variable -> Code_address.t -> bool

(** Convert to DWARF expression bytes *)
val location_to_expression : location_kind -> bytes

(** Pretty-printer *)
val print_variable : Format.formatter -> variable -> unit
val print_location : Format.formatter -> location -> unit
