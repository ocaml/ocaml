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

(** Builder for DWARF location and expression operations.

    This module provides a fluent interface for constructing DWARF
    location descriptions and expressions. These are used to describe
    where variables are located (in registers, on the stack, in memory)
    and how to compute their addresses.

    Example:
      builder
      |> push_reg x0
      |> add_constant 16
      |> deref
      |> to_bytes
*)

type t

(** Create an empty operator builder *)
val create : unit -> t

(** Push a register value onto the expression stack *)
val push_reg : t -> int -> t

(** Push a constant onto the expression stack *)
val push_constant : t -> int -> t

(** Push a large constant (64-bit) *)
val push_constant64 : t -> int64 -> t

(** Add top two stack values *)
val add : t -> t

(** Subtract top two stack values (TOS-1 - TOS) *)
val subtract : t -> t

(** Add unsigned constant to TOS *)
val add_constant : t -> int -> t

(** Dereference TOS (load from memory at address TOS) *)
val deref : t -> t

(** Dereference with size *)
val deref_size : t -> int -> t

(** Frame base relative addressing *)
val frame_base_offset : t -> int -> t

(** Stack pointer relative addressing *)
val stack_offset : t -> int -> t

(** Register + offset (bregN) *)
val reg_offset : t -> int -> int -> t

(** Duplicate top of stack *)
val dup : t -> t

(** Drop top of stack *)
val drop : t -> t

(** Mark value as in a piece (for composite locations) *)
val piece : t -> int -> t

(** Mark as stack value (no memory dereference needed) *)
val stack_value : t -> t

(** Get call frame CFA (canonical frame address) *)
val call_frame_cfa : t -> t

(** Emit the operators as bytes *)
val to_bytes : t -> bytes

(** Get the list of operators *)
val operators : t -> Dwarf_operator.t list

(** Pretty-printer *)
val print : Format.formatter -> t -> unit

(** Common patterns *)

(** Variable in register N *)
val in_register : int -> bytes

(** Variable at [frame_pointer + offset] *)
val at_frame_offset : int -> bytes

(** Variable at [stack_pointer + offset] *)
val at_stack_offset : int -> bytes

(** Variable at [register N + offset] *)
val at_reg_offset : reg:int -> offset:int -> bytes
