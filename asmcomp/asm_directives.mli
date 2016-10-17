(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Emission of assembler directives that are supported on multiple targets. *)

include Asm_directives_intf.S

module Directive : sig
  (** Internal representation of directives.  Only needed if writing a custom
      assembler or printer instead of using [print], below. *)
  type t = private
    | Align of { bytes : int; }
    | Bytes of string
    | Comment of string
    | Global of string
    | Const8 of constant
    | Const16 of constant
    | Const32 of constant
    | Const64 of constant
    | NewLabel of string
    | Section of string list * string option * string list
    | Space of { bytes : int; }
    (* gas only (the masm emitter will fail on them): *)
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_startproc
    | File of { file_num : int; filename : string; }
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | Private_extern of string
    (* Note that on Mac OS X, [Set] always makes the expression absolute. *)
    | Set of string * constant
    | Size of string * constant
    | Sleb128 of constant
    | Type of string * string
    | Uleb128 of constant
    (* Mac OS X only: *)
    | Direct_assignment of string * constant

  (** Translate the given directive to textual form.  This produces output
      suitable for either gas or MASM as appropriate. *)
  val print : Buffer.t -> t -> unit
end

(** To be called by the emitter at the very start of code generation.
    Calling the functions below will cause directives to be passed to the
    given [emit] function. *)
val initialize : emit:(Directive.t -> unit) -> unit

(** Reinitialize the emitter before compiling a different source file. *)
val reset : unit -> unit
