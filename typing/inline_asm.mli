(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                    Vladimir Brankov, Jane Street                    *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** The description of inline assembly primitives necessary for being handled by
    the various parts of the compiler. *)

open Inline_asm_arch

exception Inline_asm_error of string

(** OCaml argument kinds that the primitive.  Necessary for determining
    boxing/unboxing and the kind of register kind for the result. *)
type arg_kind =
  [ `Addr
  | `Float
  | `Int
  | `Int32
  | `Int64
  | `M128d (* 128-bit packed floats *)
  | `M256d (* 256-bit packed floats *)
  | `M128i (* 128-bit packed integers *)
  | `M256i (* 256-bit packed integers *)
  | `Nativeint
  | `Unit ]

(** Specifies a way an inline assembly argument can be handled.  An inline
    assembly primitive can have more such alternatives, and the cheapest one is
    chosen when the primitive is used.  For example, an argument can
    alternatively come from a register or memory.  If the provided argument is
    in memory, the later alternative will be chosen because it is cheaper than
    pulling the argument into a register first. *)
type alternative = {
  mach_register    : register option;
  (** If given, argument must reside in the given register *)
  copy_to_output   : int option;
  (** If given, the input and the output arguments should effectively be a
      single operand.  For example, this is used when an assembly instruction
      reads input and stores the result in the same register. *)
  commutative      : bool;
  (** If set, declares the primitive to be commutative for this operand and the
      following operand *)
  disparage        : int;
  (** The amount by which the alternative should be disparaged over other
      alternatives *)
  earlyclobber     : bool;
  (** If set, the operand is written before the instruction is finished using
      the input operands.  This means that the output cannot share space with
      any of the inputs. *)
  immediate        : bool;
  (** If set, an integer constant is allowed as the argument and it is used by
      the assembly as a constant *)
  memory           : [ `no | `m8 | `m16 | `m32 | `m64 | `m128 | `m256 ];
  (** Indicates whether the argument can come from memory and which minimal
      memory alignment is required *)
  reload_disparage : int;
  (** The amount by which the alternative should be disparaged over other
      alternatives when the argument is coming from memory *)
  register         : bool
  (** If set, the argument can come from a register *)
}

(** Specifies an argument with all its alternative parameters *)
type arg = {
  kind         : arg_kind;
  input        : bool;
  output       : bool;
  alternatives : alternative array }

(** x86 specific modifiers used in the assembly template to affect the way the
    operands are formatted in the code. *)
type arg_modifier =
  | None
  | R8L (* low 8-bit name (AL, BL, ...) *)
  | R8H (* high 8-bit name (AH, BH, ...) *)
  | R16 (* 16-bit name (AX, BX) *)
  | R32 (* 32-bit name (EAX, EBX) *)
  | R64 (* 64-bit name (RAX, RBX) *)
  | XMM (* SSE *)
  | YMM (* AVX *)

(** Parsed assembly template item *)
type template_item =
    Emit_arg of int * arg_modifier
  (** A token in the assembly template which refers to the input or output
      arguments, for example "%0" and "%1" in "addq %0, %1" *)
  | Emit_dialect of template array
  (* If the assembly specifies multiple dialects, for example GAS and MASM, a
     separate branch for each dialect *)
  | Emit_string of string
  (* Fixed text *)
  | Emit_unique
  (* A number unique for each instance of the generated assembly code *)
  | Record_frame
  (* Records the stack frame *)
and template = template_item array

(** A full specification of the inline assembly primitive *)
type inline_asm = {
  template       : template;
  args           : arg array;
  clobber_cc     : bool;
  (** If set, the assembly code modifies the flags register *)
  clobber_memory : bool;
  (** If set, the assembly performs memory reads and writes to items other than
      those listed in the input and output operands *)
  arch_specifics : arch_specific list;
  (** Architecture specific info *)
  decl           : string array
  (** The raw string declaration of the assembly primitive *)
}

val parse: arg_kind list -> string list -> inline_asm

val name : inline_asm -> string
val description : inline_asm -> string list
val bytecode_call : inline_asm -> string
