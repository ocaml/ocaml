(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Structured representation of Intel assembly language (32 and 64 bit). *)

type condition =
  | O
  | NO
  | B | C | NAE
  | NB | NC | AE
  | Z | E
  | NZ | NE
  | BE | NA
  | NBE | A
  | S
  | NS
  | P | PE
  | NP | PO
  | L | NGE
  | NL | GE
  | LE | NG
  | NLE | G

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate

type reloc_table =
  | PLT
  | GOTPCREL

(* When an integer is immediate, the [data_size] information must
   indicate in which range it is. For symbols, the [data_size] size
   depends on how the symbol will be used, i.e. B32 for displacements
   and B64 for immediate in 64 bits. *)

type data_size =
  | B8 | B16 | B32 | B64

type constant =
  | Const of data_size * int64
  | ConstLabel of string
  | ConstAdd of constant * constant
  | ConstSub of constant * constant

type data_type = (* only used for MASM *)
  | NO
  | REAL4 | REAL8 | REAL10 (* floating point values *)
  | BYTE | WORD | DWORD | QWORD | TBYTE  | OWORD (* integer values *)
  | NEAR | PROC
  (* PROC could be a display for NEAR on 32 bits ? *)

type register64 =
  | RAX | RBX | RDI | RSI | RDX | RCX | RBP | RSP
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
  | RIP

type register8 =
  | AL | BL | CL | DL
  | AH | BH | CH | DH
  | DIL | SIL | R8B | R9B
  | R10B | R11B | BPL | R12B | R13B | SPL | R14B | R15B

type register16 =
  | AX | BX | DI | SI | DX | CX | SP | BP
  | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W

type register32 =
  | EAX | EBX | EDI | ESI | EDX | ECX | EBP | ESP
  | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D

type registerf = XMM of int | TOS | ST of int

type symbol = string * reloc_table option

(* A direct value is a combination of:
   * an integer offset
   * a symbol
*)
type offset = symbol option * int64

type 'reg addr = data_type * ('reg * (* scale *) int * 'reg option) option * offset

type arg =
  (* operand is an immediate value *)
  | Imm of data_size * offset
  (* operand is a relative displacement *)
  | Rel of data_size * symbol

  | Reg8 of register8
  | Reg16 of register16
  | Reg32 of register32
  | Reg64 of register64
  | Regf of registerf
  | Mem32 of register32 addr
  | Mem64 of register64 addr

type instruction =
  | NOP

  | ADD of arg * arg
  | SUB of arg * arg
  | XOR of arg * arg
  | OR of arg * arg
  | AND of arg * arg
  | CMP of arg * arg
  | LEA of arg * arg

  | FSTP of arg

  | FCOMPP
  | FCOMP of arg
  | FLD of arg
  | FNSTSW of arg
  | FNSTCW of arg
  | FLDCW of arg

  | HLT
  | FCHS
  | FABS
  | FLD1
  | FPATAN
  | FPTAN
  | FCOS
  | FLDLN2
  | FLDLG2
  | FYL2X
  | FSIN
  | FSQRT
  | FLDZ

  | FADD of arg
  | FSUB of arg
  | FMUL of arg
  | FDIV of arg
  | FSUBR of arg
  | FDIVR of arg
  | FILD of arg
  | FISTP of arg
  | FXCH of arg

  | FADDP of arg * arg
  | FSUBP of arg * arg
  | FMULP of arg * arg
  | FDIVP of arg * arg
  | FSUBRP of arg * arg
  | FDIVRP of arg * arg


  | SAR of arg * arg
  | SHR of arg * arg
  | SAL of arg * arg
  | INC of arg
  | DEC of arg
  | IMUL of arg * arg option
  | IDIV of arg
  | PUSH of arg
  | POP of arg

  | MOV of arg * arg

  | MOVZX of arg * arg
  | MOVSX of arg * arg
  | MOVSS of arg * arg
  | MOVSXD (* MOVSLQ *) of arg * arg

  | MOVSD of arg * arg
  | ADDSD of arg * arg
  | SUBSD of arg * arg
  | MULSD of arg * arg
  | DIVSD of arg * arg
  | SQRTSD of arg * arg
  | ROUNDSD of rounding * arg * arg
  | NEG of arg

  | CVTSS2SD of arg * arg
  | CVTSD2SS of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSD2SI of arg * arg
  | CVTTSD2SI of arg * arg
  | UCOMISD of arg * arg
  | COMISD of arg * arg

  | CALL of arg
  | JMP of arg
  | RET

  | TEST of arg * arg
  | SET of condition * arg
  | J of condition * arg

  | CMOV of condition * arg * arg
  | XORPD of arg * arg
  | ANDPD of arg * arg
  | MOVAPD of arg * arg
  | MOVLPD of arg * arg

  | CDQ
  | CQTO
  | LEAVE

  | XCHG of arg * arg
  | BSWAP of arg

type asm_line =
  | Section of string list * string option * string list
  | Global of string
  | Constant of constant * data_size
  | Align of bool * int
  | NewLabel of string * data_type
  | Bytes of string
  | Space of int
  | Comment of string
  | External of string * data_type
  | Set of string * constant
  | End
  (* Windows only ? *)
  | Mode386
  | Model of string
  (* Unix only ? *)
  | Cfi_startproc
  | Cfi_endproc
  | Cfi_adjust_cfa_offset of int
  | File of int * string (* file_num * filename *)
  | Loc of int * int (* file_num x line *)
  | Private_extern of string
  | Indirect_symbol of string
  | Type of string * string
  | Size of string * constant

  | Ins of instruction

type asm_program = asm_line list
