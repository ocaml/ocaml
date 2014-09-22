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
  | O | NO
  | B | C | NAE | NB | NC | AE
  | Z | E | NZ | NE
  | BE | NA | NBE | A
  | S | NS
  | P | PE | NP | PO
  | L | NGE | NL | GE
  | LE | NG | NLE | G

type rounding =
  | RoundUp
  | RoundDown
  | RoundNearest
  | RoundTruncate

type constant =
  | Const of int64
  | ConstThis
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


type 'reg addr =
  {
    typ: data_type;
    idx: 'reg;
    scale: int;
    base: 'reg option;
    sym: string option;
    displ: int64;
  }
  (** Addressing modes:
      displ + sym + base + idx * scale
      (if scale = 0, idx is ignored and base must be None)
  *)

type arg =
  | Imm of int64
  (** Operand is an immediate constant integer *)

  | Sym of  string
  (** Address of a symbol (absolute address except for call/jmp target
      where it is interpreted as a relative displacement *)

  | Reg8 of register8
  | Reg16 of register16
  | Reg32 of register32
  | Reg64 of register64
  | Regf of registerf

  | Mem32 of register32 addr
  | Mem64 of register64 addr

type instruction =
  | ADD of arg * arg
  | ADDSD of arg * arg
  | AND of arg * arg
  | ANDPD of arg * arg
  | BSWAP of arg
  | CALL of arg
  | CDQ
  | CMOV of condition * arg * arg
  | CMP of arg * arg
  | COMISD of arg * arg
  | CQTO
  | CVTSD2SI of arg * arg
  | CVTSD2SS of arg * arg
  | CVTSI2SD of arg * arg
  | CVTSS2SD of arg * arg
  | CVTTSD2SI of arg * arg
  | DEC of arg
  | DIVSD of arg * arg
  | FABS
  | FADD of arg
  | FADDP of arg * arg
  | FCHS
  | FCOMP of arg
  | FCOMPP
  | FCOS
  | FDIV of arg
  | FDIVP of arg * arg
  | FDIVR of arg
  | FDIVRP of arg * arg
  | FILD of arg
  | FISTP of arg
  | FLD of arg
  | FLD1
  | FLDCW of arg
  | FLDLG2
  | FLDLN2
  | FLDZ
  | FMUL of arg
  | FMULP of arg * arg
  | FNSTCW of arg
  | FNSTSW of arg
  | FPATAN
  | FPTAN
  | FSIN
  | FSQRT
  | FSTP of arg
  | FSUB of arg
  | FSUBP of arg * arg
  | FSUBR of arg
  | FSUBRP of arg * arg
  | FXCH of arg
  | FYL2X
  | HLT
  | IDIV of arg
  | IMUL of arg * arg option
  | INC of arg
  | J of condition * arg
  | JMP of arg
  | LEA of arg * arg
  | LEAVE
  | MOV of arg * arg
  | MOVAPD of arg * arg
  | MOVLPD of arg * arg
  | MOVSD of arg * arg
  | MOVSS of arg * arg
  | MOVSX of arg * arg
  | MOVSXD of arg * arg
  | MOVZX of arg * arg
  | MULSD of arg * arg
  | NEG of arg
  | NOP
  | OR of arg * arg
  | POP of arg
  | PUSH of arg
  | RET
  | ROUNDSD of rounding * arg * arg
  | SAL of arg * arg
  | SAR of arg * arg
  | SET of condition * arg
  | SHR of arg * arg
  | SQRTSD of arg * arg
  | SUB of arg * arg
  | SUBSD of arg * arg
  | TEST of arg * arg
  | UCOMISD of arg * arg
  | XCHG of arg * arg
  | XOR of arg * arg
  | XORPD of arg * arg

type asm_line =
  | Ins of instruction

  | Align of bool * int
  | Byte of constant
  | Bytes of string
  | Comment of string
  | End
  | Global of string
  | Long of constant
  | NewLabel of string * data_type
  | Quad of constant
  | Section of string list * string option * string list
  | Space of int
  | Word of constant

  (* masm only *)
  | External of string * data_type
  | Mode386
  | Model of string

  (* gas only *)
  | Cfi_adjust_cfa_offset of int
  | Cfi_endproc
  | Cfi_startproc
  | File of int * string (* file_num * filename *)
  | Indirect_symbol of string
  | Loc of int * int (* file_num x line *)
  | Private_extern of string
  | Set of string * constant
  | Size of string * constant
  | Type of string * string

type asm_program = asm_line list
