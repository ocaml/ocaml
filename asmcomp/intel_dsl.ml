(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt       *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(** Helpers for Intel code generators *)

(* The DSL* modules expose functions to emit x86/x86_64 instructions
   using a syntax close to AT&T (in particular, arguments are reversed compared
   to the official Intel syntax).

   Some notes:

     - Unary floating point instructions such as fadd/fmul/fstp/fld/etc come with a single version
       supporting both the single and double precision instructions.  (As with Intel syntax.)

     - A legacy bug in GAS:
       https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
       is not replicated here.  It is managed by Intel_gas.
*)


open Intel_ast
open Intel_proc

module D = struct
  let section segment flags args = directive (Section (segment, flags, args))
  let align n = directive (Align (false, n))
  let byte n = directive (Byte n)
  let bytes s = directive (Bytes s)
  let cfi_adjust_cfa_offset n = directive (Cfi_adjust_cfa_offset n)
  let cfi_endproc () = directive Cfi_endproc
  let cfi_startproc () = directive Cfi_startproc
  let comment s = directive (Comment s)
  let data () = section [ ".data" ] None []
  let extrn s ptr = directive (External (s, ptr))
  let file num filename = directive (File (num, filename))
  let global s = directive (Global s)
  let indirect_symbol s = directive (Indirect_symbol s)
  let label ?(typ = NONE) s = directive (NewLabel (s, typ))
  let loc num loc = directive (Loc (num, loc))
  let long cst = directive (Long cst)
  let mode386 () = directive Mode386
  let model name = directive (Model name)
  let private_extern s = directive (Private_extern s)
  let qword cst = directive (Quad cst)
  let setvar (arg1, arg2) = directive (Set (arg1, arg2))
  let size name cst = directive (Size (name, cst))
  let space n = directive (Space n)
  let text () = section [ ".text" ] None []
  let type_ name typ = directive (Type (name, typ))
  let word cst = directive (Word cst)
end

module DSL = struct
  let sym s = Sym s

  let emit_nat n = Imm (Int64.of_nativeint n)
  let int n = Imm (Int64.of_int n)

  let const_64 n = Const n
  let const_32 n = Const (Int64.of_int32 n)
  let const_nat n = Const (Int64.of_nativeint n)
  let const n = Const (Int64.of_int n)


  let al  = Reg8L RAX
  let ah  = Reg8H AH
  let cl  = Reg8L RCX

  let ax  = Reg16 RAX

  let rax = Reg64 RAX
  let r10 = Reg64 R10
  let r11 = Reg64 R11
  let r14 = Reg64 R14
  let r15 = Reg64 R15
  let rsp = Reg64 RSP
  let rbp = Reg64 RBP
  let xmm15 = Regf (XMM 15)
  let eax = Reg32 RAX
  let ebx = Reg32 RBX
  let ecx = Reg32 RCX
  let edx = Reg32 RDX
  let ebp = Reg32 RBP
  let esp = Reg32 RSP
  let st0 = Regf (ST 0)
  let st1 = Regf (ST 1)

  let mem32 typ ?(scale = 1) ?base ?sym displ idx =
    assert(scale > 0);
    Mem32 {typ; idx; scale; base; sym; displ}

  let mem64 typ ?(scale = 1) ?base ?sym offset idx =
    assert(scale > 0);
    Mem64 {typ; idx; scale; base; sym; displ=offset}

  let mem64_rip typ ?(ofs = 0) s =
    Mem64_RIP (typ, s, ofs)
end

module I = struct
  let add (x, y) = emit (ADD (x, y))
  let addsd (arg1, arg2) = emit (ADDSD (arg1, arg2))
  let and_ (x, y)= emit (AND (x, y))
  let andpd (arg1, arg2) = emit (ANDPD (arg1, arg2))
  let bswap arg = emit (BSWAP arg)
  let call arg = emit (CALL arg)
  let cdq () = emit CDQ
  let cmp (x, y) = emit (CMP (x, y))
  let comisd (arg1, arg2) = emit (COMISD (arg1, arg2))
  let cqo () = emit CQO
  let cvtsd2ss (arg1, arg2) = emit (CVTSD2SS (arg1, arg2))
  let cvtsi2sd (arg1, arg2) = emit (CVTSI2SD (arg1, arg2))
  let cvtss2sd (arg1, arg2) = emit (CVTSS2SD (arg1, arg2))
  let cvttsd2si (arg1, arg2) = emit (CVTTSD2SI (arg1, arg2))
  let dec arg = emit (DEC arg)
  let divsd (arg1, arg2) = emit (DIVSD (arg1, arg2))
  let fabs () = emit FABS
  let fadd x = emit (FADD x)
  let faddp (arg1, arg2) = emit (FADDP (arg1, arg2))
  let fchs () = emit FCHS
  let fcomp arg = emit (FCOMP arg)
  let fcompp () = emit FCOMPP
  let fcos () = emit FCOS
  let fdiv x = emit (FDIV x)
  let fdivp (arg1, arg2) = emit (FDIVP (arg1, arg2))
  let fdivr x = emit (FDIVR x)
  let fdivrp (arg1, arg2) = emit (FDIVRP (arg1, arg2))
  let fild arg = emit (FILD arg)
  let fistp arg = emit (FISTP arg)
  let fld arg = emit (FLD arg)
  let fld1 () = emit FLD1
  let fldcw arg = emit (FLDCW arg)
  let fldlg2 () = emit FLDLG2
  let fldln2 () = emit FLDLN2
  let fldz () = emit FLDZ
  let fmul x = emit (FMUL x)
  let fmulp (arg1, arg2) = emit (FMULP (arg1, arg2))
  let fnstcw arg = emit (FNSTCW arg)
  let fnstsw arg = emit (FNSTSW arg)
  let fpatan () = emit FPATAN
  let fptan () = emit FPTAN
  let fsin () = emit FSIN
  let fsqrt () = emit FSQRT
  let fstp arg = emit (FSTP arg)
  let fsub x = emit (FSUB x)
  let fsubp (arg1, arg2) = emit (FSUBP (arg1, arg2))
  let fsubr x = emit (FSUBR x)
  let fsubrp (arg1, arg2) = emit (FSUBRP (arg1, arg2))
  let fxch arg = emit (FXCH arg)
  let fyl2x () = emit FYL2X
  let hlt () = emit HLT
  let idiv arg = emit (IDIV arg)
  let imul (arg1, arg2) = emit (IMUL (arg1, arg2))
  let inc arg = emit (INC arg)
  let j cond arg = emit (J (cond, arg))
  let ja = j A
  let jae = j AE
  let jb = j B
  let jbe = j BE
  let je = j E
  let jg = j G
  let jmp arg = emit (JMP arg)
  let jne = j NE
  let jp = j P
  let lea (arg1, arg2) = emit (LEA (arg1, arg2))
  let mov (arg1, arg2) = emit (MOV (arg1, arg2))
  let movapd (arg1, arg2) = emit (MOVAPD (arg1, arg2))
  let movsd (arg1, arg2) = emit (MOVSD (arg1, arg2))
  let movss (arg1, arg2) = emit (MOVSS (arg1, arg2))
  let movsx (x, y) = emit (MOVSX (x, y))
  let movsxd (arg1, arg2) = emit (MOVSXD  (arg1, arg2))
  let movzx (x, y) = emit (MOVZX (x, y))
  let mulsd (arg1, arg2) = emit (MULSD (arg1, arg2))
  let nop () = emit NOP
  let or_ (x, y) = emit (OR (x, y))
  let pop arg = emit (POP arg)
  let push arg = emit (PUSH arg)
  let ret () = emit RET
  let sal (arg1, arg2) = emit (SAL (arg1, arg2))
  let sar (arg1, arg2) = emit (SAR (arg1, arg2))
  let set cond arg = emit (SET (cond, arg))
  let shr (arg1, arg2) = emit (SHR (arg1, arg2))
  let sqrtsd (arg1, arg2) = emit (SQRTSD (arg1, arg2))
  let sub (x, y) = emit (SUB (x, y))
  let subsd  (arg1, arg2) = emit (SUBSD (arg1, arg2))
  let test (x, y)= emit (TEST (x, y))
  let ucomisd (arg1, arg2) = emit (UCOMISD (arg1, arg2))
  let xchg (arg1, arg2) = emit (XCHG (arg1, arg2))
  let xor (x, y)= emit (XOR (x, y))
  let xorpd (arg1, arg2) = emit (XORPD (arg1, arg2))
end
