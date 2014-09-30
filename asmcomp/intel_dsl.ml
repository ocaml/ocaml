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

module DSL = struct
  let sym s = Sym s

  let emit_nat n = Imm (Int64.of_nativeint n)
  let int n = Imm (Int64.of_int n)

  let const_64 n = Const n
  let const_32 n = Const (Int64.of_int32 n)
  let const_nat n = Const (Int64.of_nativeint n)
  let const n = Const (Int64.of_int n)

  let _cfi_startproc () = directive Cfi_startproc
  let _cfi_endproc () = directive Cfi_endproc
  let _cfi_adjust_cfa_offset n = directive (Cfi_adjust_cfa_offset n)
  let _file num filename = directive (File (num, filename))
  let _loc num loc = directive (Loc (num, loc))
  let _section segment flags args = directive (Section (segment, flags, args))
  let _text () = _section [ ".text" ] None []
  let _data () = _section [ ".data" ] None []
  let _section segment flags args = directive (Section (segment, flags, args))
  let _386 () = directive Mode386
  let _model name = directive (Model name)
  let _global s = directive (Global s)
  let _align n = directive (Align (false, n))
  let _llabel s = directive (NewLabel (s, NONE)) (* local label *)
  let _comment s = directive (Comment s)
  let _extrn s ptr = directive (External (s, ptr))
  let _private_extern s = directive (Private_extern s)
  let _indirect_symbol s = directive (Indirect_symbol s)
  let _size name cst = directive (Size (name, cst))
  let _type name typ = directive (Type (name, typ))

  let _qword cst = directive (Quad cst)
  let _long cst = directive (Long cst)
  let _word cst = directive (Word cst)
  let _byte n = directive (Byte n)
  let _ascii s = directive (Bytes s)
  let _space n = directive (Space n)
  let _setvar (arg1, arg2) = directive (Set (arg1, arg2))
  let _end () = directive End

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
end

module INS = struct

  let jmp arg = emit (JMP arg)
  let call arg = emit (CALL arg)
  let set cond arg = emit (SET (cond, arg))

  let j cond arg = emit (J (cond, arg))
  let je = j E
  let jae = j AE
  let jb = j B
  let jg = j G
  let jbe = j BE
  let ja = j A
  let jne = j NE
  let jp = j P

  let ret () = emit RET
  let hlt () = emit HLT
  let nop () = emit NOP

  (* Word mnemonics *)
  let movw (arg1, arg2) = emit (MOV (arg1, arg2))

  (* Byte mnemonics *)
  let decb arg = emit (DEC arg)
  let cmpb (x, y) = emit (CMP (x, y))
  let movb (x, y) = emit (MOV (x, y))
  let andb (x, y)= emit (AND (x, y))
  let xorb (x, y)= emit (XOR (x, y))
  let testb (x, y)= emit (TEST (x, y))

  (* Long-mnemonics *)
  let movl (x, y) = emit (MOV (x, y))
end

module INS32 = struct

  include INS

  (* Long-mnemonics *)
  let addl (x, y) = emit (ADD (x, y))
  let subl (x, y) = emit (SUB (x, y))
  let andl (x, y) = emit (AND (x, y))
  let orl (x, y) = emit (OR (x, y))
  let xorl (x, y) = emit (XOR (x, y))
  let cmpl (x, y) = emit (CMP (x, y))
  let testl (x, y) = emit (TEST (x, y))

  let movzbl (x, y) = emit (MOVZX (x, y))
  let movsbl (x, y) = emit (MOVSX (x, y))
  let movzwl (x, y) = emit (MOVZX (x, y))
  let movswl (x, y) = emit (MOVSX (x, y))

  let sall (arg1, arg2) = emit (SAL  (arg1, arg2))
  let sarl (arg1, arg2) = emit (SAR  (arg1, arg2))
  let shrl (arg1, arg2) = emit (SHR  (arg1, arg2))
  let imull (arg1, arg2) = emit (IMUL (arg1, arg2))

  let idivl arg = emit (IDIV arg)
  let popl arg = emit (POP arg)
  let pushl arg = emit (PUSH arg)
  let decl arg = emit (DEC arg)
  let incl arg = emit (INC arg)
  let leal (arg1, arg2) = emit (LEA (arg1, arg2))

  let fistpl arg = emit (FISTP arg)
  let fildl arg = emit (FILD arg)

  let fchs () = emit FCHS
  let fabs () = emit FABS

  let fadd x = emit (FADD x)
  let fsub x = emit (FSUB x)
  let fdiv x = emit (FDIV x)
  let fmul x = emit (FMUL x)
  let fsubr x = emit (FSUBR x)
  let fdivr x = emit (FDIVR x)

  let faddp (arg1, arg2) = emit (FADDP (arg1, arg2))
  let fmulp (arg1, arg2) = emit (FMULP (arg1, arg2))
  let fcompp () = emit FCOMPP
  let fcomp arg = emit (FCOMP arg)
  let fld arg = emit (FLD arg)
  let fnstsw arg = emit (FNSTSW arg)
  let fld1 () = emit FLD1
  let fpatan () = emit FPATAN
  let fptan () = emit FPTAN
  let fcos () = emit FCOS
  let fldln2 () = emit FLDLN2
  let fldlg2 () = emit FLDLG2
  let fxch arg = emit (FXCH arg)
  let fyl2x () = emit FYL2X
  let fsin () = emit FSIN
  let fsqrt () = emit FSQRT
  let fstp arg = emit (FSTP arg)
  let fldz () = emit FLDZ
  let fnstcw arg = emit (FNSTCW arg)
  let fldcw arg = emit (FLDCW arg)
  let cltd () = emit CDQ

  let fsubp (arg1, arg2) = emit (FSUBP (arg1, arg2))
  let fsubrp (arg1, arg2) = emit (FSUBRP (arg1, arg2))
  let fdivp (arg1, arg2) = emit (FDIVP (arg1, arg2))
  let fdivrp (arg1, arg2) = emit (FDIVRP (arg1, arg2))
end

module DSL32 = struct
  include DSL

  let _label s = directive (NewLabel (s, DWORD))

  let mem_ptr typ ?(scale = 1) ?base ?sym displ idx =
    assert(scale > 0);
    Mem32 {typ; idx; scale; base; sym; displ}

  let mem_sym typ ?(ofs = 0) l =
    Mem32 {typ; idx=RAX; scale=0; base=None; sym=Some l; displ=ofs}
end


module INS64 = struct
  include INS

  let add (x, y) = emit (ADD (x, y))
  let addsd (arg1, arg2) = emit (ADDSD (arg1, arg2))
  let and_ (x, y) = emit (AND (x, y))
  let andpd (arg1, arg2) = emit (ANDPD (arg1, arg2))
  let bswap arg = emit (BSWAP arg)
  let cmp (x, y) = emit (CMP (x, y))
  let comisd (arg1, arg2) = emit (COMISD (arg1, arg2))
  let cqo () = emit CQO
  let cvtsd2ss (arg1, arg2) = emit (CVTSD2SS (arg1, arg2))
  let cvtsi2sd (arg1, arg2) = emit (CVTSI2SD (arg1, arg2))
  let cvtss2sd (arg1, arg2) = emit (CVTSS2SD (arg1, arg2))
  let cvttsd2si (arg1, arg2) = emit (CVTTSD2SI (arg1, arg2))
  let dec arg = emit (DEC arg)
  let divsd (arg1, arg2) = emit (DIVSD (arg1, arg2))
  let idiv arg = emit (IDIV arg)
  let imul (arg1, arg2) = emit (IMUL (arg1, arg2))
  let inc arg = emit (INC arg)
  let lea (arg1, arg2) = emit (LEA (arg1, arg2))
  let mov (x, y) = emit (MOV (x, y))
  let movapd (arg1, arg2) = emit (MOVAPD (arg1, arg2))
  let movsd (arg1, arg2) = emit (MOVSD (arg1, arg2))
  let movss (arg1, arg2) = emit (MOVSS (arg1, arg2))
  let movsx (x, y) = emit (MOVSX (x, y))
  let movsxd (arg1, arg2) = emit (MOVSXD  (arg1, arg2))
  let movzx (x, y) = emit (MOVZX (x, y))
  let mulsd (arg1, arg2) = emit (MULSD (arg1, arg2))
  let or_ (x, y) = emit (OR (x, y))
  let pop arg = emit (POP arg)
  let push arg = emit (PUSH arg)
  let sal (arg1, arg2) = emit (SAL (arg1, arg2))
  let sar (arg1, arg2) = emit (SAR (arg1, arg2))
  let shr (arg1, arg2) = emit (SHR (arg1, arg2))
  let sqrtsd (arg1, arg2) = emit (SQRTSD (arg1, arg2))
  let sub (x, y) = emit (SUB (x, y))
  let subsd  (arg1, arg2) = emit (SUBSD (arg1, arg2))
  let test (x, y) = emit (TEST (x, y))
  let ucomisd (arg1, arg2) = emit (UCOMISD (arg1, arg2))
  let xchg (arg1, arg2) = emit (XCHG (arg1, arg2))
  let xor (x, y) = emit (XOR (x, y))
  let xorpd (arg1, arg2) = emit (XORPD (arg1, arg2))
end

module DSL64 = struct
  include DSL

  let _label s = directive (NewLabel (s, QWORD))

  let mem_ptr typ ?(scale = 1) ?base offset idx =
    assert(scale > 0);
    Mem64 {typ; idx; scale; base; sym=None; displ=offset}

  let from_rip typ ?(ofs = 0) s =
    Mem64_RIP (typ, s, ofs)
end
