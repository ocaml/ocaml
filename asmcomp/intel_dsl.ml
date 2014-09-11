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

open Intel_ast
open Intel_proc

module ForceMem = struct

  (* These functions are used to force the data_type on memory locations
     for some instructions, in case they have been forgotten. For example,
     "movl" will force DWORD type on both its arguments. This is needed
     since MASM requires to have data_types on memory accesses.
  *)

  let force_mem data_type0 data_type = function
    | Mem (NO, mem) -> Mem (data_type, mem)
    | Mem (dtype, mem) when dtype = data_type0 -> Mem (data_type, mem)
    | Mem (dtype, _) as mem when dtype = data_type ->  mem
    | Mem (dtype, _)
      ->
        Printf.kprintf failwith
          "wrong explicit data type %S instead of %S"
          (string_of_datatype dtype) (string_of_datatype data_type)
    | arg -> match arg, data_type with
      (* sanity checks on other operands *)
      | (Reg16 _ | Reg32 _ | Reg64 _ | Regf _), BYTE -> assert false
      | (Reg8 _ | Reg32 _ | Reg64 _ | Regf _), WORD -> assert false
      | (Reg8 _ | Reg16 _ | Reg64 _ | Regf _), DWORD -> assert false
      | (Reg8 _ | Reg16 _ | Reg32 _ | Regf _), QWORD -> assert false
      | (Reg8 _ | Reg16 _ | Reg32 _ | Reg64 _), REAL8 -> assert false
      | _ -> arg

  (* Force data_type information on argument if non-existing
     [force_mem src dst] changes memory accesses with attributes in [src]
     to the [dst] attribute. *)
  let force_real8 = force_mem QWORD REAL8
  let force_real4 = force_mem DWORD REAL4
  let force_byte = force_mem NO BYTE
  let force_word = force_mem NO WORD
  let force_dword = force_mem NO DWORD
  let force_qword = force_mem NO QWORD
  let force_option force = function
      None -> None
    | Some arg -> Some (force arg)
end

(* A DSL to write GAS-like assembly code in OCAML. It is splitted into
   three modules : DSL (the common parts), DSL32 (specialized for x86)
   and DSL64 (specialized for amd64). *)
module DSL = struct

  let _eax = R32 RAX
  let _ebx = R32 RBX
  let _edi = R32 RDI
  let _esi = R32 RSI
  let _edx = R32 RDX
  let _ecx = R32 RCX
  let _esp = R32 RSP
  let _ebp = R32 RBP
  let _r8d = R32 R8
  let _r9d = R32 R9
  let _r10d = R32 R10
  let _r11d = R32 R11
  let _r12d = R32 R12
  let _r13d = R32 R13
  let _r14d = R32 R14
  let _r15d = R32 R15


  (* Override emitaux.ml *)
  let emit_int n =
    if n >= -0x80L && n <= 0x7FL then
      Imm (B8, (None, n))
    else
    if n >= -0x8000L && n <= 0x7FFFL then
      Imm (B16, (None, n))
    else
      (* We emit all immediates as B32, even if they are bigger.
         The only instruction (movabsq) taking an immediate B64 will cast
          B8|B16|B32 to B64. *)
      Imm (B32, (None, n))

  (* Override emitaux.ml *)
  let const_int n =
    if n >= -0x80L && n <= 0x7FL then
      Const (B8, n)
    else
    if n >= -0x8000L && n <= 0x7FFFL then
      Const (B16, n)
    else
    if n >= -0x8000_0000L && n <= 0x7FFF_FFFFL then
      Const (B32, n)
    else
      Const (B64, n)


  let emit_nat n = emit_int (Int64.of_nativeint n)
  let int n = emit_int (Int64.of_int n)
  let const_64 n = const_int n
  let const_32 n = const_int (Int64.of_int32 n)
  let const_nat n = const_int (Int64.of_nativeint n)
  let const n = const_int (Int64.of_int n)

  let emit_float64_directive f = ConstFloat f

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
  let _llabel s = directive (NewLabel (s, NO)) (* local label *)
  let _comment s = directive (Comment s)
  let _extrn s ptr = directive (External (s, ptr))
  let _private_extern s = directive (Private_extern s)
  let _indirect_symbol s = directive (Indirect_symbol s)
  let _size name cst = directive (Size (name, cst))
  let _type name typ = directive (Type (name, typ))

  let _qword cst = directive (Constant (cst, B64))
  let _long cst = directive (Constant (cst, B32))
  let _word cst = directive (Constant (cst, B16))
  let _byte n = directive (Constant (n, B8))
  let _ascii s = directive (Bytes s)
  let _space n = directive (Space n)
  let _setvar (arg1, arg2) = directive (Set (arg1, arg2))
  let _end () = directive End
  (* mnemonics *)

end

module INS = struct

  open ForceMem

  (* eta-expand to create ref everytime *)
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
  let movw (arg1, arg2) = emit (MOV (force_word arg1, force_word arg2))

  (* Byte mnemonics *)
  let decb arg = emit (DEC (force_byte arg))
  let cmpb (x, y) = emit (CMP (force_byte x, force_byte y))
  let movb (x, y) = emit (MOV (force_byte x, force_byte y))
  let andb (x, y)= emit (AND (force_byte x, force_byte y))
  let xorb (x, y)= emit (XOR (force_byte x, force_byte y))
  let testb (x, y)= emit (TEST (force_byte x, force_byte y))

  (* Long-word mnemonics *)
  let movl (x, y) = emit (MOV (force_dword x, force_dword y))
end

module INS32 = struct

  open ForceMem

  include INS

  (* Long-word mnemonics *)
  let addl (x, y) = emit (ADD (force_dword x, force_dword y))
  let subl (x, y) = emit (SUB (force_dword x, force_dword y))
  let andl (x, y) = emit (AND (force_dword x, force_dword y))
  let orl (x, y) = emit (OR (force_dword x, force_dword y))
  let xorl (x, y) = emit (XOR (force_dword x, force_dword y))
  let cmpl (x, y) = emit (CMP (force_dword x, force_dword y))
  let testl (x, y) = emit (TEST (force_dword x, force_dword y))

  let movzbl (x, y) = emit (MOVZX (force_byte x, force_dword y))
  let movsbl (x, y) = emit (MOVSX (force_byte x, force_dword y))
  let movzwl (x, y) = emit (MOVZX (force_word x, force_dword y))
  let movswl (x, y) = emit (MOVSX (force_word x, force_dword y))

  let sall (arg1, arg2) = emit (SAL  (arg1, force_dword arg2))
  let sarl (arg1, arg2) = emit (SAR  (arg1, force_dword arg2))
  let shrl (arg1, arg2) = emit (SHR  (arg1, force_dword arg2))
  let imull (arg1, arg2) =
    emit (IMUL (force_dword arg1, force_option force_dword arg2))

  let idivl arg = emit (IDIV (force_dword arg))
  let popl arg = emit (POP (force_dword arg))
  let pushl arg = emit (PUSH (force_dword arg))
  let decl arg = emit (DEC (force_dword arg))
  let incl arg = emit (INC (force_dword arg))
  let leal (arg1, arg2) = emit (LEA (arg1, force_dword arg2))

  let fistpl arg = emit (FISTP (force_dword arg))
  let fildl arg = emit (FILD (force_dword arg))

  let fchs () = emit FCHS
  let fabs () = emit FABS

  let fadds x = emit (FADD (force_real4 x))
  let faddl x = emit (FADD (force_real8 x))

  let fsubs x = emit (FSUB (force_real4 x))
  let fsubl x = emit (FSUB (force_real8 x))

  let fdivs x = emit (FDIV (force_real4 x))
  let fdivl x = emit (FDIV (force_real8 x))

  let fmuls x = emit (FMUL (force_real4 x))
  let fmull x = emit (FMUL (force_real8 x))

  let fsubrs x = emit (FSUB (force_real4 x))
  let fsubrl x = emit (FSUB (force_real8 x))

  let fdivrs x = emit (FDIV (force_real4 x))
  let fdivrl x = emit (FDIV (force_real8 x))

  let faddp (arg1, arg2) = emit  (FADDP (arg1, arg2))
  let fmulp (arg1, arg2) = emit  (FMULP (arg1, arg2))
  let fcompp () = emit FCOMPP
  let fcompl arg = emit (FCOMP (force_real8 arg))
  let fldl arg = emit (FLD (force_real8 arg))
  let flds arg = emit (FLD (force_real4 arg))
  let fnstsw arg = emit (FNSTSW arg)
  let fld1 () = emit FLD1
  let fpatan () = emit  FPATAN
  let fptan () = emit  FPTAN
  let fcos () = emit  FCOS
  let fldln2 () = emit  FLDLN2
  let fldlg2 () = emit  FLDLG2
  let fxch arg = emit  (FXCH arg)
  let fyl2x () = emit  FYL2X
  let fsin () = emit  FSIN
  let fsqrt () = emit  FSQRT
  let fstps arg = emit  (FSTP (force_real4 arg))
  let fstp arg = emit  (FSTP arg)
  let fstpl arg = emit  (FSTP (force_real8 arg))
  let fldz () = emit FLDZ
  let fnstcw arg = emit (FNSTCW arg)
  let fldcw arg = emit (FLDCW arg)
  let cltd () = emit CDQ


  (* Let's be compatible with prehistoric bugs (part1) :
     https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
  *)
  let fix_bug f1 f2 = function
    | (Regf (ST 0), (Regf (ST _) as arg2)) -> emit (f2 (Regf (ST 0), arg2))
    | (arg1, arg2) -> emit  (f1 (arg1, arg2))
  let fix_bug2 f1 f2 = fix_bug f1 f2, fix_bug f2 f1
  let fsubp, fsubrp = fix_bug2
      (fun (arg1,arg2) -> FSUBP (arg1,arg2))
      (fun (arg1,arg2) -> FSUBRP (arg1,arg2))
  let fdivp, fdivrp = fix_bug2
      (fun (arg1,arg2) -> FDIVP (arg1,arg2))
      (fun (arg1,arg2) -> FDIVRP (arg1,arg2))

end

module DSL32 = struct

  include DSL

  let _st n = Regf (ST n)

  let _label s = directive (NewLabel (s, DWORD))

  let _r r = Reg32 r

  let abs_ s = (s, None)

  let _offset l = Imm (B32, (Some l,0L))
  let _l l = Rel (B32, (Some (l, None), 0L))

  let _mem_reg pref d reg =
    Mem(pref, M32 (Some (reg, 1, None), (None, Int64.of_int d)))
  let _mem_sym_x pref l d =
    Mem(pref, M32(None, (Some (l, None), Int64.of_int d)))
  let _mem_sym l = _mem_sym_x NO l 0

end


module INS64 = struct
  include INS

  open ForceMem

  (* Qword mnemonics *)
  let addq (x, y) = emit (ADD (force_qword x, force_qword y))
  let subq (x, y) = emit (SUB (force_qword x, force_qword y))
  let andq (x, y) = emit (AND (force_qword x, force_qword y))
  let orq (x, y) = emit (OR (force_qword x, force_qword y))
  let xorq (x, y) = emit (XOR (force_qword x, force_qword y))
  let cmpq (x, y) = emit (CMP (force_qword x, force_qword y))
  let testq (x, y) = emit (TEST (force_qword x, force_qword y))

  let movq (x, y) = emit (MOV (force_qword x, force_qword y))

  let movzbq (x, y) = emit (MOVZX (force_byte x, force_qword y))
  let movsbq (x, y) = emit (MOVSX (force_byte x, force_qword y))
  let movzwq (x, y) = emit (MOVZX (force_word x, force_qword y))
  let movswq (x, y) = emit (MOVSX (force_word x, force_qword y))

  let idivq arg = emit (IDIV (force_qword arg))

  let salq (arg1, arg2) = emit (SAL (arg1, force_qword arg2))
  let sarq (arg1, arg2) = emit (SAR (arg1, force_qword arg2))
  let shrq (arg1, arg2) = emit (SHR (arg1, force_qword arg2))
  let imulq (arg1, arg2) =
    emit (IMUL (force_qword arg1, force_option force_qword arg2))

  let popq arg = emit (POP (force_qword arg))
  let pushq arg = emit (PUSH (force_qword arg))
  let leaq (arg1, arg2) = emit (LEA (arg1, force_qword arg2))



  let movsd (arg1, arg2) = emit (MOVSD (arg1, arg2))
  let ucomisd (arg1, arg2) = emit (UCOMISD (arg1, arg2))
  let comisd (arg1, arg2) = emit (COMISD (arg1, arg2))
  let movapd (arg1, arg2) = emit (MOVAPD  (arg1, arg2))
  let movabsq (arg1, arg2) =
    let arg1 = match arg1 with
      | Imm(_, n) -> Imm(B64,n)
      | _ -> assert false
    in
    emit (MOV  (arg1, force_qword arg2))
  let xorpd (arg1, arg2) = emit (XORPD  (arg1, arg2))
  let andpd (arg1, arg2) = emit (ANDPD  (arg1, arg2))

  let movslq (arg1, arg2) = emit (MOVSXD  (arg1, arg2))
  let movss (arg1, arg2) = emit (MOVSS (arg1, arg2))
  let cvtss2sd (arg1, arg2) = emit (CVTSS2SD (arg1, arg2))
  let cvtsd2ss (arg1, arg2) = emit (CVTSD2SS (arg1, arg2))
  let cvtsi2sd (arg1, arg2) = emit (CVTSI2SD (arg1, arg2))
  let cvttsd2si (arg1, arg2) = emit (CVTTSD2SI (arg1, arg2))
  let addsd (arg1, arg2) = emit (ADDSD (arg1, arg2))
  let subsd  (arg1, arg2) = emit (SUBSD (arg1, arg2))
  let mulsd (arg1, arg2) = emit (MULSD (arg1, arg2))
  let divsd (arg1, arg2) = emit (DIVSD (arg1, arg2))
  let sqrtsd (arg1, arg2) = emit (SQRTSD (arg1, arg2))

  let cqto () = emit CQTO

  let incq arg = emit (INC (force_qword arg))
  let decq arg = emit (DEC (force_qword arg))
  let xchg (arg1, arg2) = emit (XCHG (arg1, arg2))
  let bswap arg = emit (BSWAP arg)


end

module DSL64 = struct
  include DSL

  let _label s = directive (NewLabel (s, QWORD))

  let al  = Reg8 AL
  let ah  = Reg8 AH
  let cl  = Reg8 CL
  let rax = Reg64 RAX
  let r10 = Reg64 R10
  let r11 = Reg64 R11
  let r14 = Reg64 R14
  let r15 = Reg64 R15
  let rsp = Reg64 RSP
  let rbp = Reg64 RBP
  let xmm15 = Regf (XMM 15)

  let _offset l = Imm (B64, (Some l,0L))
  let _l l = Rel (B32, (Some (l, None), 0L))
  let rel_ s = Rel (B32, (Some s,0L))

  let mem_ptr ?(pref = NO) ?(scale = 1) ?base offset reg =
    Mem (pref, M64(Some (reg, scale, base), (None, Int64.of_int offset)))
end
