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

  let force_mem data_types data_type ins = function
    | Mem (dtype, mem) when List.mem dtype data_types ->
        Mem (data_type, mem)
    | Mem (dtype, _) as mem when dtype = data_type ->  mem
    | Mem (dtype, _)
      ->
        Printf.kprintf failwith
          "Instruction %S: wrong explicit data type %S instead of %S"
          ins (string_of_datatype dtype) (string_of_datatype data_type)
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
  let force_real8 = force_mem [NO; QWORD] REAL8
  let force_real4 = force_mem [NO; DWORD] REAL4
  let force_byte = force_mem [NO] BYTE
  let force_word = force_mem [NO] WORD
  let force_dword = force_mem [NO] DWORD
  let force_qword = force_mem [NO] QWORD
  let force_option force ins = function
      None -> None
    | Some arg -> Some (force ins arg)

  let force2 name f force (arg1, arg2) =
    emit (f (force name arg1, force name arg2))

  let force12 name f force1 force2 (arg1, arg2) =
    emit (f (force1 name arg1, force2 name arg2))

  let force_fxxx name f =
    (function None -> assert false
            | Some arg -> emit (f (force_real4 name arg, None))),
    (function None -> assert false
            | Some arg -> emit (f (force_real8 name arg, None)))

  let force_cmp = force2 "cmp" (fun (arg1,arg2) -> CMP (arg1,arg2))
  let force_add = force2 "add" (fun (arg1,arg2) -> ADD (arg1,arg2))
  let force_sub = force2 "sub" (fun (arg1,arg2) -> SUB (arg1,arg2))
  let force_xor = force2 "xor" (fun (arg1,arg2) -> XOR (arg1,arg2))
  let force_or = force2 "or" (fun (arg1,arg2) -> OR (arg1,arg2))
  let force_and = force2 "and" (fun (arg1,arg2) -> AND (arg1,arg2))
  let force_test = force2 "test" (fun (arg1,arg2) -> TEST (arg1,arg2))

  let force_mov = force2 "mov" (fun (arg1,arg2) -> MOV (arg1,arg2))
  let force_movsx = force12 "movsx" (fun (arg1,arg2) -> MOVSX (arg1,arg2))
  let force_movzx = force12 "movzx" (fun (arg1,arg2) -> MOVZX (arg1,arg2))

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
  let _int n = emit_int (Int64.of_int n)
  let const_64 n = const_int n
  let const_32 n = const_int (Int64.of_int32 n)
  let const_nat n = const_int (Int64.of_nativeint n)
  let _const n = const_int (Int64.of_int n)

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
  let movw (arg1, arg2) = emit (MOV (
      force_word "movw" arg1,
      force_word "movw" arg2))

  (* Byte mnemonics *)
  let decb arg = emit (DEC (force_byte "decb" arg))
  let cmpb = force_cmp force_byte
  let movb = force_mov force_byte
  let andb = force_and force_byte
  let xorb = force_xor force_byte
  let testb = force_test force_byte

  (* Long-word mnemonics *)
  let movl = force_mov force_dword

end

module INS32 = struct

  open ForceMem

  include INS

  (* Long-word mnemonics *)
  let addl = force_add force_dword
  let subl = force_sub force_dword
  let andl = force_and force_dword
  let orl = force_or force_dword
  let xorl = force_xor force_dword
  let cmpl = force_cmp force_dword
  let testl = force_test force_dword

  let movzbl = force_movzx force_byte force_dword
  let movsbl = force_movsx force_byte force_dword
  let movzwl = force_movzx force_word force_dword
  let movswl = force_movsx force_word force_dword

  let sall (arg1, arg2) = emit (SAL  (arg1, force_dword "sall" arg2))
  let sarl (arg1, arg2) = emit (SAR  (arg1, force_dword "sarl" arg2))
  let shrl (arg1, arg2) = emit (SHR  (arg1, force_dword "shrl" arg2))
  let imull (arg1, arg2) = emit (IMUL (
      force_dword "imull" arg1,
      force_option force_dword "imull" arg2))

  let idivl arg = emit (IDIV (force_dword "idivl" arg))
  let popl arg = emit (POP (force_dword "popl" arg))
  let pushl arg = emit (PUSH (force_dword "pushl" arg))
  let decl arg = emit (DEC (force_dword "decl" arg))
  let incl arg = emit (INC (force_dword "incl" arg))
  let leal (arg1, arg2) = emit (LEA (arg1, force_dword "leal" arg2))

  let fistpl arg = emit (FISTP (force_dword "fistpl" arg))
  let fildl arg = emit (FILD (force_dword "fildl" arg))

  let fchs = function None -> emit FCHS | Some _ -> assert false
  let fabs = function None -> emit FABS | Some _ -> assert false

  let fadds, faddl = force_fxxx "fadd" (fun (arg1, arg2) -> FADD (arg1, arg2))
  let fsubs, fsubl = force_fxxx "fsub" (fun (arg1, arg2) -> FSUB (arg1, arg2))
  let fdivs, fdivl = force_fxxx "fdiv" (fun (arg1, arg2) -> FDIV (arg1, arg2))
  let fmuls, fmull = force_fxxx "fmul" (fun (arg1, arg2) -> FMUL (arg1, arg2))
  let fsubrs, fsubrl = force_fxxx "fsubr"
      (fun (arg1, arg2) -> FSUBR (arg1, arg2))
  let fdivrs, fdivrl = force_fxxx "fdivr"
      (fun (arg1, arg2) -> FDIVR (arg1, arg2))

  let faddp (arg1, arg2) = emit  (FADDP (arg1, arg2))
  let fmulp (arg1, arg2) = emit  (FMULP (arg1, arg2))
  let fcompp () = emit FCOMPP
  let fcompl arg = emit (FCOMP (force_real8 "fcompl" arg))
  let fldl arg = emit (FLD (force_real8 "fldl" arg))
  let flds arg = emit (FLD (force_real4 "flds" arg))
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
  let fstps arg = emit  (FSTP (force_real4 "fstps" arg))
  let fstp arg = emit  (FSTP arg)
  let fstpl arg = emit  (FSTP (force_real8 "fstpl" arg))
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
  let addq = force_add force_qword
  let subq = force_sub force_qword
  let andq = force_and force_qword
  let orq = force_or force_qword
  let xorq = force_xor force_qword
  let cmpq = force_cmp force_qword
  let testq = force_test force_qword

  let movq = force_mov force_qword
  let movzbq = force_movzx force_byte force_qword
  let movsbq = force_movsx force_byte force_qword
  let movzwq = force_movzx force_word force_qword
  let movswq = force_movsx force_word force_qword

  let idivq arg = emit (IDIV (force_qword "divq" arg))

  let salq (arg1, arg2) =
    emit (SAL (arg1, force_qword "salq" arg2))
  let sarq (arg1, arg2) =
    emit (SAR (arg1, force_qword "sarq" arg2))
  let shrq (arg1, arg2) =
    emit (SHR (arg1, force_qword "shrq" arg2))
  let imulq (arg1, arg2) = emit (IMUL (
      force_qword "imulq" arg1,
      force_option force_qword "imulq" arg2))

  let popq arg = emit (POP (force_qword "popq" arg))
  let pushq arg = emit (PUSH (force_qword "pushq" arg))
  let leaq (arg1, arg2) = emit (LEA (arg1, force_qword "leaq" arg2))



  let movsd (arg1, arg2) = emit (MOVSD (arg1, arg2))
  let ucomisd (arg1, arg2) = emit (UCOMISD (arg1, arg2))
  let comisd (arg1, arg2) = emit (COMISD (arg1, arg2))
  let movapd (arg1, arg2) = emit (MOVAPD  (arg1, arg2))
  let movabsq (arg1, arg2) =
    let arg1 = match arg1 with
      | Imm(_, n) -> Imm(B64,n)
      | _ -> assert false
    in
    emit (MOV  (arg1, force_qword "movabsq" arg2))
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

  let incq arg = emit (INC (force_qword "incq" arg))
  let decq arg = emit (DEC (force_qword "decq" arg))
  let xchg (arg1, arg2) = emit (XCHG (arg1, arg2))
  let bswap arg = emit (BSWAP arg)


end

module DSL64 = struct
  include DSL

  let _label s = directive (NewLabel (s, QWORD))

  let _r r = Reg64 r

  let rax = _r RAX
  let r10 = _r R10
  let r11 = _r R11
  let r14 = _r R14
  let r15 = _r R15
  let rsp = _r RSP
  let rbp = _r RBP
  let xmm15 = Regf (XMM 15)

(*
  let abs_ s = (s, None)
  let plt_ s = (s, Some PLT)
  let gotpcrel_ s = (s, Some GOTPCREL)
*)

  let _offset l = Imm (B64, (Some l,0L))
  let _l l = Rel (B32, (Some (l, None), 0L))
  let rel_ s = Rel (B32, (Some s,0L))

(*
  let at_rip pref s d =
    Mem (pref, M64 (Some (RIP, 1, None), (Some s, Int64.of_int d)))
*)
  let _mem_reg offset reg =
    Mem (NO, M64(Some (reg, 1, None), (None, Int64.of_int offset)))
  let _mem_ptr pref offset reg = Mem (pref, M64(Some (reg, 1, None),
                                                (None, Int64.of_int offset)))
  let _mem_base pref offset reg scale base =
    Mem (pref, M64(Some (reg, scale, Some base), (None, Int64.of_int offset)))


end
