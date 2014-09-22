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

[@@@ocaml.warning "+A-42-4"]

(*


9.13.16 AT&T Syntax bugs

The UnixWare assembler, and probably other AT&T derived ix86 Unix
assemblers, generate floating point instructions with reversed source
and destination registers in certain cases. Unfortunately, gcc and
possibly many other programs use this reversed syntax, so we're stuck
with it.

For example

             fsub %st,%st(3)

results in `%st(3)' being updated to `%st - %st(3)' rather than the
expected `%st(3) - %st'. This happens with all the non-commutative
arithmetic floating point operations with two register operands where
the source register is `%st' and the destination register is `%st(i)'.

# gas
fdiv %st, %st(i)
semantics : %st(i) = %st / %st(i)
# masm
FDIVR ST(i), ST(0)

*)


open Intel_ast
open Intel_proc

let tab b = Buffer.add_char b '\t'
let bprintf = Printf.bprintf

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let bprint_arg_mem b string_of_register {typ=_; idx; scale; base; sym; displ} =
  begin match sym with
  | None -> bprintf b "%Ld" displ
  | Some s ->
      Buffer.add_string b s;
      if displ = 0L then ()
      else if displ > 0L then bprintf b "+%Ld" displ
      else bprintf b "%Ld" displ
  end;
  if scale <> 0 || base != None then begin
    Buffer.add_char b '(';
    begin match base with
    | None -> ()
    | Some base -> print_reg b string_of_register base
    end;
    if scale <> 0 then begin
      if base <> None || scale <> 1 then Buffer.add_char b ',';
      print_reg b string_of_register idx;
      if scale <> 1 then bprintf b ",%d" scale;
      Buffer.add_char b ')'
    end
  end

let bprint_arg b = function
  | Sym x -> Buffer.add_char b '$'; Buffer.add_string b x
  | Imm x -> bprintf b "$%Ld" x
  | Reg8  x -> print_reg b string_of_register8 x
  | Reg16 x -> print_reg b string_of_register16 x
  | Reg32 x -> print_reg b string_of_register32 x
  | Reg64 x -> print_reg b string_of_register64 x
  | Regf x  -> print_reg b string_of_registerf x
  | Mem32 addr -> bprint_arg_mem b string_of_register32 addr
  | Mem64 addr -> bprint_arg_mem b string_of_register64 addr

let rec cst = function
  | ConstLabel _ | Const _ | ConstThis as c -> cst c
  | ConstAdd (c1, c2) -> scst c1 ^ " + " ^ scst c2
  | ConstSub (c1, c2) -> scst c1 ^ " - " ^ scst c2

and scst = function
  | ConstThis -> "."
  | ConstLabel l -> l
  | Const n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L -> Int64.to_string n
  | Const n -> Printf.sprintf "0x%Lx" n
  | ConstAdd (c1, c2) -> Printf.sprintf "(%s + %s)" (scst c1) (scst c2)
  | ConstSub (c1, c2) -> Printf.sprintf "(%s - %s)" (scst c1) (scst c2)

let suffix = function
  | Mem32 {typ=BYTE; _}  | Mem64 {typ=BYTE; _}  | Reg8 _   -> "b"
  | Mem32 {typ=WORD; _}  | Mem64 {typ=WORD; _}  | Reg16 _  -> "w"
  | Mem32 {typ=DWORD; _} | Mem64 {typ=DWORD; _} | Reg32 _
  | Mem32 {typ=REAL8; _} | Mem64 {typ=REAL8; _}            -> "l"
  | Mem32 {typ=QWORD; _} | Mem64 {typ=QWORD; _} | Reg64 _  -> "q"
  | Mem32 {typ=REAL4; _} | Mem64 {typ=REAL4; _}            -> "s"
  | Mem32 {typ=NO; _} | Mem64 {typ=NO; _} -> assert false
  | _ -> ""

let i0 b s =
  tab b;
  Buffer.add_string b s

let i1 b s x =
  tab b;
  Buffer.add_string b s;
  tab b;
  bprint_arg b x

(* Automatically add suffix derived from argument *)
let i1_s b s x =
  tab b;
  Buffer.add_string b s;
  Buffer.add_string b (suffix x);
  tab b;
  bprint_arg b x

let i2 b s x y =
  tab b;
  Buffer.add_string b s;
  tab b;
  bprint_arg b x;
  Buffer.add_char b ',';
  Buffer.add_char b ' ';
  bprint_arg b y

(* Automatically add suffix derived from second argument *)
let i2_s b s x y =
  tab b;
  Buffer.add_string b s;
  Buffer.add_string b (suffix y);
  tab b;
  bprint_arg b x;
  Buffer.add_char b ',';
  Buffer.add_char b ' ';
  bprint_arg b y

(* Automatically add suffixes derived from first and second argument *)
let i2_ss b s x y =
  tab b;
  Buffer.add_string b s;
  Buffer.add_string b (suffix x);
  Buffer.add_string b (suffix y);
  tab b;
  bprint_arg b x;
  Buffer.add_char b ',';
  Buffer.add_char b ' ';
  bprint_arg b y

let i1_call_jmp b s x =
  match x with
  (* this is the encoding of jump labels: don't use * *)
  | Mem64 {idx=RIP; scale=1; base=None; sym=Some _; _}
  | Mem32 {idx=_;   scale=0; base=None; sym=Some _; _} (*used?*) ->
      i1 b s x
  | Reg32 _ | Reg64 _ | Mem32 _ | Mem64 _ ->
      tab b;
      Buffer.add_string b s;
      tab b;
      Buffer.add_char b '*';
      bprint_arg b x
  | Sym x ->
      tab b;
      Buffer.add_string b s;
      tab b;
      Buffer.add_string b x
  | _ ->
      assert false


let emit_instr b = function
  | ADD (arg1, arg2) -> i2_s b "add" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | AND (arg1, arg2) -> i2_s b "and" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg  -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cltd"
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2_s b "cmp" arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2
  | CQTO ->  i0 b "cqto"
  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b ("cvtsi2sd" ^ suffix arg1) arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
  | CVTTSD2SI (arg1, arg2) -> i2_s b "cvttsd2si" arg1 arg2
  | DEC arg -> i1_s b "dec" arg
  | DIVSD (arg1, arg2) -> i2 b "divsd" arg1 arg2
  | FABS -> i0 b "fabs"
  | FADD arg -> i1_s b "fadd" arg
  | FADDP (arg1, arg2)  -> i2 b "faddp" arg1 arg2
  | FCHS -> i0 b "fchs"
  | FCOMP arg -> i1_s b "fcomp" arg
  | FCOMPP -> i0 b "fcompp"
  | FCOS -> i0 b "fcos"
  | FDIV arg -> i1_s b "fdiv" arg
  | FDIVP (Regf (ST 0), arg2)  -> i2 b "fdivrp" (Regf (ST 0)) arg2 (* bug *)
  | FDIVP (arg1, arg2)  -> i2 b "fdivp" arg1 arg2
  | FDIVR arg -> i1_s b "fdivr" arg
  | FDIVRP (Regf (ST 0), arg2)  -> i2 b "fdivp" (Regf (ST 0)) arg2 (* bug *)
  | FDIVRP (arg1, arg2)  -> i2 b "fdivrp" arg1 arg2
  | FILD arg -> i1_s b "fild" arg
  | FISTP arg -> i1_s b "fistp" arg
  | FLD (Mem32 {typ=REAL4; _} as arg ) -> i1 b "flds" arg
  | FLD arg -> i1 b "fldl" arg
  | FLD1 -> i0 b "fld1"
  | FLDCW arg -> i1 b "fldcw" arg
  | FLDLG2 -> i0 b "fldlg2"
  | FLDLN2 -> i0 b "fldln2"
  | FLDZ -> i0 b "fldz"
  | FMUL arg -> i1_s b "fmul" arg
  | FMULP (arg1, arg2)  -> i2 b "fmulp" arg1 arg2
  | FNSTCW arg -> i1 b "fnstcw" arg
  | FNSTSW arg -> i1 b "fnstsw" arg
  | FPATAN -> i0 b "fpatan"
  | FPTAN -> i0 b "fptan"
  | FSIN -> i0 b "fsin"
  | FSQRT -> i0 b "fsqrt"
  | FSTP (Mem32 {typ=REAL4; _} as arg) -> i1 b "fstps" arg
  | FSTP arg -> i1 b "fstpl" arg
  | FSUB arg -> i1_s b "fsub" arg
  | FSUBP (Regf (ST 0), arg2)  -> i2 b "fsubrp" (Regf (ST 0)) arg2 (* bug *)
  | FSUBP (arg1, arg2)  -> i2 b "fsubp" arg1 arg2
  | FSUBR arg -> i1_s b "fsubr" arg
  | FSUBRP (Regf (ST 0), arg2) -> i2 b "fsubp" (Regf (ST 0)) arg2 (* bug *)
  | FSUBRP (arg1, arg2)  -> i2 b "fsubrp" arg1 arg2
  | FXCH arg -> i1 b "fxch" arg
  | FYL2X -> i0 b "fyl2x"
  | HLT -> i0 b "hlt"
  | IDIV arg -> i1_s b "idiv" arg
  | IMUL (arg, None) -> i1_s b "imul" arg
  | IMUL (arg1, Some arg2) -> i2_s b "imul" arg1 arg2
  | INC arg -> i1_s b "inc" arg
  | J (c, arg) -> i1_call_jmp b ("j" ^ string_of_condition c) arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | LEA (arg1, arg2) -> i2_s b "lea" arg1 arg2
  | LEAVE -> i0 b "leave"
  | MOV ((Imm n as arg1), (Reg64 _ as arg2))
    when not (n <= 0x7FFF_FFFFL && n >= -0x8000_0000L) ->
      i2 b "movabsq" arg1 arg2
  | MOV (arg1, arg2) -> i2_s b "mov" arg1 arg2
  | MOVAPD (arg1, arg2) -> i2 b "movapd" arg1 arg2
  | MOVLPD (arg1, arg2) -> i2 b "movlpd" arg1 arg2
  | MOVSD (arg1, arg2) -> i2 b "movsd" arg1 arg2
  | MOVSS (arg1, arg2) -> i2 b "movss" arg1 arg2
  | MOVSX (arg1, arg2) -> i2_ss b "movs" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movslq" arg1 arg2
  | MOVZX (arg1, arg2) -> i2_ss b "movz" arg1 arg2
  | MULSD (arg1, arg2) -> i2 b "mulsd" arg1 arg2
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | OR (arg1, arg2) -> i2_s b "or" arg1 arg2
  | POP  arg -> i1_s b "pop" arg
  | PUSH arg -> i1_s b "push" arg
  | RET ->  i0 b "ret"
  | ROUNDSD (r, arg1, arg2) -> i2 b (string_of_rounding r) arg1 arg2
  | SAL (arg1, arg2) -> i2_s b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2_s b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2_s b "shr" arg1 arg2
  | SQRTSD (arg1, arg2) -> i2 b "sqrtsd" arg1 arg2
  | SUB (arg1, arg2) -> i2_s b "sub" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | TEST (arg1, arg2) -> i2_s b "test" arg1 arg2
  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2_s b "xor" arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2

(* bug:
   https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
*)


let bprint_instr_name b = function
  | Ins instr -> emit_instr b instr

  | Align (_data,n) ->
      (* MacOSX assembler interprets the integer n as a 2^n alignment *)
      let n = if system = S_macosx then Misc.log2 n else n in
      bprintf b "\t.align\t%d" n
  | Byte n -> bprintf b "\t.byte\t%s" (cst n)
  | Bytes s ->
      if system = S_solaris then assert false (* TODO *)
      else bprintf b "\t.ascii\t\"%s\"" (string_of_string_literal s)
  | Comment s -> bprintf b "\t\t\t\t(* %s *)" s
  | End -> ()
  | Global s -> bprintf b "\t.globl\t%s" s;
  | Long n -> bprintf b "\t.long\t%s" (cst n)
  | NewLabel (s, _) -> bprintf b "%s:" s
  | Quad n -> bprintf b "\t.quad\t%s" (cst n)
  | Section ([".data" ], _, _) -> bprintf b "\t.data"
  | Section ([".text" ], _, _) -> bprintf b "\t.text"
  | Section (name, flags, args) ->
      bprintf b ".section %s" (String.concat "," name);
      begin match flags with
      | None -> ()
      | Some flags -> bprintf b ",%S" flags
      end;
      begin match args with
      | [] -> ()
      | _ -> bprintf b ",%s" (String.concat "," args)
      end
  | Space n ->
      if system = S_solaris then bprintf b "\t.zero\t%d" n
      else bprintf b "\t.space\t%d" n
  | Word n ->
      if system = S_solaris then bprintf b "\t.value\t%s" (cst n)
      else bprintf b "\t.word\t%s" (cst n)

  (* gas only *)
  | Cfi_adjust_cfa_offset n -> bprintf b "\t.cfi_adjust_cfa_offset %d" n
  | Cfi_endproc -> bprintf b "\t.cfi_endproc"
  | Cfi_startproc -> bprintf b "\t.cfi_startproc"
  | File (file_num, file_name) ->
      bprintf b "\t.file\t%d\t\"%s\""
        file_num (Intel_proc.string_of_string_literal file_name)
  | Indirect_symbol s -> bprintf b "\t.indirect_symbol %s" s
  | Loc (file_num, line) -> bprintf b "\t.loc\t%d\t%d" file_num line
  | Private_extern s -> bprintf b "\t.private_extern %s" s
  | Set (arg1, arg2) -> bprintf b "\t.set %s, %s" arg1 (cst arg2)
  | Size (s, c) -> bprintf b "\t.size %s,%s" s (cst c)
  | Type (s, typ) -> bprintf b "\t.type %s,%s" s typ

  (* masm only *)
  | External _
  | Mode386
  | Model _
    -> assert false


let bprint_instr b instr =
  bprint_instr_name b instr;
  Buffer.add_char b '\n'
