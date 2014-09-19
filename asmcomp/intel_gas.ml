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

let string_of_table = function
  | Some PLT -> "@PLT"
  | Some GOTPCREL -> "@GOTPCREL"
  | None -> ""

let string_of_symbol = function
  | s, Some PLT -> s ^ "@PLT"
  | s, Some GOTPCREL -> s ^ "@GOTPCREL"
  | s, None -> s

let print_ofs b strict = function
  | 0L -> ()
  | x when strict && x > 0L -> Printf.bprintf b "+%Ld" x
  | x -> Printf.bprintf b "%Ld" x

let print_sym_tbl b (s, table) =
  Buffer.add_string b s;
  Buffer.add_string b (string_of_table table)

let print_opt_sym_tbl b = function
  | None -> ()
  | Some sym -> print_sym_tbl b sym

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let print_opt_reg b f = function
  | None -> ()
  | Some reg -> print_reg b f reg

let bprint_arg_mem b string_of_register (a, (sym, offset) : 'a addr) =
  print_opt_sym_tbl b sym;
  print_ofs b (sym <> None) offset;
  match a with
  | Some (reg1, scale, base) ->
      Buffer.add_char b '(';
      print_opt_reg b string_of_register base;
      if base <> None || scale <> 1 then Buffer.add_char b ',';
      print_reg b string_of_register reg1;
      if scale <> 1 then Printf.bprintf b ",%d" scale;
      Buffer.add_char b ')'
  | None ->
      assert (sym <> None || offset <> 0L)

let bprint_arg b arg =
  match arg with
  | Rel (_, sym) ->
      Printf.bprintf b "%s" (string_of_symbol sym)

  | Imm (_, (None, int) ) ->
      Printf.bprintf b "$%Ld" int
  | Imm (_, (Some sym,0L)) ->
      Printf.bprintf b "$%s" (string_of_symbol sym)
  | Imm (_, (Some sym,d)) ->
      if d > 0L then
        Printf.bprintf b "$%s+%Ld" (string_of_symbol sym) d
      else
        Printf.bprintf b "$%s%Ld" (string_of_symbol sym) d

  | Reg8 register8 ->
      Printf.bprintf b "%%%s" (string_of_register8 register8)
  | Reg16 register16 ->
      Printf.bprintf b "%%%s" (string_of_register16 register16)
  | Reg32 register32 ->
      Printf.bprintf b "%%%s" (string_of_register32 register32)
  | Reg64 register ->
      Printf.bprintf b "%%%s" (string_of_register64 register)
  | Regf registerf ->
      Printf.bprintf b "%%%s" (string_of_registerf registerf)

  | Mem (_ptr, M32 addr) ->
      bprint_arg_mem b string_of_register32 addr
  | Mem (_ptr, M64 addr) ->
      bprint_arg_mem b string_of_register64 addr

let rec string_of_constant = function
  | ConstLabel _
  | Const _
    as c -> string_of_simple_constant c
  | ConstAdd (c1, c2) ->
      (string_of_simple_constant c1) ^ " + " ^ (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      (string_of_simple_constant c1) ^ " - " ^ (string_of_simple_constant c2)

and string_of_simple_constant = function
  | ConstLabel l -> l
  | Const (B64, n) -> Printf.sprintf "0x%Lx" n
  | Const (_, n) -> Int64.to_string n
  | ConstAdd (c1, c2) ->
      Printf.sprintf "(%s + %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      Printf.sprintf "(%s - %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)

let suffix = function
  | Mem (BYTE, _) | Reg8 _   -> "b"
  | Mem (WORD, _) | Reg16 _  -> "w"
  | Mem (DWORD, _) | Reg32 _ | Mem (REAL8, _) -> "l"
  | Mem (QWORD, _) | Reg64 _ -> "q"
  | Mem (REAL4, _) -> "s"
  | Mem (NO, _) -> assert false
  | _ -> ""
(*
  | Imm (_ -> Printf.eprintf "%s\n%!" (Printexc.raw_backtrace_to_string (Printexc.get_callstack 10)); exit 2
*)

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
  | Mem (_, M64 (Some (RIP, _, _), (Some _,_)))
  | Mem (_, M32 (None, (Some _, _))) ->
      i1 b s x
  | Reg32 _ | Reg64 _ | Mem _ ->
      tab b;
      Buffer.add_string b s;
      tab b;
      Buffer.add_char b '*';
      bprint_arg b x
  | _ ->
      i1 b s x

let emit_instr b = function
  | NOP -> i0 b "nop"
  | NEG arg -> i1 b "neg" arg
  | ADD (arg1, arg2) -> i2_s b "add" arg1 arg2
  | SUB (arg1, arg2) -> i2_s b "sub" arg1 arg2
  | XOR (arg1, arg2) -> i2_s b "xor" arg1 arg2
  | OR (arg1, arg2) -> i2_s b "or" arg1 arg2
  | AND (arg1, arg2) -> i2_s b "and" arg1 arg2
  | CMP (arg1, arg2) -> i2_s b "cmp" arg1 arg2

  | LEAVE -> i0 b "leave"
  | SAR (arg1, arg2) -> i2_s b "sar" arg1 arg2
  | SHR (arg1, arg2) -> i2_s b "shr" arg1 arg2
  | SAL (arg1, arg2) -> i2_s b "sal" arg1 arg2

  | FISTP arg -> i1_s b "fistp" arg


  | FSTP (Mem(REAL4, _)  as arg) -> i1 b "fstps" arg
  | FSTP arg -> i1 b "fstpl" arg
  | FILD arg -> i1_s b "fild" arg
  | HLT -> i0 b "hlt"

  | FCOMPP -> i0 b "fcompp"
  | FCOMP arg -> i1_s b "fcomp" arg
  | FLD (Mem(REAL4, _ ) as arg ) -> i1 b "flds" arg
  | FLD arg -> i1 b "fldl" arg
  | FNSTSW arg -> i1 b "fnstsw" arg
  | FNSTCW arg -> i1 b "fnstcw" arg
  | FLDCW arg -> i1 b "fldcw" arg

  | FCHS -> i0 b "fchs"
  | FABS -> i0 b "fabs"

  | FADD arg -> i1_s b "fadd" arg
  | FMUL arg -> i1_s b "fmul" arg
  | FSUB arg -> i1_s b "fsub" arg
  | FSUBR arg -> i1_s b "fsubr" arg
  | FDIV arg -> i1_s b "fdiv" arg
  | FDIVR arg -> i1_s b "fdivr" arg

  (* Let's be compatible with prehistoric bugs:
     https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
  *)
  | FSUBP (Regf (ST 0), arg2)  -> i2 b "fsubrp" (Regf (ST 0)) arg2
  | FSUBRP (Regf (ST 0), arg2) -> i2 b "fsubp" (Regf (ST 0)) arg2
  | FDIVP (Regf (ST 0), arg2)  -> i2 b "fdivrp" (Regf (ST 0)) arg2
  | FDIVRP (Regf (ST 0), arg2)  -> i2 b "fdivp" (Regf (ST 0)) arg2

  | FSUBP (arg1, arg2)  -> i2 b "fsubp" arg1 arg2
  | FSUBRP (arg1, arg2)  -> i2 b "fsubrp" arg1 arg2
  | FDIVP (arg1, arg2)  -> i2 b "fdivp" arg1 arg2
  | FDIVRP (arg1, arg2)  -> i2 b "fdivrp" arg1 arg2

  | FLD1 -> i0 b "fld1"
  | FPATAN -> i0 b "fpatan"
  | FPTAN -> i0 b "fptan"
  | FCOS -> i0 b "fcos"
  | FLDLN2 -> i0 b "fldln2"
  | FLDLG2 -> i0 b "fldlg2"
  | FXCH arg -> i1 b "fxch" arg
  | FYL2X -> i0 b "fyl2x"
  | FSIN -> i0 b "fsin"
  | FSQRT -> i0 b "fsqrt"
  | FLDZ -> i0 b "fldz"

  | FADDP (arg1, arg2)  -> i2 b "faddp" arg1 arg2
  | FMULP (arg1, arg2)  -> i2 b "fmulp" arg1 arg2

  | INC arg -> i1_s b "inc" arg
  | DEC arg -> i1_s b "dec" arg

  | IMUL (arg, None) -> i1_s b "imul" arg
  | IMUL (arg1, Some arg2) -> i2_s b "imul" arg1 arg2
  | IDIV arg -> i1_s b "idiv" arg

  | MOV (
      (Imm (B64, _) as arg1),
      (Reg64 _ as arg2))
    -> i2 b "movabsq" arg1 arg2
  | MOV (arg1, arg2) -> i2_s b "mov" arg1 arg2
  | MOVZX (arg1, arg2) -> i2_ss b "movz" arg1 arg2
  | MOVSX (arg1, arg2) -> i2_ss b "movs" arg1 arg2
  | MOVSS (arg1, arg2) -> i2 b "movss" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movslq" arg1 arg2

  | MOVSD (arg1, arg2) -> i2 b "movsd" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | MULSD (arg1, arg2) -> i2 b "mulsd" arg1 arg2
  | DIVSD (arg1, arg2) -> i2 b "divsd" arg1 arg2
  | SQRTSD (arg1, arg2) -> i2 b "sqrtsd" arg1 arg2
  | ROUNDSD (rounding, arg1, arg2) ->
      let s =
        match rounding with
        | RoundDown -> "roundsd.down"
        | RoundUp -> "roundsd.up"
        | RoundTruncate -> "roundsd.trunc"
        | RoundNearest -> "roundsd.near"
      in
      i2 b s arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2

  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2

  | CVTSI2SD (arg1, arg2) -> i2 b ("cvtsi2sd" ^ suffix arg1) arg1 arg2
  | CVTTSD2SI (arg1, arg2) -> i2_s b "cvttsd2si" arg1 arg2

  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2

  | CALL arg  -> i1_call_jmp b "call" arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | RET ->  i0 b "ret"
  | PUSH arg -> i1_s b "push" arg
  | POP  arg -> i1_s b "pop" arg

  | TEST (arg1, arg2) -> i2_s b "test" arg1 arg2
  | SET (condition, arg) ->
      i1 b ("set" ^ string_of_condition condition) arg
  | J (condition, arg) ->
      i1 b ("j" ^ string_of_condition condition) arg


  | CMOV (condition, arg1, arg2) ->
      i2 b ("cmov" ^ string_of_condition condition) arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | MOVLPD (arg1, arg2) -> i2 b "movlpd" arg1 arg2
  | MOVAPD (arg1, arg2) -> i2 b "movapd" arg1 arg2
  | LEA (arg1, arg2) -> i2_s b "lea" arg1 arg2
  | CQTO ->  i0 b "cqto"
  | CDQ -> i0 b "cltd"

  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg

let bprint_instr_name b instr =
  match instr with
  | Global s ->
      Printf.bprintf b "\t.globl\t%s" s;
  | Align (_data,n) ->
      (* MacOSX assembler interprets the integer n as a 2^n alignment *)
      let n = if system = S_macosx then Misc.log2 n else n in
      Printf.bprintf b "\t.align\t%d" n
  | NewLabel (s, _) ->
      Printf.bprintf b "%s:" s
  | Comment s ->
      Printf.bprintf b "\t\t\t\t(* %s *)" s
  | Section ([".data" ], _, _) ->
      Printf.bprintf b "\t.data"
  | Section ([".text" ], _, _) ->
      Printf.bprintf b "\t.text"
  | Section (name, flags, args) ->
      Printf.bprintf b ".section %s" (String.concat "," name);
      begin match flags with
        None -> ()
      | Some flags -> Printf.bprintf b ",%S" flags
      end;
      begin match args with
        [] -> ()
      | _ ->
          Printf.bprintf b ",%s" (String.concat "," args)
      end;
  | End -> ()
  (* TODO: should we do something for this ? *)
  | External _ -> assert false
  | Mode386 -> assert false
  | Model _ -> assert false
  | Cfi_startproc -> Printf.bprintf b "\t.cfi_startproc"
  | Cfi_endproc -> Printf.bprintf b "\t.cfi_endproc"
  | Cfi_adjust_cfa_offset n ->
      Printf.bprintf b "\t.cfi_adjust_cfa_offset %d" n
  | File (file_num, file_name) ->
      Printf.bprintf b "\t.file\t%d\t\"%s\""
        file_num (Intel_proc.string_of_string_literal file_name)
  | Loc (file_num, line) ->
      Printf.bprintf b "\t.loc\t%d\t%d" file_num line
  | Set (arg1, arg2) ->
      Printf.bprintf b "\t.set %s, %s" arg1
        (string_of_constant arg2)
  | Space n ->
      if system = S_solaris then
        Printf.bprintf b "\t.zero\t%d" n
      else
        Printf.bprintf b "\t.space\t%d" n
  | Private_extern s ->
      Printf.bprintf b "\t.private_extern %s" s
  | Indirect_symbol s ->
      Printf.bprintf b "\t.indirect_symbol %s" s
  | Type (s, typ) ->
      Printf.bprintf b "\t.type %s,%s" s typ
  | Size (s, cst) ->
      Printf.bprintf b "\t.size %s,%s" s (string_of_constant cst)
  | Constant (n, B8) ->
      Printf.bprintf b "\t.byte\t%s" (string_of_constant n)
  | Constant (n, B16) ->
      if system = S_solaris then
        Printf.bprintf b "\t.value\t%s" (string_of_constant n)
      else
        Printf.bprintf b "\t.word\t%s" (string_of_constant n)
  | Constant (n, B32) ->
      Printf.bprintf b "\t.long\t%s" (string_of_constant n)
  | Constant (Const(_, n), B64) ->
      Printf.bprintf b "\t.quad\t%s" (string_of_constant (Const (B64,n)))
  | Constant (n, B64) ->
      Printf.bprintf b "\t.quad\t%s" (string_of_constant n)
  | Bytes s ->
      if system = S_solaris then
        assert false (* TODO *)
      else
        Printf.bprintf b "\t.ascii\t\"%s\"" (string_of_string_literal s)

  | Ins instr ->
      emit_instr b instr

let bprint_instr b instr =
  bprint_instr_name b instr;
  Buffer.add_string b "\n"
