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
let bprint b s = tab b; Buffer.add_string b s

let string_of_table = function
  | Some PLT -> "@PLT"
  | Some GOTPCREL -> "@GOTPCREL"
  | None -> ""

let string_of_symbol = function
  | s, Some PLT -> s ^ "@PLT"
  | s, Some GOTPCREL -> s ^ "@GOTPCREL"
  | s, None -> s

let bprint_arg_mem b string_of_register ( mem : 'a addr ) =
  match mem with
  | None, (None, addr) ->
      Printf.bprintf b "%Ld" addr
  | None, (Some (s, table), 0L)  ->
      Printf.bprintf b "%s%s" s (string_of_table table)
  | None, (Some (s, table), d) ->
      Printf.bprintf b "%s%s+%Ld" s (string_of_table table) d
  | Some (reg1, 1, None), (Some (s, table), 0L) ->
      Printf.bprintf b "%s%s(%%%s)" s (string_of_table table)
        (string_of_register reg1)
  | Some (reg1, 1, None), (Some (s, table), offset) ->
      if offset < 0L then
        Printf.bprintf b "%s%s%Ld(%%%s)" s (string_of_table table)
          offset (string_of_register reg1)
      else
        Printf.bprintf b "%s%s+%Ld(%%%s)" s (string_of_table table)
          offset (string_of_register reg1)

  | Some(reg1, scale, None), (Some (s, table), offset) ->
      if offset = 0L then
        Printf.bprintf b "%s%s(,%%%s,%d)" s (string_of_table table)
          (string_of_register reg1) scale
      else
      if offset < 0L then
        Printf.bprintf b "%s%s%Ld(,%%%s,%d)"
          s (string_of_table table)
          offset (string_of_register reg1) scale
      else
        Printf.bprintf b "%s%s+%Ld(,%%%s,%d)" s (string_of_table table)
          offset (string_of_register reg1) scale

  | Some (reg1, 1, None), (None, 0L) ->
      Buffer.add_char b '(';
      Printf.bprintf b "%%%s" (string_of_register reg1);
      Buffer.add_char b ')'

  | Some (reg1, 1, None), (None, offset) ->
      if offset <> 0L then begin
        if offset < 0L then
          Printf.bprintf b "-%Ld" (Int64.sub 0L offset)
        else
          Printf.bprintf b "%Ld" offset
      end;
      Buffer.add_char b '(';
      Printf.bprintf b "%%%s" (string_of_register reg1);
      Buffer.add_char b ')'

  | Some (reg1, scale, base), (None, offset) ->
      if offset <> 0L then begin
        if offset < 0L then
          Printf.bprintf b "-%Ld" (Int64.sub 0L offset)
        else
          Printf.bprintf b "%Ld" offset
      end;
      Buffer.add_char b '(';
      begin
        match base with
          None -> ()
        | Some reg2 ->
            Printf.bprintf b "%%%s" (string_of_register reg2)
      end;
      Buffer.add_char b ',';
      Printf.bprintf b "%%%s" (string_of_register reg1);
      if scale <> 1 then
        Printf.bprintf b ",%d" scale;
      Buffer.add_char b ')'

  | Some (reg1, scale, base), (Some (s, table), offset) ->
      Printf.bprintf b "%s%s" s (string_of_table table);
      if offset <> 0L then begin
        if offset < 0L then
          Printf.bprintf b "-%Ld" (Int64.sub 0L offset)
        else
          Printf.bprintf b "+%Ld" offset
      end;
      Buffer.add_char b '(';
      begin
        match base with
        | None -> ()
        | Some reg2 ->
            Printf.bprintf b "%%%s" (string_of_register reg2)
      end;
      Buffer.add_char b ',';
      Printf.bprintf b "%%%s" (string_of_register reg1);
      if scale <> 1 then
        Printf.bprintf b ",%d" scale;
      Buffer.add_char b ')'



let bprint_arg b arg =
  match arg with
  (*  | ConstantInt int -> Printf.bprintf b "$%d" int *)
  | Imm (_, (None, int) ) ->
      Printf.bprintf b "$%Ld" int

  | Rel (_, (Some sym,0L)) ->
      Printf.bprintf b "%s" (string_of_symbol sym)
  | Rel (_, _) -> assert false

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

let bprint_args b instr args =
  match args, instr with
  | [], _ -> ()
  | [ (* this is the encoding of jump labels: don't use * *)
    Mem (_, M64 (Some (RIP, _, _), (Some _,_)))
  | Mem (_, M32 (None, (Some _, _)))
    as arg ],  (CALL _ | JMP _)
    -> tab b; bprint_arg b arg
  | [ Reg32 _
    | Reg64 _
    | Mem _
      as arg ],  (CALL _ | JMP _) ->
      tab b; Buffer.add_char b '*'; bprint_arg b arg
  | [ arg ], _ -> tab b; bprint_arg b arg
  | [ arg1; arg2 ], _ ->
      tab b; bprint_arg b arg1;
      Buffer.add_char b ',';
      Buffer.add_char b ' ';
      bprint_arg b arg2
  | _ -> assert false

let rec string_of_constant = function
  | ConstLabel _
  | Const _
  | ConstFloat _
    as c -> string_of_simple_constant c
  | ConstAdd (c1, c2) ->
      (string_of_simple_constant c1) ^ " + " ^ (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      (string_of_simple_constant c1) ^ " - " ^ (string_of_simple_constant c2)

and string_of_simple_constant = function
  | ConstLabel (l, reloc_table) -> l ^ (string_of_table reloc_table)
  | Const (B64, n) -> Printf.sprintf "0x%Lx" n
  | Const (_, n) -> Int64.to_string n
  | ConstFloat f ->
      let x = Int64.bits_of_float (float_of_string f) in
      Printf.sprintf "0x%Lx" x
  | ConstAdd (c1, c2) ->
      Printf.sprintf "(%s + %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      Printf.sprintf "(%s - %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)

let auto_suffix ins arg =
  let suffix =
    match arg with
    | Mem (BYTE, _)
    | Reg8 _ -> "b"
    | Mem (WORD, _)
    | Reg16 _ -> "w"
    | Mem(DWORD, _)
    | Reg32 _ -> "l"
    | Mem(QWORD, _)
    | Reg64 _
      -> "q"
    | Mem(NO, M32 _) -> "l"
    | Mem(NO, M64 _) -> "q"
    | _ -> ""
  in
  ins ^ suffix


let list_o arg = match arg with None -> [] | Some arg -> [arg]

let split_instr = function
  | NOP -> "nop", []
  | NEG arg ->  "neg", [ arg ]
  | ADD (arg1, arg2) ->  auto_suffix "add" arg2, [arg1; arg2]
  | SUB (arg1, arg2) -> auto_suffix "sub" arg2, [arg1; arg2]
  | XOR (arg1, arg2) ->  auto_suffix "xor" arg2, [arg1; arg2]
  | OR (arg1, arg2) -> auto_suffix "or" arg2, [arg1; arg2]
  | AND (arg1, arg2) -> auto_suffix "and" arg2, [arg1; arg2]
  | CMP (arg1, arg2) -> auto_suffix "cmp" arg2, [arg1; arg2]

  | LEAVE -> "leave", []
  | SAR (arg1, arg2) -> auto_suffix "sar" arg2, [arg1; arg2]
  | SHR (arg1, arg2) -> auto_suffix "shr" arg2, [arg1; arg2]
  | SAL (arg1, arg2) -> auto_suffix "sal" arg2, [arg1; arg2]

  (*        | MOVABSQ (arg1, arg2) -> "movabsq", [arg1; arg2] *)
  | FISTP arg -> auto_suffix "fistp" arg, [ arg ]

  | FSTP ( Mem( REAL4, _)  as arg) -> "fstps", [arg]
  | FSTP arg -> "fstpl", [arg]
  | FILD (arg) -> auto_suffix "fild" arg, [ arg ]
  | HLT -> "hlt", []

  | FCOMPP -> "fcompp", []
  | FCOMP ( Mem ( REAL4, _ ) as arg ) -> "fcomps", [ arg ]
  | FCOMP arg -> "fcompl", [ arg ]
  | FLD ( Mem( REAL4, _ ) as arg ) -> "flds", [ arg ]
  | FLD arg -> "fldl", [ arg ]
  | FNSTSW arg -> "fnstsw", [ arg ]
  | FNSTCW arg -> "fnstcw", [ arg ]
  | FLDCW arg -> "fldcw", [ arg ]

  | FCHS -> "fchs", []
  | FABS -> "fabs", []

  | FADD (Mem ( (REAL8|QWORD), _) as  arg, None) -> "faddl", [arg]
  | FADD (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fadds", [arg]
  | FADD _ -> assert false

  | FMUL (Mem ( (REAL8|QWORD), _) as  arg, None) -> "fmull", [arg]
  | FMUL (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fmuls", [arg]
  | FMUL _ -> assert false

  | FSUB (Mem ( (REAL8|QWORD), _) as  arg, None) -> "fsubl", [arg]
  | FSUB (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fsubs", [arg]
  | FSUB _ -> assert false
  | FSUBR (Mem ( (REAL8|QWORD), _) as  arg, None) -> "fsubrl", [arg]
  | FSUBR (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fsubrs", [arg]
  | FSUBR _ -> assert false

  | FDIV (Mem ( (REAL8|QWORD), _) as  arg, None) -> "fdivl", [arg]
  | FDIV (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fdivs", [arg]
  | FDIV _ -> assert false
  | FDIVR (Mem ( (REAL8|QWORD), _) as  arg, None) -> "fdivrl", [arg]
  | FDIVR (Mem ( (REAL4|DWORD), _) as  arg, None) -> "fdivrs", [arg]
  | FDIVR _ -> assert false

  (* Let's be compatible with prehistoric bugs (part2):
     https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
  *)
  | FSUBP (Regf (ST 0), arg2)  -> "fsubrp", [ Regf (ST 0); arg2 ]
  | FSUBRP (Regf (ST 0), arg2)  -> "fsubp", [ Regf (ST 0); arg2 ]
  | FDIVP (Regf (ST 0), arg2)  -> "fdivrp", [ Regf (ST 0); arg2 ]
  | FDIVRP (Regf (ST 0), arg2)  -> "fdivp", [ Regf (ST 0); arg2 ]

  | FSUBP (arg1, arg2)  -> "fsubp", [ arg1; arg2 ]
  | FSUBRP (arg1, arg2)  -> "fsubrp", [ arg1; arg2 ]
  | FDIVP (arg1, arg2)  -> "fdivp", [ arg1; arg2 ]
  | FDIVRP (arg1, arg2)  -> "fdivrp", [ arg1; arg2 ]

  | FLD1 -> "fld1", []
  | FPATAN -> "fpatan", []
  | FPTAN -> "fptan", []
  | FCOS -> "fcos", []
  | FLDLN2 -> "fldln2", []
  | FLDLG2 -> "fldlg2", []
  | FXCH arg -> "fxch", list_o arg
  | FYL2X -> "fyl2x", []
  | FSIN -> "fsin", []
  | FSQRT -> "fsqrt", []
  | FLDZ -> "fldz", []

  | FADDP (arg1, arg2)  -> "faddp", [ arg1; arg2 ]
  | FMULP (arg1, arg2)  -> "fmulp", [ arg1; arg2 ]

  | INC arg ->  auto_suffix "inc" arg, [ arg ]
  | DEC arg ->  auto_suffix "dec" arg, [ arg ]

  | IMUL (arg1, None) -> auto_suffix "imul" arg1, [ arg1 ]
  | IMUL (arg1, Some arg2) -> auto_suffix "imul" arg1, [arg1 ; arg2 ]
  | IDIV arg ->  auto_suffix "idiv" arg, [ arg ]

  | MOV (
      (Imm (B64, _) as arg1),
      (Reg64 _ as arg2))
    ->  "movabsq", [arg1; arg2]
  | MOV (arg1, arg2) ->  auto_suffix "mov" arg2, [arg1; arg2]
  | MOVZX (arg1, arg2) ->
      auto_suffix (auto_suffix "movz" arg1) arg2, [arg1; arg2]
  | MOVSX (arg1, arg2) ->
      auto_suffix (auto_suffix "movs" arg1) arg2, [arg1; arg2]
  | MOVSS (arg1, arg2) ->  "movss", [arg1; arg2]
  | MOVSXD (arg1, arg2) ->  "movslq", [arg1; arg2]

  | MOVSD (arg1, arg2) ->  "movsd", [ arg1 ; arg2 ]
  | ADDSD (arg1, arg2) ->  "addsd", [ arg1 ; arg2 ]
  | SUBSD (arg1, arg2) ->  "subsd", [ arg1 ; arg2 ]
  | MULSD (arg1, arg2) ->  "mulsd", [ arg1 ; arg2 ]
  | DIVSD (arg1, arg2) ->  "divsd", [ arg1 ; arg2 ]
  | SQRTSD (arg1, arg2) -> "sqrtsd", [ arg1; arg2 ]
  | ROUNDSD (rounding, arg1, arg2) ->
      Printf.sprintf "roundsd.%s" (match rounding with
            RoundDown -> "down"
          | RoundUp -> "up"
          | RoundTruncate -> "trunc"
          | RoundNearest -> "near"), [ arg1; arg2 ]
  | CVTSS2SD (arg1, arg2) ->  "cvtss2sd", [ arg1; arg2 ]
  | CVTSD2SS (arg1, arg2) ->  "cvtsd2ss", [ arg1; arg2 ]

  | CVTSD2SI (arg1, arg2) ->  "cvtsd2si", [ arg1; arg2 ]

  | CVTSI2SD (arg1, arg2) -> auto_suffix "cvtsi2sd" arg1, [ arg1; arg2 ]
  | CVTTSD2SI (arg1, arg2) -> auto_suffix "cvttsd2si" arg2, [ arg1; arg2 ]

  | UCOMISD (arg1, arg2) ->  "ucomisd", [ arg1; arg2 ]
  | COMISD (arg1, arg2) ->  "comisd", [ arg1; arg2 ]

  | CALL arg  ->  "call", [ arg ]
  | JMP arg ->  "jmp", [ arg ]
  | RET ->  "ret", []
  | PUSH arg -> auto_suffix "push" arg, [ arg ]
  | POP  arg -> auto_suffix "pop" arg, [ arg ]

  | TEST (arg1, arg2) ->  auto_suffix "test" arg2, [ arg1; arg2]
  | SET (condition, arg) ->
      Printf.sprintf  "set%s" (string_of_condition condition), [ arg ]
  | J (condition, arg) ->
      Printf.sprintf  "j%s" (string_of_condition condition), [arg]


  | CMOV (condition, arg1, arg2) ->
      Printf.sprintf "cmov%s" (string_of_condition condition), [ arg1; arg2 ]
  | XORPD (arg1, arg2) ->  "xorpd", [ arg1; arg2 ]
  | ANDPD (arg1, arg2) ->  "andpd", [ arg1; arg2 ]
  | MOVLPD (arg1, arg2) ->  "movlpd", [arg1; arg2]
  | MOVAPD (arg1, arg2) ->  "movapd", [arg1; arg2]
  | LEA (arg1, arg2) ->  auto_suffix "lea" arg2, [ arg1; arg2 ]
  | CQTO ->  "cqto", []
  | CDQ -> "cltd", []

  | XCHG (arg1, arg2) -> "xchg", [ arg1; arg2 ]
  | BSWAP arg -> "bswap", [ arg ]

let bprint_instr_name b instr =
  match instr with
  | Global s ->
      Printf.bprintf b "\t.globl\t%s" s;
  | Align (_data,n) ->
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
      let ins, args = split_instr instr in
      bprint b ins;
      bprint_args b instr args

let bprint_instr b instr =
  bprint_instr_name b instr;
  Buffer.add_string b "\n"
