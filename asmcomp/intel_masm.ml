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

open Intel_ast
open Intel_proc

let tab b = Buffer.add_char b '\t'

let string_of_datatype_ptr = function
  | QWORD -> "QWORD PTR "
  | OWORD -> "OWORD PTR "
  | NO -> ""
  | REAL4 -> "REAL4 PTR "
  | REAL8 -> "REAL8 PTR "
  | REAL10 -> "REAL10 PTR "
  | BYTE -> "BYTE PTR "
  | TBYTE -> "TBYTE PTR "
  | WORD -> "WORD PTR "
  | DWORD -> "DWORD PTR "
  | NEAR -> "NEAR PTR "
  | PROC -> "PROC PTR "

let bprint_arg_mem b string_of_register {typ; idx; scale; base; displ} =
  Buffer.add_string b (string_of_datatype_ptr typ);
  Buffer.add_char b '[';
  let (s, o) = displ in
  begin match s with
  | None -> ()
  | Some s -> Buffer.add_string b s;
  end;
  if scale <> 0 then begin
    if s <> None then Buffer.add_char b '+';
    Buffer.add_string b (string_of_register idx);
    if scale <> 1 then Printf.bprintf b "*%d" scale;
  end;
  begin match base with
  | None -> ()
  | Some r ->
      assert(scale > 0);
      Buffer.add_char b '+';
      Buffer.add_string b (string_of_register r);
  end;
  begin if o > 0L then Printf.bprintf b "+%Ld" o
    else if o < 0L then Printf.bprintf b "%Ld" o
  end;
  Buffer.add_char b ']'

let bprint_arg b arg =
  match arg with
  | Imm n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L ->
      Printf.bprintf b "%Ld" n
  | Imm int ->
      (* force ml64 to use mov reg, imm64 instruction *)
      Printf.bprintf b "0%LxH" int
  | Sym s -> Printf.bprintf b "OFFSET %s" s
  | Reg8 x -> Buffer.add_string b (string_of_register8 x)
  | Reg16 x -> Buffer.add_string b (string_of_register16 x)
  | Reg32 x -> Buffer.add_string b (string_of_register32 x)
  | Reg64 x -> Buffer.add_string b (string_of_register64 x)
  | Regf x -> Buffer.add_string b (string_of_registerf x)

  (* We don't need to specify RIP on Win64, since EXTERN will provide
     the list of external symbols that need this addressing mode, and
     MASM will automatically use RIP addressing when needed. *)
  | Mem64 {typ; idx=RIP; scale=1; base=None; displ=(Some s, 0L)} ->
      Printf.bprintf b "%s %s" (string_of_datatype_ptr typ) s
  | Mem64 {typ; idx=RIP; scale=1; base=None; displ=(Some s, d)} ->
      Printf.bprintf b "%s %s+%Ld" (string_of_datatype_ptr typ) s d

  | Mem32 addr -> bprint_arg_mem b string_of_register32 addr
  | Mem64 addr -> bprint_arg_mem b string_of_register64 addr


let rec string_of_constant = function
  | ConstLabel _ | Const _ as c -> string_of_simple_constant c
  | ConstAdd (c1, c2) ->
      (string_of_simple_constant c1) ^ " + " ^ (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      (string_of_simple_constant c1) ^ " - " ^ (string_of_simple_constant c2)

and string_of_simple_constant = function
  | ConstLabel l -> if l = "." then "THIS BYTE" else l
  | Const n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L -> Int64.to_string n
  | Const n -> Printf.sprintf "0%LxH" n
  | ConstAdd (c1, c2) ->
      Printf.sprintf "(%s + %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)
  | ConstSub (c1, c2) ->
      Printf.sprintf "(%s - %s)"
        (string_of_simple_constant c1) (string_of_simple_constant c2)

let buf_bytes_directive b directive s =
  let pos = ref 0 in
  for i = 0 to String.length s - 1 do
    if !pos = 0
    then begin
      if i > 0 then Buffer.add_char b '\n';
      Buffer.add_char b '\t';
      Buffer.add_string b directive;
      Buffer.add_char b '\t';
    end
    else Buffer.add_char b ',';
    Printf.bprintf b "%d" (Char.code s.[i]);
    incr pos;
    if !pos >= 16 then begin pos := 0 end
  done

let i0 b s =
  tab b;
  Buffer.add_string b s

let i1 b s x =
  tab b;
  Buffer.add_string b s;
  tab b;
  bprint_arg b x

let i2 b s x y =
  tab b;
  Buffer.add_string b s;
  tab b;
  bprint_arg b y;
  Buffer.add_char b ',';
  Buffer.add_char b ' ';
  bprint_arg b x

let i1_call_jmp b s x =
  match x with
  | Sym x ->
      tab b;
      Buffer.add_string b s;
      tab b;
      Buffer.add_string b x
  | _ ->
      i1 b s x

let print_instr b = function
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | ADD (arg1, arg2) -> i2 b "add" arg1 arg2
  | SUB (arg1, arg2) -> i2 b "sub" arg1 arg2
  | XOR (arg1, arg2) -> i2 b "xor" arg1 arg2
  | OR (arg1, arg2) -> i2 b "or" arg1 arg2
  | AND (arg1, arg2) -> i2 b "and" arg1 arg2
  | CMP (arg1, arg2) -> i2 b "cmp" arg1 arg2

  | LEAVE -> i0 b "leave"
  | SAR (arg1, arg2) -> i2 b "sar" arg1 arg2
  | SHR (arg1, arg2) -> i2 b "shr" arg1 arg2
  | SAL (arg1, arg2) -> i2 b "sal" arg1 arg2

  | FSTP arg -> i1 b "fstp" arg
  | FILD arg -> i1 b "fild" arg
  | FCOMPP -> i0 b "fcompp"
  | FCOMP arg -> i1 b "fcomp" arg
  | FLD arg -> i1 b "fld" arg
  | FLDCW arg -> i1 b "fldcw" arg
  | FISTP arg -> i1 b "fistp" arg

  | FNSTSW arg -> i1 b "fnstsw" arg
  | FNSTCW arg -> i1 b "fnstcw" arg

  | FCHS -> i0 b "fchs"
  | FABS -> i0 b "fabs"
  | FADD arg -> i1 b "fadd" arg
  | FSUB arg -> i1 b "fsub" arg
  | FMUL arg -> i1 b "fmul" arg
  | FDIV arg -> i1 b "fdiv" arg
  | FSUBR arg -> i1 b "fsubr" arg
  | FDIVR arg -> i1 b "fdivr" arg

  | FADDP (arg1, arg2)  -> i2 b "faddp" arg1 arg2
  | FSUBP (arg1, arg2)  -> i2 b "fsubp" arg1 arg2
  | FMULP (arg1, arg2)  -> i2 b "fmulp" arg1 arg2
  | FDIVP (arg1, arg2)  -> i2 b "fdivp" arg1 arg2
  | FSUBRP (arg1, arg2)  -> i2 b "fsubrp" arg1 arg2
  | FDIVRP (arg1, arg2)  -> i2 b "fdivrp" arg1 arg2

  | INC arg -> i1 b "inc" arg
  | DEC arg -> i1 b "dec" arg

  | IMUL (arg, None) -> i1 b "imul" arg
  | IMUL (arg1, Some arg2) -> i2 b "imul" arg1 arg2
  | IDIV arg -> i1 b "idiv" arg
  | HLT -> assert false
  | MOV (Imm n as arg1, Reg64 r) when
      n >= 0x8000_0000L && n <= 0xFFFF_FFFFL ->
      (* Work-around a bug in ml64.  Use a mov to the corresponding
         32-bit lower register when the constant fits in 32-bit.
         The associated higher 32-bit register will be zeroed. *)
      i2 b "mov" arg1 (Reg32 (Intel_proc.reg_low_32 r))
  | MOV (arg1, arg2) -> i2 b "mov" arg1 arg2

  | MOVZX (arg1, arg2) -> i2 b "movzx" arg1 arg2
  | MOVSX (arg1, arg2) -> i2 b "movsx" arg1 arg2
  | MOVSS (arg1, arg2) -> i2 b "movss" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movsxd" arg1 arg2

  | MOVSD (arg1, arg2) -> i2 b "movsd" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | MULSD (arg1, arg2) -> i2 b "mulsd" arg1 arg2
  | DIVSD (arg1, arg2) -> i2 b "divsd" arg1 arg2
  | SQRTSD (arg1, arg2) -> i2 b "sqrtsd" arg1 arg2
  | ROUNDSD (rounding, arg1, arg2) ->
      i2 b
        (Printf.sprintf "roundsd.%s" (match rounding with
               RoundDown -> "down"
             | RoundUp -> "up"
             | RoundTruncate -> "trunc"
             | RoundNearest -> "near")) arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b "cvtsi2sd" arg1 arg2
  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2
  | CVTTSD2SI (arg1, arg2) -> i2 b "cvttsd2si" arg1 arg2
  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2

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

  | CALL arg  -> i1_call_jmp b "call" arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | RET -> i0 b "ret"
  | PUSH arg -> i1 b "push" arg
  | POP arg -> i1 b "pop" arg

  | TEST (arg1, arg2) -> i2 b "test" arg1 arg2
  | SET (condition, arg) ->
      i1 b
        (Printf.sprintf  "set%s" (string_of_condition condition)) arg
  | J (condition, arg) -> (* TODO: fix sym case *)
      i1_call_jmp b
        (Printf.sprintf  "j%s" (string_of_condition condition)) arg

  | CMOV (condition, arg1, arg2) ->
      i2 b (Printf.sprintf "cmov%s" (string_of_condition condition))
        arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | MOVLPD (arg1, arg2) -> i2 b "movlpd" arg1 arg2
  | MOVAPD (arg1, arg2) -> i2 b "movapd" arg1 arg2
  | CDQ -> i0 b "cdq"

  | LEA (arg1, arg2) -> i2 b "lea" arg1 arg2
  | CQTO -> i0 b "cqo"
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg


let bprint_instr_name b instr =
  match instr with
  | Global s ->
      Printf.bprintf b "\tPUBLIC\t%s" s
  | Align (_data,n) ->
      Printf.bprintf b "\tALIGN\t%d" n
  | NewLabel (s, NO) ->
      Printf.bprintf b "%s:" s
  | NewLabel (s, ptr) ->
      Printf.bprintf b "%s LABEL %s" s (string_of_datatype ptr)
  | Comment s ->
      Printf.bprintf b " ; %s " s

  | Cfi_startproc -> assert false
  | Cfi_endproc -> assert false
  | Cfi_adjust_cfa_offset _ -> assert false
  | File _ -> assert false
  | Loc _ -> assert false
  | Private_extern _ -> assert false
  | Indirect_symbol _ -> assert false
  | Type _ -> assert false
  | Size _ -> assert false

  | Mode386 -> Printf.bprintf b "\t.386"
  | Model name -> Printf.bprintf b "\t.MODEL %s" name (* name = FLAT *)
  | Section (name, None, []) ->
      Printf.bprintf b "\t%s" (match name with
          | [".text"] -> ".CODE"
          | [".data"] -> ".DATA"
          | _ -> assert false)
  | Section _ -> assert false

  | Quad n -> Printf.bprintf b "\tQWORD\t%s" (string_of_constant n)
  | Long n -> Printf.bprintf b "\tDWORD\t%s" (string_of_constant n)
  | Word n -> Printf.bprintf b "\tWORD\t%s" (string_of_constant n)
  | Byte n -> Printf.bprintf b "\tBYTE\t%s" (string_of_constant n)

  | External (s, ptr) ->
      Printf.bprintf b "\tEXTRN\t%s: %s" s (string_of_datatype ptr)

  | End ->
      Printf.bprintf b "END"

  | Space n ->
      Printf.bprintf b "\tBYTE\t%d DUP (?)" n
  | Set (arg1, arg2) ->
      Printf.bprintf b "\t.set %s, %s" arg1 (string_of_constant arg2)

  | Bytes s -> buf_bytes_directive b "BYTE" s

  | Ins instr ->
      print_instr b instr

let bprint_instr b instr =
  bprint_instr_name b instr;
  Buffer.add_char b '\n'
