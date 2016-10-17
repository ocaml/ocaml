(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open X86_ast
open X86_proc

let bprintf = Printf.bprintf

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let opt_displ b displ =
  if displ = 0 then ()
  else if displ > 0 then bprintf b "+%d" displ
  else bprintf b "%d" displ

let arg_mem b {arch; typ=_; idx; scale; base; sym; displ} =
  let string_of_register =
    match arch with
    | X86 -> string_of_reg32
    | X64 -> string_of_reg64
  in
  begin match sym with
  | None ->
      if displ <> 0 || scale = 0 then
        Buffer.add_string b (string_of_int displ)
  | Some s ->
      Buffer.add_string b s;
      opt_displ b displ
  end;
  if scale <> 0 then begin
    Buffer.add_char b '(';
    begin match base with
    | None -> ()
    | Some base -> print_reg b string_of_register base
    end;
    if base != None || scale <> 1 then Buffer.add_char b ',';
    print_reg b string_of_register idx;
    if scale <> 1 then bprintf b ",%s" (string_of_int scale);
    Buffer.add_char b ')'
  end

let arg b = function
  | Sym x -> Buffer.add_char b '$'; Buffer.add_string b x
  | Imm x -> bprintf b "$%Ld" x
  | Reg8L x -> print_reg b string_of_reg8l x
  | Reg8H x -> print_reg b string_of_reg8h x
  | Reg16 x -> print_reg b string_of_reg16 x
  | Reg32 x -> print_reg b string_of_reg32 x
  | Reg64 x -> print_reg b string_of_reg64 x
  | Regf x  -> print_reg b string_of_registerf x
  | Mem addr -> arg_mem b addr
  | Mem64_RIP (_, s, displ) -> bprintf b "%s%a(%%rip)" s opt_displ displ

let typeof = function
  | Mem {typ; _} | Mem64_RIP (typ, _, _) -> typ
  | Reg8L _ | Reg8H _ -> BYTE
  | Reg16 _ -> WORD
  | Reg32 _ -> DWORD
  | Reg64 _ -> QWORD
  | Imm _ | Sym _ -> NONE
  | Regf _ -> assert false

let suf arg =
  match typeof arg with
  | BYTE -> "b"
  | WORD -> "w"
  | DWORD | REAL8 -> "l"
  | QWORD -> "q"
  | REAL4 -> "s"
  | NONE -> ""
  | OWORD | NEAR | PROC -> assert false

let i0 b s = bprintf b "\t%s" s
let i1 b s x = bprintf b "\t%s\t%a" s arg x
let i1_s b s x = bprintf b "\t%s%s\t%a" s (suf x) arg x
let i2 b s x y = bprintf b "\t%s\t%a, %a" s arg x arg y
let i2_s b s x y = bprintf b "\t%s%s\t%a, %a" s (suf y) arg x arg y
let i2_ss b s x y = bprintf b "\t%s%s%s\t%a, %a" s (suf x) (suf y) arg x arg y

let i1_call_jmp b s = function
  (* this is the encoding of jump labels: don't use * *)
  | Mem {arch=X86; idx=_;   scale=0; base=None; sym=Some _; _} as x ->
      i1 b s x
  | Reg32 _ | Reg64 _ | Mem _  | Mem64_RIP _ as x ->
      bprintf b "\t%s\t*%a" s arg x
  | Sym x -> bprintf b "\t%s\t%s" s x
  | _ -> assert false

let print_instr b = function
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
  | CQO ->  i0 b "cqto"
  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b ("cvtsi2sd" ^ suf arg1) arg1 arg2
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
  | FLD (Mem {typ=REAL4; _} as arg) -> i1 b "flds" arg
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
  | FSTP (Mem {typ=REAL4; _} as arg) -> i1 b "fstps" arg
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
  | MOV ((Sym _ as arg1), (Reg64 _ as arg2)) when Target_system.windows ->
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

   The AT&T syntax has a bug for fsub/fdiv/fsubr/fdivr instructions when
   the source register is %st and the destination is %st(i).  In those
   case, AT&T use fsub (resp. fsubr) in place of fsubr (resp. fsub),
   and idem for fdiv/fdivr.

   Concretely, AT&T syntax interpretation of:

      fsub  %st, %st(3)

   should normally be:

      %st(3) := %st(3) - %st

   but it should actually be interpreted as:

      %st(3) := %st - %st(3)

   which means the FSUBR instruction should be used.
*)


let print_line b = function
  | Ins instr -> print_instr b instr
  | Directive d -> Asm_directives.Directive.print b d
  | MASM_directive _ ->
    Misc.fatal_error "Cannot emit MASM directives to gas target"

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  output_string oc "\t.file \"\"\n"; (* PR#7037 *)
  List.iter
    (fun i ->
       Buffer.clear b;
       print_line b i;
       Buffer.add_char b '\n';
       Buffer.output_buffer oc b;
    )
    lines
