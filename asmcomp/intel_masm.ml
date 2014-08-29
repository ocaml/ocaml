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

open Intel_ast
open Intel_proc

let string_of_data_size = function
  | B64 -> "QWORD"
  | B32 -> "DWORD"
  | B16 -> "WORD"
  | B8 -> "BYTE"

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
  | PROC -> "PROC PTR"

let bprint_arg_mem b string_of_register ptr mem =
  match mem with
  | None, (None, _) -> assert false (* not implemented *)
  | None, (Some (s,_) , 0L) ->
      Printf.bprintf b "%s %s" (string_of_datatype_ptr ptr) s
  | None, (Some (s,_) , d) ->
      if d > 0L then
        Printf.bprintf b "%s %s+%Ld" (string_of_datatype_ptr ptr) s d
      else
        Printf.bprintf b "%s %s%Ld" (string_of_datatype_ptr ptr) s d

  |  Some (reg1, 1, None), (None, 0L) ->
      Printf.bprintf b "%s[%s]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg1);

  |  Some (reg1, 1, None), (None, offset) ->
      Printf.bprintf b "%s[%s%s%Ld]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg1)
        (if offset > 0L then "+" else "")
        offset

  |  Some (reg1, scale, None), (None, 0L) ->
      Printf.bprintf b "%s[%s*%d]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg1)
        scale
  |  Some (reg1, scale, None), (None, offset) ->
      Printf.bprintf b "%s[%s*%d%s%Ld]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg1)
        scale
        (if offset > 0L then "+" else "")
        offset
  |  Some (reg1, 1, Some reg2), (None, 0L) ->
      Printf.bprintf b "%s[%s+%s]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg2)
        (string_of_register reg1)
  |  Some (reg1, 1, None), (Some (s,_), 0L) ->
      Printf.bprintf b "%s[%s+%s]"
        (string_of_datatype_ptr ptr)
        s
        (string_of_register reg1)
  |  Some (reg1, 1, Some reg2), (None, offset) ->
      Printf.bprintf b "%s[%s+%s%s%Ld]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg2)
        (string_of_register reg1)
        (if offset > 0L then "+" else "")
        offset
  |  Some (reg1, 1, None), (Some (s,_), offset ) ->
      Printf.bprintf b "%s[%s+%s%s%Ld]"
        (string_of_datatype_ptr ptr)
        s
        (string_of_register reg1)
        (if offset > 0L then "+" else "")
        offset
  |  Some (reg1, scale, Some reg2), (None, 0L) ->
      Printf.bprintf b "%s[%s+%s*%d]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg2)
        (string_of_register reg1)
        scale
  |  Some (reg1, scale, None), (Some (s,_), 0L ) ->
      Printf.bprintf b "%s[%s+%s*%d]"
        (string_of_datatype_ptr ptr)
        s
        (string_of_register reg1)
        scale
  |  Some (reg1, scale, Some reg2), (None, offset) ->
      Printf.bprintf b "%s[%s+%s*%d%s%Ld]"
        (string_of_datatype_ptr ptr)
        (string_of_register reg2)
        (string_of_register reg1)
        scale
        (if offset > 0L then "+" else "")
        offset
  |  Some (reg1, scale, Some reg2), (Some (s,_), offset) ->
      Printf.bprintf b "%s[%s+%s+%s*%d%s%Ld]"
        (string_of_datatype_ptr ptr)
        s
        (string_of_register reg2)
        (string_of_register reg1)
        scale
        (if offset > 0L then "+" else "")
        offset
  |  Some (reg1, scale, None), (Some (s,_), offset) ->
      Printf.bprintf b "%s[%s+%s*%d%s%Ld]"
        (string_of_datatype_ptr ptr)
        s
        (string_of_register reg1)
        scale
        (if offset > 0L then "+" else "")
        offset

(* TODO: remove need of "ins" *)
let bprint_arg arch b arg =
  match arg with
  (*    | ConstantInt int -> Printf.bprintf b "%d" int *)
  | Imm ( (B8|B16), (None, int)) ->
      Printf.bprintf b "%Ld" int
  | Imm ( B32, (None, int)) ->
      (* force ml64 to use mov reg, imm64 instruction *)
      Printf.bprintf b "0%Lxh" int
  | Imm ( B64, (None, int)) ->
      (* force ml64 to use mov reg, imm64 instruction *)
      Printf.bprintf b "0%LxH" int
  | Imm (_, (Some (s, None),0L)) ->
      Printf.bprintf b "OFFSET %s" s
  | Rel (_, (Some (s, None),0L)) ->
      Printf.bprintf b "%s" s
  | Imm (_, _) | Rel (_, _) -> assert false
  | Reg8 register8 ->
      Printf.bprintf b "%s" (string_of_register8 register8)
  | Reg16 register16 ->
      Printf.bprintf b "%s" (string_of_register16 register16)
  | Reg32 register32 ->
      Printf.bprintf b "%s" (string_of_register32 register32)
  | Reg64 register ->
      Printf.bprintf b "%s" (string_of_register64 register)
  | Regf registerf ->
      Printf.bprintf b "%s" (string_of_registerf registerf)

  (* We don't need to specify RIP on Win64, since EXTERN will provide
     the list of external symbols that need this addressing mode, and
     MASM will automatically use RIP addressing when needed. *)
  | Mem (ptr, M64 (Some (RIP, 1, None), (Some (s,_) , 0L))) ->
      Printf.bprintf b "%s %s" (string_of_datatype_ptr ptr) s
  | Mem (ptr, M64 (Some (RIP, 1, None), (Some (s,_), d))) ->
      Printf.bprintf b "%s %s+%Ld" (string_of_datatype_ptr ptr) s d

  | Mem (ptr, M32 addr) ->
      bprint_arg_mem b string_of_register32 ptr addr
  | Mem (ptr, M64 addr) ->
      bprint_arg_mem b string_of_register64 ptr addr


let bprint_args arch b instr args =
  match args with
  | [] -> ()
  | [ arg ] -> tab b; bprint_arg arch b arg
  | [ arg2; arg1 ] ->
      tab b; bprint_arg arch b arg1;
      Buffer.add_char b ',';
      Buffer.add_char b ' ';
      bprint_arg arch b arg2
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
  | ConstLabel (l, None) -> if l = "." then  "THIS BYTE"      else l
  | ConstLabel _ -> assert false
  | Const ( (B8|B16), n) -> Int64.to_string n
  | Const (B32, n) -> Printf.sprintf "0%Lxh" n
  | Const (B64, n) -> Printf.sprintf "0%LxH" n
  | ConstFloat s ->
      (* MASM doesn't like floating-point constants such as 2e9.
          Turn them into 2.0e9. *)
      let pos_e = ref (-1) and pos_dot = ref (-1) in
      for i = 0 to String.length s - 1 do
        match s.[i] with
          'e'|'E' -> pos_e := i
        | '.'     -> pos_dot := i
        | _       -> ()
      done;
      if !pos_dot < 0 && !pos_e >= 0 then begin
        Printf.sprintf "%s.0%s"
          (String.sub s 0 !pos_e)
          (String.sub s !pos_e (String.length s - !pos_e))
      end else
        s
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



let list_o arg = match arg with None -> [] | Some arg -> [arg]

let bprint_instr_name b arch instr =
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

  | Mode386 ->  Printf.bprintf b "\t.386"
  | Model name ->  Printf.bprintf b "\t.MODEL %s" name (* name = FLAT *)
  | Section (name, None, []) ->
      Printf.bprintf b "\t%s" (match name with
          | [".text"] -> ".CODE"
          | [".data"] -> ".DATA"
          | _ -> assert false)
  | Section _ -> assert false

  | Constant (n, ptr) ->
      Printf.bprintf b "\t%s\t%s" (string_of_data_size ptr) (string_of_constant n)

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
      let name, args =
        match instr with


        | NEG arg ->  "neg", [arg]
        | NOP -> "nop", []
        | ADD (arg1, arg2) ->  "add", [arg1; arg2]
        | SUB (arg1, arg2) ->   "sub", [arg1; arg2]
        | XOR (arg1, arg2) ->   "xor", [arg1; arg2]
        | OR (arg1, arg2) ->    "or", [arg1; arg2]
        | AND (arg1, arg2) ->   "and", [arg1; arg2]
        | CMP (arg1, arg2) ->  "cmp", [arg1; arg2]

        | LEAVE -> "leave", []
        | SAR (arg1, arg2) ->  "sar", [arg1; arg2]
        | SHR (arg1, arg2) ->  "shr", [arg1; arg2]
        | SAL (arg1, arg2) ->  "sal", [arg1; arg2]

        | FSTP arg -> "fstp", [ arg ]
        | FILD arg -> "fild", [arg]
        | FCOMPP -> "fcompp", []
        | FCOMP arg -> "fcomp", [ arg ]
        | FLD arg -> "fld", [ arg ]
        | FLDCW arg -> "fldcw", [ arg ]
        | FISTP arg -> "fistp", [ arg]

        | FNSTSW arg -> "fnstsw", [ arg ]
        | FNSTCW arg -> "fnstcw", [ arg ]

        | FCHS -> "fchs", []
        | FABS -> "fabs", []
        | FADD (arg, None) -> "fadd", [arg]
        | FADD _ -> assert false
        | FSUB (arg, None) -> "fsub", [arg]
        | FSUB _ -> assert false
        | FMUL (arg, None) -> "fmul", [arg]
        | FMUL _ -> assert false
        | FDIV (arg, None) -> "fdiv", [arg]
        | FDIV _ -> assert false
        | FSUBR (arg, None) -> "fsubr", [arg]
        | FSUBR _ -> assert false
        | FDIVR (arg, None) -> "fdivr", [arg]
        | FDIVR _ -> assert false

        | FADDP (arg1, arg2)  -> "faddp", [ arg1; arg2 ]
        | FSUBP (arg1, arg2)  -> "fsubp", [ arg1; arg2 ]
        | FMULP (arg1, arg2)  -> "fmulp", [ arg1; arg2 ]
        | FDIVP (arg1, arg2)  -> "fdivp", [ arg1; arg2 ]
        | FSUBRP (arg1, arg2)  -> "fsubrp", [ arg1; arg2 ]
        | FDIVRP (arg1, arg2)  -> "fdivrp", [ arg1; arg2 ]

        | INC arg ->  "inc", [ arg ]
        | DEC arg ->  "dec", [ arg ]

        | IMUL (arg1, arg2) ->  "imul", arg1 :: list_o arg2
        | IDIV arg ->  "idiv", [ arg ]
        | HLT -> assert false
        | MOV (arg1, arg2) ->  "mov", [ arg1; arg2]

        | MOVZX (arg1, arg2) ->  "movzx", [ arg1; arg2]
        | MOVSX (arg1, arg2) ->  "movsx",  [ arg1; arg2]
        | MOVSS (arg1, arg2) ->  "movss", [ arg1; arg2 ]
        | MOVSXD (arg1, arg2) ->  "movsxd", [ arg1; arg2 ]

        | MOVSD (arg1, arg2) ->  "movsd", [ arg1; arg2 ]
        | ADDSD (arg1, arg2) ->  "addsd", [ arg1 ; arg2 ]
        | SUBSD (arg1, arg2) ->  "subsd", [ arg1 ; arg2 ]
        | MULSD (arg1, arg2) ->  "mulsd", [ arg1 ; arg2 ]
        | DIVSD (arg1, arg2) ->  "divsd", [ arg1 ; arg2 ]
        | SQRTSD (arg1, arg2) -> "sqrtsd", [ arg1; arg2]
        | ROUNDSD (rounding, arg1, arg2) ->
            Printf.sprintf "roundsd.%s" (match rounding with
                  RoundDown -> "down"
                | RoundUp -> "up"
                | RoundTruncate -> "trunc"
                | RoundNearest -> "near"), [ arg1 ; arg2 ]
        | CVTSS2SD (arg1, arg2) ->  "cvtss2sd", [ arg1; arg2 ]
        | CVTSD2SS (arg1, arg2) ->  "cvtsd2ss", [ arg1; arg2 ]
        | CVTSI2SD (arg1, arg2) ->  "cvtsi2sd", [ arg1; arg2 ]
        | CVTSD2SI (arg1, arg2) ->  "cvtsd2si", [ arg1; arg2 ]
        | CVTTSD2SI (arg1, arg2) ->  "cvttsd2si", [ arg1; arg2 ]
        | UCOMISD (arg1, arg2) ->  "ucomisd", [ arg1; arg2]
        | COMISD (arg1, arg2) ->  "comisd", [arg1; arg2]

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

        | CALL arg  ->  "call", [ arg ]
        | JMP arg ->  "jmp", [ arg]
        | RET ->  "ret", []
        | PUSH arg ->  "push", [arg]
        | POP arg ->  "pop", [arg]

        | TEST (arg1, arg2) ->  "test", [arg1; arg2]
        | SET (condition, arg) ->
            Printf.sprintf  "set%s" (string_of_condition condition), [ arg ]
        | J (condition, arg) ->
            Printf.sprintf  "j%s" (string_of_condition condition), [ arg ]

        | CMOV (condition, arg1, arg2) ->
            Printf.sprintf "cmov%s" (string_of_condition condition), [ arg1; arg2]
        | XORPD (arg1, arg2) ->  "xorpd", [ arg1; arg2 ]
        | ANDPD (arg1, arg2) ->  "andpd", [ arg1; arg2 ]
        | MOVLPD (arg1, arg2) ->  "movlpd", [ arg1; arg2 ]
        | MOVAPD (arg1, arg2) ->  "movapd", [ arg1; arg2 ]
        | CDQ -> "cdq", []

        | LEA (arg1, arg2) ->  "lea", [arg1; arg2]
        | CQTO ->  "cqo", []
        | XCHG (arg1, arg2) -> "xchg", [ arg1; arg2 ]
        | BSWAP arg -> "bswap", [ arg ]

      in
      bprint b name;
      bprint_args arch b instr args;
      ()

let bprint_instr b arch instr =
  bprint_instr_name b arch instr;
  Buffer.add_string b "\n"

