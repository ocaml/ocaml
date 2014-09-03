(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

[@@@ocaml.warning "+A-42-4"]

open Misc
open Intel_ast

type system =
  (* 32 bits and 64 bits *)
  | S_macosx
  | S_gnu
  | S_cygwin

  (* 32 bits only *)
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw

  (* 64 bits only *)
  | S_win64
  | S_linux
  | S_mingw64

  | S_unknown


let string_of_datatype = function
  | QWORD -> "QWORD"
  | OWORD -> "OWORD"
  | NO -> assert false
  | REAL4 -> "REAL4"
  | REAL8 -> "REAL8"
  | REAL10 -> "REAL10"
  | BYTE -> "BYTE"
  | TBYTE -> "TBYTE"
  | WORD -> "WORD"
  | DWORD -> "DWORD"
  | NEAR -> "NEAR"
  | PROC -> "PROC"


let system = match Config.system with
  | "macosx" -> S_macosx
  | "solaris" -> S_solaris
  | "win32" -> S_win32
  | "linux_elf" -> S_linux_elf
  | "bsd_elf" -> S_bsd_elf
  | "beos" -> S_beos
  | "gnu" -> S_gnu
  | "cygwin" -> S_cygwin
  | "mingw" -> S_mingw
  | "mingw64" -> S_mingw64
  | "win64" -> S_win64
  | "linux" -> S_linux

  | _ -> S_unknown

let string_of_string_literal s =
  let b = Buffer.create (String.length s + 2) in
  let last_was_escape = ref false in
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.bprintf b "\\%o" (Char.code c)
      else Buffer.add_char b c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      Buffer.add_char b c;
      last_was_escape := false
    end else begin
      Printf.bprintf b "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  Buffer.contents b

let string_of_symbol prefix s =
  let b = Buffer.create (1 + String.length s) in
  Buffer.add_string b prefix;
  String.iter
    (function
      | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c -> Buffer.add_char b c
      | c -> Printf.bprintf b "$%02x" (Char.code c)
    )
    s;
  Buffer.contents b


let string_of_register64 reg64 =
  match reg64 with
  | RAX -> "rax"
  | RBX -> "rbx"
  | RDI -> "rdi"
  | RSI -> "rsi"
  | RDX -> "rdx"
  | RCX -> "rcx"
  | RBP -> "rbp"
  | RSP -> "rsp"
  | R8 -> "r8"
  | R9 -> "r9"
  | R10 -> "r10"
  | R11 -> "r11"
  | R12 -> "r12"
  | R13 -> "r13"
  | R14 -> "r14"
  | R15 -> "r15"
  | RIP -> "rip"

let string_of_register8 reg8 =
  match reg8 with
  | AL -> "al"
  | BL -> "bl"
  | DL -> "dl"
  | CL -> "cl"
  | AH -> "ah"
  | BH -> "bh"
  | CH -> "ch"
  | DH -> "dh"
  | DIL -> "dil"
  | SIL -> "sil"
  | R8B -> "r8b"
  | R9B -> "r9b"
  | R10B -> "r10b"
  | R11B -> "r11b"
  | BPL -> "bpl"
  | R12B -> "r12b"
  | R13B -> "r13b"
  | SPL -> "spl"
  | R14B -> "r14b"
  | R15B -> "r15b"

let string_of_register16 reg16 =
  match reg16 with
  | AX -> "ax"
  | BX -> "bx"
  | DI -> "di"
  | SI -> "si"
  | DX -> "dx"
  | CX -> "cx"
  | SP -> "sp"
  | BP -> "bp"
  | R8W -> "r8w"
  | R9W -> "r9w"
  | R10W -> "r10w"
  | R11W -> "r11w"
  | R12W -> "r12w"
  | R13W -> "r13w"
  | R14W -> "r14w"
  | R15W -> "r15w"

let string_of_register32 reg32 =
  match reg32 with
  | R32 RAX -> "eax"
  | R32 RBX -> "ebx"
  | R32 RDI -> "edi"
  | R32 RSI -> "esi"
  | R32 RDX -> "edx"
  | R32 RCX -> "ecx"
  | R32 RSP -> "esp"
  | R32 RBP -> "ebp"
  | R32 R8 -> "r8d"
  | R32 R9 -> "r9d"
  | R32 R10 -> "r10d"
  | R32 R11 -> "r11d"
  | R32 R12 -> "r12d"
  | R32 R13 -> "r13d"
  | R32 R14 -> "r14d"
  | R32 R15 -> "r15d"
  | R32 RIP -> assert false

let string_of_registerf regf =
  match regf with
  | XMM n -> Printf.sprintf "xmm%d" n
  | TOS -> Printf.sprintf "tos"
  | ST n -> Printf.sprintf "st(%d)" n

let string_of_condition condition =
  match condition with
  | E -> "e"
  | AE -> "ae"
  | A -> "a"
  | GE -> "ge"
  | G -> "g"
  | NE -> "ne"
  | B -> "b"
  | BE -> "be"
  | L -> "l"
  | LE -> "le"
  | NLE -> "nle"
  | NG -> "ng"
  | NL -> "nl"
  | NGE -> "nge"
  | PO -> "po"
  | NP -> "np"
  | PE -> "pe"
  | P -> "p"
  | NS -> "ns"
  | S -> "s"
  | NBE -> "nbe"
  | NA -> "na"
  | NZ -> "nz"
  | Z -> "z"
  | NC -> "nc"
  | NB -> "nb"
  | NAE -> "nae"
  | C -> "c"
  | NO -> "no"
  | O -> "o"


(* These hooks can be used to insert optimization passes on
   the assembly code. *)
let assembler_passes = ref ([] : (asm_program -> asm_program) list)

let internal_assembler = ref None
let register_internal_assembler f = internal_assembler := Some f

(* Which asm conventions to use *)
let masm =
  match system with
  | S_win32 | S_win64 -> true
  | _ -> false

(* Shall we use an external assembler command ?
   If [binary_content] contains some data, we can directly
   save it. Otherwise, we have to ask an external command.
*)
let binary_content = ref None

let compile infile outfile =
  if masm then
    Ccomp.command (Config.asm ^
                   Filename.quote outfile ^ " " ^ Filename.quote infile ^
                   (if !Clflags.verbose then "" else ">NUL"))
  else
    Ccomp.command (Config.asm ^ " -o " ^
                   Filename.quote outfile ^ " " ^ Filename.quote infile)

let assemble_file infile outfile =
  match !binary_content with
  | None -> compile infile outfile
  | Some content -> content outfile; binary_content := None; 0

let asm_code = ref []

let directive dir = asm_code := dir :: !asm_code
let emit ins = directive (Ins ins)

let reset_asm_code () = asm_code := []


let split_sections instrs =
  let sections = ref StringMap.empty in
  let section s =
    try
      StringMap.find s !sections
    with Not_found ->
      let section = (ref [], { sec_name = s; sec_instrs = [||] }) in
      sections := StringMap.add s section !sections;
      section
  in
  let current_section = ref (section ".text") in
  List.iter
    (function
       | Section ([sec], _, _) ->
           current_section := section sec
       | ins ->
           let (section, _) = !current_section in
           section := ins :: !section
    )
    instrs;
  StringMap.map
    (fun (ref, section) ->
       { section with sec_instrs = Array.of_list (List.rev !ref) }
    )
    !sections


let generate_code asm =
  let instrs = List.rev !asm_code in
  let instrs =
    List.fold_left (fun instrs pass -> pass instrs) instrs !assembler_passes
  in
  begin match asm with
  | Some (oc, syntax) ->
      let b = Buffer.create 10000 in
      List.iter
        (fun i ->
           Buffer.clear b;
           syntax b i;
           Buffer.output_buffer oc b
        )
        instrs
  | None -> ()
  end;
  begin match !internal_assembler with
  | Some f -> binary_content := Some (f (split_sections instrs))
  | None -> binary_content := None
  end
