(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Description of inline asm primitives *)

(* Not implemented:
   
  - Explicit register naming ("sub %[p1], %[out_name]")
  - '*' modifier
  - Simple constraints: 'o', 'V', '<', '>', 'n', 'E', 'F', 's', 'X', 'p'
  - X86 machine constraints: 'R', 'q', 'Q', 'A', 'f', 't', 'u', 'y', 'Yz', 'I',
    'J', 'K', 'L', 'M', 'N', 'G', 'C', 'e', 'Z'
  - X87 registers
  - MMX registers
  - All other machine constraints. *)

exception Inline_asm_error of string

type arg_kind =
  [ `Addr
  | `Float
  | `Int
  | `Int32
  | `Int64
  | `M128d
  | `M256d
  | `M128i
  | `M256i
  | `Nativeint
  | `Unit ]

type register =
  [ `D  | `S  | `a   | `b   | `c   | `d
  | `r8 | `r9 | `r10 | `r11 | `r12 | `r13
  | `x0 | `x1 | `x2  | `x3  | `x4  | `x5  | `x6  | `x7
  | `x8 | `x9 | `x10 | `x11 | `x12 | `x13 | `x14 | `x15 ]

type alternative = {
  mach_register    : [ `r | `sse | register ];
  copy_to_output   : int option;
  commutative      : bool;
  disparage        : int;
  earlyclobber     : bool;
  immediate        : bool;
  memory           : [ `no | `m8 | `m16 | `m32 | `m64 | `m128 | `m256 ];
  reload_disparage : int;
  register         : bool }

type arg = {
  kind         : arg_kind;
  input        : bool;
  output       : bool;
  alternatives : alternative array }

type arg_modifier =
  | None
  | R8L
  | R8H
  | R16
  | R32
  | R64
  | XMM
  | YMM

type template_item =
    Emit_arg of int * arg_modifier
  | Emit_dialect of template array
  | Emit_string of string
  | Emit_unique
and template = template_item array

type inline_asm = {
  template : template;
  args     : arg array;
  clobber  : [ `cc | `memory | register ] list;
  decl     : string array }

(** Parses the template given as string with arguments given as %i, for example
    "addpd %0 %1".  Double percentage is unescaped. *)
let parse_template ~nargs template =
  let template = Scanf.unescaped template in
  (* Returns the index of the first non digit character with index greater or equal [i].
     If such character does not exist, returns the length of the string. *)
  let rec first_non_digit s i =
    if i = String.length s then i
    else if s.[i] >= '0' && s.[i] <= '9' then first_non_digit s (i + 1)
    else i
  in
  let rec loop acc s i =
    let len = String.length s in
    if len = 0 then acc
    else if i + 1 = len then Emit_string s :: acc
    else
      match s.[i], s.[i + 1] with
        '%', '%' -> (* Unescape *)
          let s = (String.sub s 0 i) ^ (String.sub s (i + 1) (len - i - 1)) in
          loop acc s (i + 1)
      | '%', '=' ->
          let acc = if i = 0 then acc else Emit_string (String.sub s 0 i) :: acc in
          loop (Emit_unique :: acc) (String.sub s (i + 2) (len - i - 2)) 0
      | '%', ( 'B' | 'L' | 'Q' | 'S' | 'T' | 'W' | 'b' | 'h' | 'w' | 'k' | 'q' | 't'
             | 'x' as c)
        when len > i + 1 && s.[i + 2] >= '0' && s.[i + 2] <= '9' ->
          let acc = if i = 0 then acc else Emit_string (String.sub s 0 i) :: acc in
          let j = first_non_digit s (i + 2) in
          let arg = int_of_string (String.sub s (i + 2) (j - i - 2)) in
          if arg < 0 || arg >= nargs then
            raise(Inline_asm_error(Printf.sprintf
              "invalid 'asm': operand number out of range"));
          let c =
            begin match c with
              'B' -> `dialect 'b'
            | 'L' -> `dialect 'l'
            | 'Q' -> `dialect 'l'
            | 'S' -> `dialect 's'
            | 'T' -> `dialect 't'
            | 'W' -> `dialect 'w'
            | 'b' -> `reg R8L
            | 'h' -> `reg R8H
            | 'w' -> `reg R16
            | 'k' -> `reg R32
            | 'q' -> `reg R64
            | 't' -> `reg YMM
            | 'x' -> `reg XMM
            | _   -> assert false
            end |> function
              `dialect d ->
                Emit_dialect [| [| Emit_string (String.make 1 d) |];
                                [| Emit_string "" |] |]
            | `reg r -> Emit_arg (arg, r)
          in
          let acc = c :: acc in
          if j < len then loop acc (String.sub s j (len - j)) 0
          else acc
      | '%', c when c >= '0' && c <= '9' -> (* Argument *)
          let acc = if i = 0 then acc else Emit_string (String.sub s 0 i) :: acc in
          let j = first_non_digit s (i + 1) in
          let arg = int_of_string (String.sub s (i + 1) (j - i - 1)) in
          if arg < 0 || arg >= nargs then
            raise(Inline_asm_error(Printf.sprintf
              "invalid 'asm': operand number out of range"));
          let acc = Emit_arg (arg, None) :: acc in
          if j < len then loop acc (String.sub s j (len - j)) 0
          else acc
      | _, _ -> loop acc s (i + 1)
  in
  List.rev (loop [] template 0) |> Array.of_list

let parse kinds decl =
  let error f = Printf.ksprintf (fun msg -> raise (Inline_asm_error msg)) f in
  let kinds = Array.of_list kinds in
  let decl = Array.of_list decl in
  let nargs = Array.length kinds in
  if Array.length decl < Array.length kinds + 2 then
    error "expected constraints for all arguments";
  let template = parse_template ~nargs decl.(1) in
  let args = Array.mapi (fun i kind ->
    let arg = ref {
      kind;
      input        = true;
      output       = false;
      alternatives = [| |] } in
    let alt = ref {
      mach_register    = `r;
      copy_to_output   = None;
      commutative      = false;
      disparage        = 1;
      earlyclobber     = false;
      immediate        = false;
      memory           = `no;
      reload_disparage = 4;
      register         = false } in
    let decl = decl.(i + 2) in
    let add_digit =
      let s = ref 0 in
      let e = ref 0 in
      fun j c ->
        if !e > !s && !e != j then begin
          let a = String.sub decl !s (!e - !s) in
          if !s > 0 && decl.[!s - 1] = 'm' then
            let memory =
              match a with
                "8"   -> `m8
              | "16"  -> `m16
              | "32"  -> `m32
              | "64"  -> `m64
              | "128" -> `m128
              | "256" -> `m256
              | _ -> error "invalid memory alignment constraint 'm%s'" a
            in
            alt := { !alt with memory }
          else
            alt := { !alt with copy_to_output = Some (int_of_string a) };
          s := j;
          e := j + 1
        end else begin
          if !e != j then s := j;
          e := j + 1
        end
    in
    let comment = ref false in
    String.iteri (fun j -> function
      (* all characters are sorted by ASCII except when match order is important *)
        ',' ->
          comment := false;
          arg := { !arg with alternatives = Array.append !arg.alternatives [| !alt |] };
          alt := {
            mach_register    = `r;
            copy_to_output   = None;
            commutative      = false;
            disparage        = 1;
            earlyclobber     = false;
            immediate        = false;
            memory           = `no;
            reload_disparage = 4;
            register         = false }
      | _ when !comment -> ()
      | '!' -> alt := { !alt with disparage = !alt.disparage + 100 }
      | '#' -> comment := true
      | '$' -> alt := { !alt with reload_disparage = !alt.reload_disparage + 100 }
      | '%' ->
          if i >= Array.length kinds - 1 then
            error "'%%' constraint used with last operand";
          alt := { !alt with commutative = true }
      | '&' ->
          if not !arg.output then error "input operand constraint contains '&'";
            alt := { !alt with earlyclobber = true }
      | '+' ->
          if j > 0 then
            error "output constraint '+' for operand %d is not at the beginning" i;
          arg := { !arg with input = true; output = true }
      | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as c ->
          add_digit j c
      | '=' ->
        if j > 0 then
          error "output constraint '=' for operand %d is not at the beginning" i;
        arg := { !arg with input = false; output = true }
      | '?' -> alt := { !alt with disparage = !alt.disparage + 1 }
      | 'D' -> alt := { !alt with register = true; mach_register = `D }
      | 'S' -> alt := { !alt with register = true; mach_register = `S }
      | '^' -> alt := { !alt with reload_disparage = !alt.reload_disparage + 1 }
      | 'a' -> alt := { !alt with register = true; mach_register = `a }
      | 'b' -> alt := { !alt with register = true; mach_register = `b }
      | 'c' -> alt := { !alt with register = true; mach_register = `c }
      | 'd' -> alt := { !alt with register = true; mach_register = `d }
      | 'g' -> alt := { !alt with immediate = true; memory = `m16; register = true }
      | 'i' -> alt := { !alt with immediate = true }
      | 'm' -> alt := { !alt with memory = `m16 }
      | 'r' -> alt := { !alt with register = true; mach_register = `r }
      | 'x' -> alt := { !alt with register = true; mach_register = `sse }
      | c -> error "invalid punctuation '%c' in constraint" c) decl;
    add_digit (String.length decl + 1) '0'; (* flushes *)
    arg := { !arg with alternatives = Array.append !arg.alternatives [| !alt |] };
    !arg) kinds in
  let rec check_operand_number a =
    Array.iter (function
        Emit_arg (i, _) ->
          if i >= Array.length args then error "operand number out of range"
      | Emit_dialect ds -> Array.iter check_operand_number ds
      | Emit_string _ | Emit_unique -> ()) a
  in
  check_operand_number template;
  Array.iter (fun arg ->
    if Array.length arg.alternatives != Array.length args.(0).alternatives then
      error "operand constraints for 'asm' differ in number of alternatives";
    Array.iter (fun alt ->
      match alt.copy_to_output with
      | None -> ()
      | Some i ->
          if arg.output then error "matching constraint not valid in output parameter";
        if i >= Array.length args then
          error "matching constraint references invalid operand number";
        if not args.(i).output then
          error "matching constraint references non-output operand"
      ) arg.alternatives) args;
  let ret = args.(nargs - 1) in
  if ret.input && ret.kind != `Unit then
    error "output operand constraint lacks '='";
  let inline_asm = ref { decl; template; args; clobber = [] } in
  for i = nargs + 1 to Array.length decl - 1 do
    try
      let c =
        match decl.(i) with
          "cc"     -> `cc
        | "memory" -> `memory
        | "%rdi"   -> `D
        | "%rsi"   -> `S
        | "%rax"   -> `a
        | "%rbx"   -> `b
        | "%rcx"   -> `c
        | "%rdx"   -> `d
        | "%r8"    -> `r8
        | "%r9"    -> `r9
        | "%r10"   -> `r10
        | "%r11"   -> `r11
        | "%r12"   -> `r12
        | "%r13"   -> `r13
        | "%xmm0"  | "%ymm0"  -> `x0
        | "%xmm1"  | "%ymm1"  -> `x1
        | "%xmm2"  | "%ymm2"  -> `x2
        | "%xmm3"  | "%ymm3"  -> `x3
        | "%xmm4"  | "%ymm4"  -> `x4
        | "%xmm5"  | "%ymm5"  -> `x5
        | "%xmm6"  | "%ymm6"  -> `x6
        | "%xmm7"  | "%ymm7"  -> `x7
        | "%xmm8"  | "%ymm8"  -> `x8
        | "%xmm9"  | "%ymm9"  -> `x9
        | "%xmm10" | "%ymm10" -> `x10
        | "%xmm11" | "%ymm11" -> `x11
        | "%xmm12" | "%ymm12" -> `x12
        | "%xmm13" | "%ymm13" -> `x13
        | "%xmm14" | "%ymm14" -> `x14
        | "%xmm15" | "%ymm15" -> `x15
        | _ -> raise Not_found
      in
      inline_asm := { !inline_asm with clobber = c :: !inline_asm.clobber }
    with Not_found -> ()
  done;
  !inline_asm

let name inline_asm = inline_asm.decl.(1)
let description inline_asm = Array.to_list inline_asm.decl
let bytecode_call inline_asm = inline_asm.decl.(0)
