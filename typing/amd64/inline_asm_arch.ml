(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                    Vladimir Brankov, Jane Street                    *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

type register =
    R  | SSE
  | BP | DI | SI | A   | B   | C   | D
  | R8 | R9 | R10 | R11 | R12 | R13
  | X0 | X1 | X2  | X3  | X4  | X5  | X6  | X7
  | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15

type arch_specific = register

let parse_register_constraint = function
    'D' -> Some DI
  | 'S' -> Some SI
  | 'a' -> Some A
  | 'b' -> Some B
  | 'c' -> Some C
  | 'd' -> Some D
  | 'r' -> Some R
  | 'x' -> Some SSE
  | _ -> None

let parse_arch_specific = function
    "%rbp"   -> Some BP
  | "%rdi"   -> Some DI
  | "%rsi"   -> Some SI
  | "%rax"   -> Some A
  | "%rbx"   -> Some B
  | "%rcx"   -> Some C
  | "%rdx"   -> Some D
  | "%r8"    -> Some R8
  | "%r9"    -> Some R9
  | "%r10"   -> Some R10
  | "%r11"   -> Some R11
  | "%r12"   -> Some R12
  | "%r13"   -> Some R13
  | "%xmm0"  | "%ymm0"  -> Some X0
  | "%xmm1"  | "%ymm1"  -> Some X1
  | "%xmm2"  | "%ymm2"  -> Some X2
  | "%xmm3"  | "%ymm3"  -> Some X3
  | "%xmm4"  | "%ymm4"  -> Some X4
  | "%xmm5"  | "%ymm5"  -> Some X5
  | "%xmm6"  | "%ymm6"  -> Some X6
  | "%xmm7"  | "%ymm7"  -> Some X7
  | "%xmm8"  | "%ymm8"  -> Some X8
  | "%xmm9"  | "%ymm9"  -> Some X9
  | "%xmm10" | "%ymm10" -> Some X10
  | "%xmm11" | "%ymm11" -> Some X11
  | "%xmm12" | "%ymm12" -> Some X12
  | "%xmm13" | "%ymm13" -> Some X13
  | "%xmm14" | "%ymm14" -> Some X14
  | "%xmm15" | "%ymm15" -> Some X15
  | _ -> None
