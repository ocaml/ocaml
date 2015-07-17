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

type register = R | DI | SI | A | B | C | D | BP | TOS

type arch_specific = register

let parse_register_constraint = function
    'D' -> Some DI
  | 'S' -> Some SI
  | 'a' -> Some A
  | 'b' -> Some B
  | 'c' -> Some C
  | 'd' -> Some D
  | 'r' -> Some R
  | 't' -> Some TOS
  | _ -> None

let parse_arch_specific = function
    "%rdi"   -> Some DI
  | "%rsi"   -> Some SI
  | "%rax"   -> Some A
  | "%rbx"   -> Some B
  | "%rcx"   -> Some C
  | "%rdx"   -> Some D
  | "%rbp"   -> Some BP
  | "%tos"   -> Some TOS
  | _ -> None
