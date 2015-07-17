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
    F of int
  | FP
  | R of int

type arch_specific =
    Clobber of register
  | Length of int

let parse_register_constraint = function
    'd' -> Some FP
  |  _ -> None

let parse_arch_specific s =
  try
    let i = int_of_string (String.sub s 1 (String.length s - 1)) in
    match s.[0] with
      'f' when i >= 1 && i <= 31 -> Some (Clobber (F i))
    | 'l' -> Some (Length i)
    | 'r' when (i >= 3 && i <= 10) || (i >= 14 && i <= 28) -> Some (Clobber (R i))
    | _ -> None
  with _ -> None
