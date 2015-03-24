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

type register =
    R of int
  | D of int

type arch_specific =
    Clobber of register
  | Length of int

let parse_register_constraint _ = None

let parse_arch_specific s =
  try
    let i = int_of_string (String.sub s 1 (String.length s - 1)) in
    match s.[0] with
      'd' when (i >= 0 && i <= 7) || i = 12 -> Some (Clobber (D i))
    | 'l' -> Some (Length i)
    | 'r' when i >= 0 && i <= 31 -> Some (Clobber (R i))
    | _ -> None
  with _ -> None
