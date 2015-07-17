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
    D of int
  | FP
  | X of int

type arch_specific = register

let parse_register_constraint = function
    'w' -> Some FP
  |  _ -> None

let parse_arch_specific s =
  try
    let i = int_of_string (String.sub s 1 (String.length s - 1)) in
    match s.[0] with
      'd' when i >= 0 && i <= 31 -> Some (D i)
    | 'x' when i >= 0 && i <= 28 && i <> 18 -> Some (X i)
    | _ -> None
  with _ -> None
