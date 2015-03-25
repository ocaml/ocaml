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
    F of int
  | G of int
  | I of int
  | L of int
  | O of int
  | FP

type arch_specific = register

let parse_register_constraint = function
    'f' -> Some FP
  |  _ -> None

let parse_arch_specific s =
  try
    let i = int_of_string (String.sub s 1 (String.length s - 1)) in
    match s.[0] with
      'f' when i land 2 = 0 -> Some (F i)
    | 'g' when i >= 3 && i <= 4 -> Some (G i)
    | 'i' when i >= 0 && i <= 5 -> Some (I i)
    | 'l' when i >= 0 && i <= 4 -> Some (L i)
    | 'o' when i >= 0 && i <= 5 -> Some (O i)
    | _ -> None
  with _ -> None
