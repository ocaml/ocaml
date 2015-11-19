(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Xavier Leroy, projet Gallium, INRIA Paris-Rocquencourt     *)
(*                                                                     *)
(*  Copyright 2015 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

let _ =
  let a = [| 0.0; -. 0.0 |] in
  Printf.printf "%Lx %Lx\n"
                (Int64.bits_of_float a.(0)) (Int64.bits_of_float a.(1))

