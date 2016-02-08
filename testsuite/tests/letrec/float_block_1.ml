(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*          Gabriel Scherer, projet Gallium, INRIA Rocquencourt        *)
(*                                                                     *)
(*  Copyright 2012 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Effect are not named to allow different evaluation orders (flambda
   and clambda differ on this point).
 *)
let test =
  let rec x = print_endline "effect"; [| 1; 2; 3 |]
      and y = print_endline "effect"; [| 1.; 2.; 3. |]
  in
  assert (x = [| 1; 2; 3 |]);
  assert (y = [| 1.; 2.; 3. |]);
  ()
