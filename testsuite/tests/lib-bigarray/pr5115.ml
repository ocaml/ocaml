(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* PR#5115 - multiple evaluation of bigarray expr *)

open Bigarray

let f y0 =
  Printf.printf "***EXEC***\n%!";
  y0

let _ =
  let y = Array1.of_array float64 fortran_layout [| 1. |] in
  (f y).{1};
  (f y).{1} <- 3.14
