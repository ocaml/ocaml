(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*           Gabriel Scherer, projet Gallium, INRIA Rocquencourt          *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* a simple test with mutually recursive functions *)
let test =
  let rec even = function
    | 0 -> true
    | n -> odd (n - 1)
  and odd = function
    | 0 -> false
    | n -> even (n - 1)
  in
  List.iter (fun i -> assert (even i <> odd i && even i = (i mod 2 = 0)))
    [0;1;2;3;4;5;6]
