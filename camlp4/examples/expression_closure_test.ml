(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2007  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* x and y are free *)
close_expr(x y);;

(* bind x *)
let x a = a + 42;;

(* y is free *)
close_expr(x y);;

(* bind y locally so the expr is closed *)
close_expr(let y = x 2 in x y);;

(* bind y locally but outside, z is free *)
let y = x 2 in close_expr(x (z y));;
