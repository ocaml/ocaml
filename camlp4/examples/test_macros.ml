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

(* DEFINE F(x, y, z) = x + y * z;; *)
(* F(F(1, 2, 3), 4, 5);; *)

(* !+ (1, 2, 3, 4);; *)

(* foldl(( + ), 1, 2, 3, 4);; *)
(* foldr(cons, 1, 2, 3, []);; *)

let cons x xs = x :: xs;;

def_foldl ( !+ ) ( + );;
def_foldr ( !:: ) cons;;

!+ (1, 2, 3, 4);;
!:: (1, 2, 3, []);;
