(*************************************************************************)
(*                                                                       *)
(*                Objective Caml LablTk library                          *)
(*                                                                       *)
(*            Jacques Garrigue, Kyoto University RIMS                    *)
(*                                                                       *)
(*   Copyright 1999 Institut National de Recherche en Informatique et    *)
(*   en Automatique and Kyoto University.  All rights reserved.          *)
(*   This file is distributed under the terms of the GNU Library         *)
(*   General Public License, with the special exception on linking       *)
(*   described in file ../../../LICENSE.                                 *)
(*                                                                       *)
(*************************************************************************)

(* $Id$ *)

type ('a, 'b) assoc_list =
    Nil
  | Cons of 'a * 'b * ('a, 'b) assoc_list

let rec assq key = function
    Nil -> raise Not_found
  | Cons (a, b, l) ->
      if key == a then b else assq key l

let fast ~f =
  let memo = ref Nil in
  fun key ->
    try assq key !memo
    with Not_found ->
      let data = f key in
      memo := Cons(key, data, !memo);
      data
