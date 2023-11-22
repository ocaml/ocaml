(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = Head of 'a * 'a list

external unsafe_of_list : 'a list -> 'a t = "%identity"

let of_list = function
    [] -> None
  | l -> Some (unsafe_of_list l)

external to_list : 'a t -> 'a list = "%identity"

let length (Head (_, t)) = Int.succ (List.length t)

let singleton x = Head (x, [])
let cons x ne = Head (x, to_list ne)

let hd (Head (h, _)) = h
let tl (Head (_, t)) = t

let map f (Head (h, t)) = Head (f h, List.map f t)
let iter f (Head (h, t)) = f h; List.iter f t

let append ne1 ne2 = unsafe_of_list (to_list ne1 @ to_list ne2)
let append_list_right ne l = unsafe_of_list (to_list ne @ l)
let append_list_left l ne = unsafe_of_list (l @ to_list ne)

let to_seq (Head (h, t)) () =
  Seq.Cons (h, List.to_seq t)

let of_seq seq =
  match seq () with
  | Seq.Nil -> None
  | Seq.Cons (x, seq) -> Some (Head (x, List.of_seq seq))
