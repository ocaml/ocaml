(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*               Jacques Garrigue, Nagoya University                      *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* PR6216: wrong inlining of GADT match *)

type _ t =
 | Float : float t
 | String : string t

let f : type a . a t -> a -> unit = fun t a ->
 match t with
 | Float -> ()
 | String -> ignore (String.length a : int)

let _g (kind : float t) (x : float) : unit = f kind (x *. 13.)
