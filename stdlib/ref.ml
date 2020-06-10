(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2020 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type 'a t = 'a ref = {mutable contents : 'a}

external ref : 'a -> 'a t = "%makemutable"

external get : 'a t -> 'a = "%field0"

external set : 'a t -> 'a -> unit = "%setfield0"

let with_ref r x f =
  let oldx = !r in
  r := x;
  Fun.protect ~finally:(fun () -> r := oldx) f

external equal : 'a t -> 'a t -> bool = "%eq"

external incr : int t -> unit = "%incr"

external decr : int t -> unit = "%decr"

let add_to_list r x = set r (x :: get r)
