(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      KC Sivaramakrishnan, Indian Institute of Technology, Madras       *)
(*                 Stephen Dolan, University of Cambridge                 *)
(*                   Tom Kelly, OCaml Labs Consultancy                    *)
(*                                                                        *)
(*   Copyright 2019 Indian Institute of Technology, Madras                *)
(*   Copyright 2014 University of Cambridge                               *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type tls_state = Obj.t array

val unique_value : Obj.t

external get_tls_state : unit -> tls_state = "%tls_get"

external set_tls_state : tls_state -> unit =
  "caml_tls_set" [@@noalloc]

type 'a key = int * (unit -> 'a)

val new_key : ?split_from_parent:('a -> 'a) -> (unit -> 'a) -> 'a key

val set : 'a key -> 'a -> unit

val get : 'a key -> 'a

val get_initial_keys : unit -> (int * Obj.t) list

val set_initial_keys : (int * Obj.t) list -> unit
