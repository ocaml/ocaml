(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Functions and operators on functional values.

    @since 4.08 *)

module Ops : sig
  val ( %< ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
  (** Function composition: [f %< g] is equivalent to [fun x -> f (g x)].
      Left-associative at precedence level 7/11.
      @since 4.08
  *)


  val ( %> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
  (** Function reverse-composition: [f %> g] is equivalent to
      [fun x -> g (f x)].
      Left-associative at precedence level 7/11.
      @since 4.08
  *)
end
