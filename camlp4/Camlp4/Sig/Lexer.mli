(****************************************************************************)
(*                                                                          *)
(*                              Objective Caml                              *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the Objective   *)
(*  Caml source tree.                                                       *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Nicolas Pouillard: initial version
 *)

module type S = sig
  module Loc : Loc.S;
  module Token : Token.S with module Loc = Loc;
  module Error : Error.S;

  (** The constructor for a lexing function. The character stream is the input
      stream to be lexed. The result is a stream of pairs of a token and
      a location.
      The lexer do not use global (mutable) variables: instantiations
      of [Lexer.mk ()] do not perturb each other. *)
  value mk : unit -> (Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t));
end;
