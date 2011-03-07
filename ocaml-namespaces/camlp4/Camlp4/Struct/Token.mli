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
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)

module Make (Loc : Sig.Loc) : Sig.Camlp4Token with module Loc = Loc;

module Eval : sig
  value char : string -> char;
      (** Convert a char token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if an
          incorrect backslash sequence is found; [Token.Eval.char (Char.escaped c)]
          returns [c] *)

  value string : ?strict:unit -> string -> string;
      (** [Taken.Eval.string strict s]
          Convert a string token, where the escape sequences (backslashes)
          remain to be interpreted; raise [Failure] if [strict] and an
          incorrect backslash sequence is found;
          [Token.Eval.string strict (String.escaped s)] returns [s] *)
end;
