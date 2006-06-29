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

(** The AntiquotSyntax signature describe the minimal interface needed
    for antiquotation handling. *)
module type S = sig
  module Ast          : Ast.S;

  (** The parse function for expressions.
      The underlying expression grammar entry is generally "expr; EOI". *)
  value parse_expr    : Ast.Loc.t -> string -> Ast.expr;

  (** The parse function for patterns.
      The underlying pattern grammar entry is generally "patt; EOI". *)
  value parse_patt    : Ast.Loc.t -> string -> Ast.patt;
end;

