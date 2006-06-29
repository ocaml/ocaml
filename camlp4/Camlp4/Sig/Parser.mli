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

(** {6 Parser} *)
module type S = sig

  module Ast : Ast.S;
  open Ast;

  (** Called when parsing an implementation (ml file) to build the syntax
      tree; the returned list contains the phrases (structure items) as a
      single "declare" node (a list of structure items);   if  the parser
      encounter a directive it stops (since the directive may change  the
      syntax), the given [directive_handler] function  evaluates  it  and
      the parsing starts again. *)
  value parse_implem : ?directive_handler:(str_item -> option str_item) ->
                       Loc.t -> Stream.t char -> Ast.str_item;

  (** Same as {!parse_implem} but for interface (mli file). *)
  value parse_interf : ?directive_handler:(sig_item -> option sig_item) ->
                       Loc.t -> Stream.t char -> Ast.sig_item;

end;
