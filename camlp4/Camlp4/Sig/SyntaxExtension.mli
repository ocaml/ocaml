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

module type S = functor (Syn : Syntax.S)
                  -> (Syntax.S with module Loc            = Syn.Loc
                                and module Warning        = Syn.Warning
                                and module Ast            = Syn.Ast
                                and module Token          = Syn.Token
                                and module Gram           = Syn.Gram
                                and module AntiquotSyntax = Syn.AntiquotSyntax
                                and module Quotation      = Syn.Quotation);
