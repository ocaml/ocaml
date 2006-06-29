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

(** A syntax module is a sort of constistent bunch of modules and values.
   In such a module you have a parser, a printer, and also modules for
   locations, syntax trees, tokens, grammars, quotations, anti-quotations.
   There is also the main grammar entries. *)
module type S = sig
  module Loc            : Loc.S;
  module Warning        : Warning.S with module Loc = Loc;
  module Ast            : Ast.S with module Loc = Loc;
  module Token          : Token.S with module Loc = Loc;
  module Gram           : Grammar.Static.S with module Loc = Loc and module Token = Token;
  module AntiquotSyntax : AntiquotSyntax.S with module Ast = Ast;
                          (* Gram is not constrained here for flexibility *)
  module Quotation      : Quotation.S with module Ast = Ast;
  module Parser         : Parser.S with module Ast = Ast;
  module Printer        : Printer.S with module Ast = Ast;
end;
