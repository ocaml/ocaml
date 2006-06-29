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
module Plugin
  (Id : Sig.Id.S) (Plugin : functor (Unit : sig end) -> sig end) : sig end;

module SyntaxExtension
  (Id : Sig.Id.S) (SyntaxExtension : Sig.SyntaxExtension.S) : sig end;

module OCamlSyntaxExtension
  (Id : Sig.Id.S)
  (SyntaxExtension : functor (Syntax : Sig.Camlp4Syntax.S) -> Sig.Camlp4Syntax.S)
  : sig end;

module Parser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Ast.S) -> Sig.Parser.S with module Ast = Ast) : sig end;

module OCamlParser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Camlp4Ast.S) -> Sig.Parser.S with module Ast = Ast) : sig end;

module SyntaxPlugin
  (Id : Sig.Id.S) (SyntaxPlugin : functor (Syn : Sig.Syntax.S) -> sig end) :
    sig end;

module Printer
  (Id : Sig.Id.S)
  (Maker : functor (Syn : Sig.Syntax.S) -> Sig.Printer.S with module Ast = Syn.Ast) :
    sig end;

module OCamlPrinter
  (Id : Sig.Id.S)
  (Maker : functor (Syn : Sig.Camlp4Syntax.S) -> Sig.Printer.S with module Ast = Syn.Ast) :
    sig end;

module AstFilter
  (Id : Sig.Id.S) (Maker : functor (F : Sig.AstFilters.S) -> sig end) : sig end;

value declare_dyn_module : string -> (unit -> unit) -> unit;
value iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit;

module CurrentParser : Sig.Parser.S with module Ast = PreCast.Ast;
module CurrentPrinter : Sig.Printer.S with module Ast = PreCast.Ast;
