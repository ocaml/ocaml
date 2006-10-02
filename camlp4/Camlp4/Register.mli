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

module SyntaxPlugin
  (Id : Sig.Id.S) (SyntaxPlugin : functor (Syn : Sig.Syntax.S) -> sig end) :
    sig end;

module SyntaxExtension
  (Id : Sig.Id.S) (SyntaxExtension : Sig.SyntaxExtension.S) : sig end;

module OCamlSyntaxExtension
  (Id : Sig.Id.S)
  (SyntaxExtension : functor (Syntax : Sig.Camlp4Syntax.S) -> Sig.Camlp4Syntax.S)
  : sig end;

(** {6 Registering Parsers} *)

type parser_fun 'a =
  ?directive_handler:('a -> option 'a) -> PreCast.Loc.t -> Stream.t char -> 'a;

value register_str_item_parser : parser_fun PreCast.Ast.str_item -> unit;
value register_sig_item_parser : parser_fun PreCast.Ast.sig_item -> unit;
value register_parser : parser_fun PreCast.Ast.str_item -> parser_fun PreCast.Ast.sig_item -> unit;

module Parser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Ast.S) -> Sig.Parser.S with module Ast = Ast) : sig end;

module OCamlParser
  (Id : Sig.Id.S) (Maker : functor (Ast : Sig.Camlp4Ast.S) -> Sig.Parser.S with module Ast = Ast) : sig end;

module OCamlPreCastParser
  (Id : Sig.Id.S) (Parser : Sig.Parser.S with module Ast = PreCast.Ast) : sig end;

(** {6 Registering Printers} *)

type printer_fun 'a =
  ?input_file:string -> ?output_file:string -> 'a -> unit;

value register_str_item_printer : printer_fun PreCast.Ast.str_item -> unit;
value register_sig_item_printer : printer_fun PreCast.Ast.sig_item -> unit;
value register_printer : printer_fun PreCast.Ast.str_item -> printer_fun PreCast.Ast.sig_item -> unit;

module Printer
  (Id : Sig.Id.S)
  (Maker : functor (Syn : Sig.Syntax.S) -> Sig.Printer.S with module Ast = Syn.Ast) :
    sig end;

module OCamlPrinter
  (Id : Sig.Id.S)
  (Maker : functor (Syn : Sig.Camlp4Syntax.S) -> Sig.Printer.S with module Ast = Syn.Ast) :
    sig end;

module OCamlPreCastPrinter
  (Id : Sig.Id.S) (Printer : Sig.Printer.S with module Ast = PreCast.Ast) :
    sig end;

(** {6 Registering Filters} *)

module AstFilter
  (Id : Sig.Id.S) (Maker : functor (F : Sig.AstFilters.S) -> sig end) : sig end;

value declare_dyn_module : string -> (unit -> unit) -> unit;
value iter_and_take_callbacks : ((string * (unit -> unit)) -> unit) -> unit;

module CurrentParser : Sig.Parser.S with module Ast = PreCast.Ast;
module CurrentPrinter : Sig.Printer.S with module Ast = PreCast.Ast;

value enable_ocaml_printer : unit -> unit;
value enable_ocamlr_printer : unit -> unit;
(* value enable_ocamlrr_printer : unit -> unit; *)
value enable_null_printer : unit -> unit;
value enable_dump_ocaml_ast_printer : unit -> unit;
value enable_dump_camlp4_ast_printer : unit -> unit;

