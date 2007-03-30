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

type camlp4_token = Sig.camlp4_token ==
  [ KEYWORD       of string
  | SYMBOL        of string
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of Sig.quotation
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

module Id         : Sig.Id;
module Loc        : Sig.Loc;
module Ast        : Sig.Camlp4Ast with module Loc = Loc;
module Token      : Sig.Token
                      with module Loc = Loc
                       and type t = camlp4_token;
module Lexer      : Sig.Lexer
                      with module Loc = Loc
                       and module Token = Token;
module Gram       : Sig.Grammar.Static
                      with module Loc = Loc
                       and module Token = Token;
module Quotation  : Sig.Quotation with module Ast = Sig.Camlp4AstToAst Ast;
module DynLoader  : Sig.DynLoader;
module AstFilters : Sig.AstFilters with module Ast = Ast;
module Syntax     : Sig.Camlp4Syntax
                      with module Loc     = Loc
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram
                       and module Quotation = Quotation;

module Printers : sig
  module OCaml         : (Sig.Printer Ast).S;
  module OCamlr        : (Sig.Printer Ast).S;
  module DumpOCamlAst  : (Sig.Printer Ast).S;
  module DumpCamlp4Ast : (Sig.Printer Ast).S;
  module Null          : (Sig.Printer Ast).S;
end;

module MakeGram (Lexer : Sig.Lexer with module Loc = Loc)
  : Sig.Grammar.Static with module Loc = Loc and module Token = Lexer.Token;

module MakeSyntax (U : sig end) : Sig.Syntax;
