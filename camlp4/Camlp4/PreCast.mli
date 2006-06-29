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

type camlp4_token = Sig.Camlp4Token.t ==
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
  | QUOTATION     of Sig.Quotation.t
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

module Id         : Sig.Id.S;
module Loc        : Sig.Loc.S;
module Warning    : Sig.Warning.S with module Loc = Loc;
module Ast        : Sig.Camlp4Ast.S with module Loc = Loc;
module Token      : Sig.Token.S
                      with module Loc = Loc
                       and type t = camlp4_token;
module Lexer      : Sig.Lexer.S
                      with module Loc = Loc
                       and module Token = Token;
module Gram       : Sig.Grammar.Static.S
                      with module Loc = Loc
                       and module Token = Token;
module Quotation  : Sig.Quotation.S with module Ast = Sig.Camlp4Ast.ToAst Ast;
module DynLoader  : Sig.DynLoader.S;
module AstFilters : Sig.AstFilters.S with module Ast = Ast;
module Syntax     : Sig.Camlp4Syntax.S
                      with module Loc     = Loc
                       and module Warning = Warning
                       and module Token   = Token
                       and module Ast     = Ast
                       and module Gram    = Gram;
module MakeGram (Lexer : Sig.Lexer.S with module Loc = Loc)
  : Sig.Grammar.Static.S with module Loc = Loc and module Token = Lexer.Token;
