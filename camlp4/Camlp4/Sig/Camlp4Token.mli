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

(** This signature describes tokens for the Objective Caml and the Revised
    syntax lexing rules. For some tokens the data constructor holds two
    representations with the evaluated one and the source one. For example
    the INT data constructor holds an integer and a string, this string can
    contains more information that's needed for a good pretty-printing
    ("42", "4_2", "0000042", "0b0101010"...).

    The meaning of the tokens are:
-      * [KEYWORD s] is the keyword [s].
-      * [LIDENT s] is the ident [s] starting with a lowercase letter.
-      * [UIDENT s] is the ident [s] starting with an uppercase letter.
-      * [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
       is the integer constant [i] whose string source is [s].
-      * [FLOAT f s] is the float constant [f] whose string source is [s].
-      * [STRING s s'] is the string constant [s] whose string source is [s'].
-      * [CHAR c s] is the character constant [c] whose string source is [s].
-      * [QUOTATION q] is a quotation [q], see {!Quotation.t} for more information.
-      * [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
-      * [EOI] is the end of input.

     Warning: the second string associated with the constructor [STRING] is
     the string found in the source without any interpretation. In particular,
     the backslashes are not interpreted. For example, if the input is ["\n"]
     the string is *not* a string with one element containing the character
     "return", but a string of two elements: the backslash and the character
     ["n"]. To interpret a string use the first string of the [STRING]
     constructor (or if you need to compute it use the module
     {!Camlp4.Struct.Token.Eval}. Same thing for the constructor [CHAR]. *)

type t =
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
  | QUOTATION     of Quotation.t
  | ANTIQUOT      of string and string
  | COMMENT       of string
  | BLANKS        of string
  | NEWLINE
  | LINE_DIRECTIVE of int and option string
  | EOI ];

module type S = Token.S with type t = t;
