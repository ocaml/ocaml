/***********************************************************************/
/*                                                                     */
/*                           Objective Caml                            */
/*                                                                     */
/*             Damien Doligez, projet Para, INRIA Rocquencourt         */
/*                                                                     */
/*  Copyright 1996 Institut National de Recherche en Informatique et   */
/*  en Automatique.  All rights reserved.  This file is distributed    */
/*  under the terms of the GNU Library General Public License, with    */
/*  the special exception on linking described in file ../LICENSE.     */
/*                                                                     */
/***********************************************************************/

/* $Id$ */

type 'Odds' {
  longint;                  /* resource ID of corresponding 'Sods' */
  longint = $$CountOf (suffixes);
  wide array suffixes { pstring; };
};

type 'Sods' {
  longint = 0xA5666D66;
  pstring;                  /* default token breaks */
  longint = $$CountOf (keywords);
  wide array keywords {
    pstring;                /* the keyword itself */
    longint                 /* the keyword type */
      case=0,
      caseWord=1,
      noCase=2,
      noCaseWord=3,
      line=4,
      mystery=0xB,
      opening=0x10000,
      closing=0x20000,
      openingFill=0x50000,
      closingFill=0x60000,
      quote=0x80000;
    literal longint;        /* scoping parameter */
    longint                 /* color */
      red=0,
      blue=1,
      bluegreen=2,
      bluepurple=3,
      grey=4,
      darkgreen=7,
      black=255;
  };
};

resource 'Odds' (26087, "O'Caml") {
  26087,
  { ".ml", ".mli", ".mll", ".mly", ".mlp" }
};

resource 'Sods' (26087, "O'Caml Keywords") {
  "\t\r\n ~!$%^&*()/-+=<>,[]{};",
  { 
    "(*", openingFill, '(**)', red,
    "*)", closingFill, '(**)', red,
    "\"", quote, 0, grey,
#define KEY caseWord, 0, blue
    "and", KEY,
    "as", KEY,
    "assert", KEY,
    "begin", KEY,
    "class", KEY,
    "constraint", KEY,
    "do", KEY,
    "done", KEY,
    "downto", KEY,
    "else", KEY,
    "end", KEY,
    "exception", KEY,
    "external", KEY,
    "false", KEY,
    "for", KEY,
    "fun", KEY,
    "function", KEY,
    "functor", KEY,
    "if", KEY,
    "in", KEY,
    "include", KEY,
    "inherit", KEY,
    "initializer", KEY,
    "lazy", KEY,
    "let", KEY,
    "match", KEY,
    "method", KEY,
    "module", KEY,
    "mutable", KEY,
    "new", KEY,
    "object", KEY,
    "of", KEY,
    "open", KEY,
    "or", KEY,
    "parser", KEY,
    "private", KEY,
    "rec", KEY,
    "sig", KEY,
    "struct", KEY,
    "then", KEY,
    "to", KEY,
    "true", KEY,
    "try", KEY,
    "type", KEY,
    "val", KEY,
    "virtual", KEY,
    "when", KEY,
    "while", KEY,
    "with", KEY,
    "mod", KEY,
    "land", KEY,
    "lor", KEY,
    "lxor", KEY,
    "lsl", KEY,
    "lsr", KEY,
    "asr", KEY,
  }
};
