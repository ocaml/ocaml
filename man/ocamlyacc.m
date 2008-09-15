\" $Id$
.TH OCAMLYACC 1

.SH NAME
ocamlyacc \- The Objective Caml parser generator

.SH SYNOPSIS
.B ocamlyacc
[
.B -v
]
[
.BI \-b prefix
]
.I filename.mly

.SH DESCRIPTION

The
.BR ocamlyacc (1)
command produces a parser from a LALR(1) context-free grammar
specification with attached semantic actions, in the style of
.BR yacc (1).
Assuming the input file is
.IR grammar \&.mly,
running
.B ocamlyacc
produces Caml code for a parser in the file
.IR grammar \&.ml,
and its interface in file
.IR grammar \&.mli.

The generated module defines one parsing function per entry point in
the grammar. These functions have the same names as the entry points.
Parsing functions take as arguments a lexical analyzer (a function
from lexer buffers to tokens) and a lexer buffer, and return the
semantic attribute of the corresponding entry point. Lexical analyzer
functions are usually generated from a lexer specification by the
.BR ocamllex (1)
program. Lexer buffers are an abstract data type
implemented in the standard library module Lexing. Tokens are values from
the concrete type token, defined in the interface file
.IR grammar \&.mli
produced by
.BR ocamlyacc (1).

.SH OPTIONS

The
.BR ocamlyacc (1)
command recognizes the following options:
.TP
.BI \-b prefix
Name the output files
.IR prefix \&.ml,
.IR prefix \&.mli,
.IR prefix \&.output,
instead of the default naming convention.
.TP
.B \-q
This option has no effect.
.TP
.B \-v
Generate a description of the parsing tables and a report on conflicts
resulting from ambiguities in the grammar. The description is put in
file
.IR grammar .output.
.TP
.B \-version
Print version and exit.
.TP
.B \-
Read the grammar specification from standard input.  The default
output file names are stdin.ml and stdin.mli.
.TP
.BI \-\- \ file
Process
.I file
as the grammar specification, even if its name
starts with a dash (-) character.  This option must be the last on the
command line.

.SH SEE ALSO
.BR ocamllex (1).
.br
.IR "The Objective Caml user's manual" ,
chapter "Lexer and parser generators".
