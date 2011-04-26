\" $Id$

.TH OCAMLCP 1

.SH NAME
ocamlcp \- The OCaml profiling compiler

.SH SYNOPSIS
.B ocamlcp
[
.I ocamlc options
]
[
.BI \-p \ flags
]
.I filename ...

.SH DESCRIPTION
The
.B ocamlcp
command is a front-end to
.BR ocamlc (1)
that instruments the source code, adding code to record how many times
functions are called, branches of conditionals are taken, ...
Execution of instrumented code produces an execution profile in the
file ocamlprof.dump, which can be read using
.BR ocamlprof (1).

.B ocamlcp
accepts the same arguments and options as
.BR ocamlc (1).

.SH OPTIONS

In addition to the
.BR ocamlc (1)
options,
.B ocamlcp
accepts the following option controlling the amount of profiling
information:
.TP
.BI \-p \ letters
The
.I letters
indicate which parts of the program should be profiled:
.TP
.B a
all options
.TP
.B f
function calls : a count point is set at the beginning of each function body
.TP
.B i
.BR if \ ... \ then \ ... \ else :
count points are set in both
.BR then \ and \ else
branches
.TP
.B l
\BR while , \ for
loops: a count point is set at the beginning of the loop body
.TP
.B m
.B match
branches: a count point is set at the beginning of the
body of each branch of a pattern-matching
.TP
.B t
.BR try \ ... \ with
branches: a count point is set at the beginning of the body of each
branch of an exception catcher

.PP
For instance, compiling with
.B ocamlcp\ \-pfilm
profiles function calls,
.BR if \ ... \ then \ ... \ else \ ...,
loops, and pattern matching.

Calling
.BR ocamlcp (1)
without the
.B \-p
option defaults to
.B \-p\ fm
meaning that only function calls and pattern matching are profiled.

Note: due to the implementation of streams and stream patterns as
syntactic sugar, it is hard to predict what parts of stream expressions
and patterns will be profiled by a given flag.  To profile a program with
streams, we recommend using
.BR ocamlcp\ \-p\ a .

.SH SEE ALSO
.BR ocamlc (1),
.BR ocamlprof (1).
.br
.IR "The OCaml user's manual" ,
chapter "Profiling".
