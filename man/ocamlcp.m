.TH OCAMLCP 1

.SH NAME
ocamlcp \- The Objective Caml profiling compiler

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
script is a front-end to
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
.BR \-p \ letters
The letters following
.B -p
indicate which parts of the program should be profiled:

.TP
.B a
all options
.TP
.B f
function calls : a count point is set at the beginning of function bodies
.TP
.B i
if... then... else: count points are set in
both "then" branch and "else" branch
.TP
.B l
while, for loops: a count point is set at the beginning of
the loop body
.TP
.B m
"match" branches: a count point is set at the beginning of the
body of each branch
.TP
.B t
try...with branches: a count point is set at the
beginning of the body of each branch

For instance, compiling with 
.B ocamlcp \-pfilm
profiles function calls, if... then... else..., loops, and pattern
matching.

Calling 
.BR ocamlcp (1)
without the
.B \-p
option defaults to
.B \-p fm
meaning
that only function calls and pattern matching are profiled.

.SH SEE ALSO
.BR ocamlc (1),
.BR ocamlprof (1).
.br
.I The Objective Caml user's manual,
chapter "Profiling".
