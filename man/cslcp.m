.TH CSLCP 1

.SH NAME
cslcp \- The Caml Special Light profiling compiler

.SH SYNOPSIS
.B cslcp
[
.I cslc options
]
[
.BI \-p \ flags
]
.I filename ...

.SH DESCRIPTION
The
.B cslcp
script is a front-end to
.BR cslc (1)
that instruments the source code, adding code to record how many times
functions are called, branches of conditionals are taken, ...
Execution of instrumented code produces an execution profile in the
file cslprof.dump, which can be read using
.BR cslprof (1).

.B cslcp
accepts the same arguments and options as
.BR cslc (1).

.SH OPTIONS

In addition to the
.BR cslc (1)
options,
.B cslcp
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
.B cslcp \-pfilm
profiles function calls, if... then... else..., loops, and pattern
matching.

Calling 
.BR cslcp (1)
without the
.B \-p
option defaults to
.B \-p fm
meaning
that only function calls and pattern matching are profiled.

.SH SEE ALSO
.BR cslc (1),
.BR cslprof (1).
.br
.I The Caml Special Light user's manual,
chapter "Profiling".
