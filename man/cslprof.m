.TH CSLPROF 1

.SH NAME
cslprof \- The Caml Special Light profiler

.SH SYNOPSIS
.B cslprof
[
.BI \-f \ dump-file
]
[
.BI \-F \ text
]
.I filename ...

.SH DESCRIPTION
The
.B cslprof
command prints execution counts gathered during the execution of a
Caml Special Light program instrumented with
.BR cslcp (1).

It produces a source listing of the program modules given as arguments
where execution counts have been inserted as comments. For instance,
.P
cslprof foo.ml
.P
prints the source code for the foo module, with comments indicating
how many times the functions in this module have been called. Naturally,
this information is accurate only if the source file has not been modified
since the profiling execution took place.

.SH OPTIONS

.TP
.BI \-f \ dumpfile 
Specifies an alternate dump file of profiling information.
The default is the file cslprof.dump in the current directory.
.TP
.BI \-F \ string
Specifies an additional string to be output with profiling information.
By default,
.B cslprof
will annotate progams with comments of the form
.BI (* \ n \ *)
where
.I n
is the counter value for a profiling point. With option
.BI \-F \ string
the annotation will be
.BI (* \ s\ n \ *)

.SH SEE ALSO
.BR cslcp (1).
.br
.I The Caml Special Light user's manual,
chapter "Profiling".
