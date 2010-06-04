.TH OCAMLPROF 1

.SH NAME
ocamlprof \- The Objective Caml profiler

.SH SYNOPSIS
.B ocamlprof
[
.I options
]
.I filename ...

.SH DESCRIPTION
The
.B ocamlprof
command prints execution counts gathered during the execution of a
Objective Caml program instrumented with
.BR ocamlcp (1).

It produces a source listing of the program modules given as arguments
where execution counts have been inserted as comments. For instance,
.P
ocamlprof foo.ml
.P
prints the source code for the foo module, with comments indicating
how many times the functions in this module have been called. Naturally,
this information is accurate only if the source file has not been modified
since the profiling execution took place.

.SH OPTIONS

.TP
.BI \-f \ dumpfile 
Specifies an alternate dump file of profiling information.
The default is the file ocamlprof.dump in the current directory.
.TP
.BI \-F \ string
Specifies an additional string to be output with profiling information.
By default,
.B ocamlprof
will annotate programs with comments of the form
.BI (* \ n \ *)
where
.I n
is the counter value for a profiling point. With option
.BI \-F \ string
the annotation will be
<<<<<<< .courant
.BI (* \ s\ n \ *)
=======
.BI (* \ sn \ *)
.TP
.BI \-impl \ filename
Compile the file
.I filename
as an implementation file, even if its extension is not .ml.
.TP
.BI \-intf \ filename
Compile the file
.I filename
as an interface file, even if its extension is not .mli.
.TP
.B \-version
Print version string and exit.
.TP
.B \-vnum
Print short version number and exit.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.
>>>>>>> .fusion-droit.r10497

.SH SEE ALSO
.BR ocamlcp (1).
.br
.I The Objective Caml user's manual,
chapter "Profiling".
