.TH CSLDEP 1

.SH NAME
csldep \- Dependency generator for Caml Special Light

.SH SYNOPSIS
.B csldep 
[
.BI \-I \ lib-dir
]
.I filename ...

.SH DESCRIPTION

The 
.BR csldep (1)
command scans a set of Caml Special Light source files
(.ml and .mli files) for references to external compilation units,
and outputs dependency lines in a format suitable for the
.BR make (1)
utility. This ensures that make will compile the source files in the
correct order, and recompile those files that need to when a source
file is modified.

The typical usage is:
.P
csldep 
.I options
*.mli *.ml > .depend
.P
where .depend is the file that should contain the
dependencies.

Dependencies are generated both for compiling with the bytecode
compiler 
.BR cslc (1)
and with the native-code compiler 
.BR cslopt (1).

.SH OPTIONS

The following command-line option is recognized by 
.BR csldep (1).

.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
source files. If a source file foo.ml mentions an external
compilation unit Bar, a dependency on that unit's interface
bar.cmi is generated only if the source for bar is found in the
current directory or in one of the directories specified with 
.BR -I .
Otherwise, Bar is assumed to be a module form the standard library,
and no dependencies are generated. For programs that span multiple
directories, it is recommended to pass 
.BR csldep (1)
the same -I options that are passed to the compiler.

.SH SEE ALSO
.BR cslc (1),
.BR cslopt (1).
.br
.I The Caml Special Light user's manual,
chapter "Dependency generator".
