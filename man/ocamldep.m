.TH CSLDEP 1

.SH NAME
ocamldep \- Dependency generator for Objective Caml

.SH SYNOPSIS
.B ocamldep 
[
.BI \-I \ lib-dir
]
.I filename ...

.SH DESCRIPTION

The 
.BR ocamldep (1)
command scans a set of Objective Caml source files
(.ml and .mli files) for references to external compilation units,
and outputs dependency lines in a format suitable for the
.BR make (1)
utility. This ensures that make will compile the source files in the
correct order, and recompile those files that need to when a source
file is modified.

The typical usage is:
.P
ocamldep 
.I options
*.mli *.ml > .depend
.P
where .depend is the file that should contain the
dependencies.

Dependencies are generated both for compiling with the bytecode
compiler 
.BR ocamlc (1)
and with the native-code compiler 
.BR ocamlopt (1).

.SH OPTIONS

The following command-line option is recognized by 
.BR ocamldep (1).

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
.BR ocamldep (1)
the same -I options that are passed to the compiler.

.SH SEE ALSO
.BR ocamlc (1),
.BR ocamlopt (1).
.br
.I The Objective Caml user's manual,
chapter "Dependency generator".
