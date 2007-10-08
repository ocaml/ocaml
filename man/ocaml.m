.TH OCAML 1

.SH NAME
ocaml \- The Objective Caml interactive toplevel


.SH SYNOPSIS
.B ocaml
[
.B \-unsafe
]
[
.BI \-I \ lib-dir
]
[
.I object-files
]
[
.I script-file
]
.SH DESCRIPTION

The
.BR ocaml (1)
command is the toplevel system for Objective Caml,
that permits interactive use of the Objective Caml system through a
read-eval-print loop. In this mode, the system repeatedly reads Caml
phrases from the input, then typechecks, compiles and evaluates
them, then prints the inferred type and result value, if any. The
system prints a # (sharp) prompt before reading each phrase.

A toplevel phrase can span several lines. It is terminated by ;; (a
double-semicolon). The syntax of toplevel phrases is as follows.

The toplevel system is started by the command 
.BR ocaml (1).
Phrases are read on standard input, results are printed on standard
output, errors on standard error. End-of-file on standard input
terminates
.BR ocaml (1).

If one or more
.I object-files
(ending in
.B .cmo
or
.B .cma
 ) are given, they are loaded silently before starting the toplevel.

If a
.I script-file
is given, phrases are read silently from the file, errors printed on
standard error.
.BR ocaml (1)
exits after the execution of the last phrase.

.SH OPTIONS

The following command-line options are recognized by
.BR ocaml (1).

.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
source and compiled files. By default, the current directory is
searched first, then the standard library directory. Directories added
with 
.B \-I
are searched after the current directory, in the order in which they
were given on the command line, but before the standard library
directory.

.TP
.B \-unsafe
Turn bound checking off on array and string accesses (the v.(i)
and s.[i] constructs). Programs compiled with 
.B \-unsafe
are therefore slightly faster, but unsafe: anything can happen if the program
accesses an array or string outside of its bounds.

.SH ENVIRONMENT VARIABLES

.TP
.B LC_CTYPE
If set to iso_8859_1, accented characters (from the
ISO Latin-1 character set) in string and character literals are
printed as is; otherwise, they are printed as decimal escape sequences.

.TP
.B TERM
When printing error messages, the toplevel system
attempts to underline visually the location of the error. It
consults the TERM variable to determines the type of output terminal
and look up its capabilities in the terminal database.

.SH SEE ALSO
.BR ocamlc (1).
.br
.I The Objective Caml user's manual,
chapter "The toplevel system".

