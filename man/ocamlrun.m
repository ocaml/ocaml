.TH OCAMLRUN 1

.SH NAME
ocamlrun \- The Objective Caml bytecode interpreter

.SH SYNOPSIS
.B ocamlrun
[
.B \-v
]
.I filename argument ...

.SH DESCRIPTION
The 
.BR ocamlrun (1)
command executes bytecode files produced by the
linking phase of the 
.BR ocamlc (1)
command.

The first non-option argument is taken to be the name of the file
containing the executable bytecode. (That file is searched in the
executable path as well as in the current directory.) The remaining
arguments are passed to the Caml Light program, in the string array
Sys.argv. Element 0 of this array is the name of the
bytecode executable file; elements 1 to 
.I n
are the remaining arguments.

In most cases, the bytecode
executable files produced by the 
.BR ocamlc (1)
command are self-executable,
and manage to launch the 
.BR ocamlrun (1)
command on themselves automatically.

.SH OPTIONS

The following command-line option is recognized by 
.BR ocamlrun (1).

.TP
.B \-v 
When set, the memory manager prints verbose messages on standard error
to signal garbage collections and heap extensions.

.SH ENVIRONMENT VARIABLES

The following environment variable are also consulted:

.TP
.B CAMLRUNPARAM
Set the garbage collection parameters.
This variable must be a sequence of parameter specifications.
A parameter specification is an option letter followed by an =
sign and a decimal number.  There are four options:
.TP
.BR s \ (minor_heap_size)
Size of the minor heap.
.TP
.BR i \ (major_heap_increment)
Minimum size increment for the major heap.
.TP
.BR o \ (space_overhead)
The major GC speed setting.
.TP
.BR v \ (verbose)
Whether to print GC messages or not.  0 is
false; 1 is true; other values may give unexpected results.

.TP
.B PATH
List of directories searched to find the bytecode executable file.

.SH SEE ALSO
.BR ocamlc (1).
.br
.I The Objective Caml user's manual,
chapter "Runtime system".
