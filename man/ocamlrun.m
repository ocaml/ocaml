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
arguments are passed to the Objective Caml program, in the string array
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
.B OCAMLRUNPARAM
Set the garbage collection parameters.
(If
.B OCAMLRUNPARAM
is not set,
.B CAMLRUNPARAM
will be used instead.)
This variable must be a sequence of parameter specifications.
A parameter specification is an option letter followed by an =
sign, a decimal number, and an optional multiplier.  There are seven
options:
.TP
.BR b \ (backtrace)
Print a stack backtrace in case of an uncaught exception.
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
.BR O \ (max_overhead)
The heap compaction trigger setting.
.TP
.BR l \ (stack_limit)
The limit (in words) of the stack size.
.TP
.BR h
The initial size of the major heap (in words).
.TP
.BR v \ (verbose)
What GC messages to print to stderr.  This is a sum of values selected
from the following:
.TP
.BR 1
Start of major GC cycle.
.TP
.BR 2
Minor collection and major GC slice.
.TP
.BR 4
Growing and shrinking of the heap.
.TP
.BR 8
Resizing of stacks and memory manager tables.
.TP
.BR 16
Heap compaction.
.TP
.BR 32
Change of GC parameters.
.TP
.BR 64
Computation of major GC slice size.
.TP
.BR 128
Calling of finalisation function.
.TP
.BR 256
Startup messages.

The multiplier is
.B k
,
.B M
, or
.B G
, for multiplication by 2^10, 2^20, and 2^30 respectively.
For example, on a 32-bit machine under bash, the command
.B export OCAMLRUNPARAM='s=256k,v=1'
tells a subsequent
.B ocamlrun
to set its initial minor heap size to 1 megabyte and to print
a message at the start of each major GC cycle.

.TP
.B PATH
List of directories searched to find the bytecode executable file.

.SH SEE ALSO
.BR ocamlc (1).
.br
.I The Objective Caml user's manual,
chapter "Runtime system".
