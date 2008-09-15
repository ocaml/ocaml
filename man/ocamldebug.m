\" $Id$

.TH OCAMLDEBUG 1

.SH NAME
ocamldebug \- the Objective Caml source-level replay debugger.
.SH SYNOPSIS
.B ocamldebug
.I "[options] program [arguments]"
.SH DESCRIPTION
.B ocamldebug
is the Objective Caml source-level replay debugger.

Before the debugger can be used, the program must be compiled and
linked with the
.B \-g
option: all .cmo and .cma files that are part
of the program should have been created with
.BR ocamlc\ \-g ,
and they must be linked together with
.BR ocamlc\ \-g .

Compiling with
.B \-g
entails no penalty on the running time of
programs: object files and bytecode executable files are bigger and
take longer to produce, but the executable files run at
exactly the same speed as if they had been compiled without
.BR \-g .

.SH OPTIONS
A summary of options are included below.
For a complete description, see the html documentation in the ocaml-doc
package.
.TP
.BI \-c \ count
Set the maximum number of simultaneously live checkpoints to
.IR count .
.TP
.BI \-cd \ dir
Run the debugger program from the working directory
.IR dir ,
instead of the current working directory. (See also the
.B cd
command.)
.TP
.B \-emacs
Tell the debugger it is executed under Emacs.  (See
.I "The Objective Caml user's manual"
for information on how to run the debugger under Emacs.)
.TP
.BI \-I \ directory
Add
.I directory
to the list of directories searched for source files and
compiled files.  (See also the
.B directory
command.)
.TP
.BI \-s \ socket
Use
.I socket
for communicating with the debugged program. See the description
of the command
.B set\ socket
in
.I "The Objective Caml user's manual"
for the format of
.IR socket .
.TP
.B \-version
Print version and exit.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.
.SH SEE ALSO
.BR ocamlc (1)
.br
.IR "The Objective Caml user's manual" ,
chapter "The debugger".
.SH AUTHOR
This manual page was written by Sven LUTHER <luther@debian.org>,
for the Debian GNU/Linux system (but may be used by others).
