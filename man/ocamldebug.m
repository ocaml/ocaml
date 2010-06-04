.TH OCAMLDEBUG 1

.SH NAME
ocamldebug \- the Objective Caml source-level replay debugger.
.SH SYNOPSIS
.B ocamldebug
.RI [\  options \ ]\  program \ [\  arguments \ ]
.SH DESCRIPTION
.B ocamldebug
is the Objective Caml source-level replay debugger.
.SH OPTIONS
A summary of options are included below.
For a complete description, see the html documentation in the ocaml-doc
package.
.TP
.B \-I directory
Add directory to the list of directories searched for source files and
compiled files.
.TP
.B \-s socket
Use socket for communicating with the debugged program.
.TP 
.B \-c count
Set the maximum number of simultaneously live checkpoints to count.
.TP 
.B \-cd directory
Run the debugger program from the given directory,
instead of the current working directory. 
.TP
.B \-emacs
<<<<<<< .courant
Tell the debugger it is executed under Emacs.
=======
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
Print version string and exit.
.TP
.B \-vnum
Print short version number and exit.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.
>>>>>>> .fusion-droit.r10497
.SH SEE ALSO
ocamldebug is documented fully in the Ocaml HTML documentation.
.SH AUTHOR
This manual page was written by Sven LUTHER <luther@debian.org>,
for the Debian GNU/Linux system (but may be used by others).

