.TH OCAMLDEBUG 1

.SH NAME
ocamldebug \- the Objective Caml source-level replay debugger.
.SH SYNOPSIS
.B ocamldebug
.I "[options] program [arguments]"
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
Tell the debugger it is executed under Emacs.
.SH SEE ALSO
ocamldebug is documented fully in the Ocaml HTML documentation.
.SH AUTHOR
This manual page was written by Sven LUTHER <luther@debian.org>,
for the Debian GNU/Linux system (but may be used by others).

