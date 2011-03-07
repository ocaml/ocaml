.TH CAMLP4 1  "" "INRIA"
.SH NAME
camlp4 - Pre-Precessor-Pretty-Printer for OCaml
.br
mkcamlp4 - Create custom camlp4
.br
ocpp - Universal preprocessor

.SH SYNOPSIS
.B camlp4
[
load-options
] [--] [
other-options
]
.br
.B camlp4o
[
load-options
] [--] [
other-options
]
.br
.B camlp4r
[
load-options
] [--] [
other-options
]
.br
.B camlp4o.cma
.br
.B camlp4r.cma
.br
.B mkcamlp4
.br
.B ocpp
[
load-options
]
file
.LP
.br
.B camlp4o.opt
[--] [
other-options
]
.br
.B camlp4r.opt
[--] [
other-options
]

.SH DESCRIPTION
.B camlp4
is a Pre-Processor-Pretty-Printer for OCaml, parsing a source
file and printing some result on standard output.
.LP
.B camlp4o
and
.B camlp4r
are versions of
.B camlp4
with some files already loaded (see below).
.LP
.B camlp4o.cma
and
.B camlp4r.cma
are files to be loaded in ocaml toplevel to use the camlp4 machinery.
.LP
.B mkcamlp4
creates camlp4 executables with almost the same options as ocamlmktop (see below).
.LP
.B ocpp
is an universal preprocessor, expanding quotations in any kind of text file.
.LP
.B camlp4o.opt
and
.B camlp4r.opt
are versions of camlp4o and camlp4r compiled by the native-code compiler
ocamlopt. They are faster but not extensible. And they are not available
in all installations of camlp4.

.SH LOAD OPTIONS

The load options select parsing and printing actions recorded in OCaml
object files (ending with .cmo or .cma). They must precede other
options.

.LP
An optional
.B \-\-
may end the load options.

.TP
.BI \-I\  directory
Add
.I directory
in the search path for files loaded. Unless the option \-nolib is used,
the camlp4 library directory is appended to the path. Note that there is
no automatic search in the current directory: add "\-I ." for this.
.TP
.B \-where
Print camlp4 library directory name and exit.
.TP
.B \-nolib
No automatic search for objects files in camlp4 library directory.
.TP
.I object-file
Load
.I object-file
in the camlp4 core.

.SH OTHER OPTIONS

.LP
The others options are:

.TP
.I file
Treat
.I file
as an interface file if it ends with .mli and as an implementation file
if it ends with .ml.

.TP
.BI \-intf\  file
Treat
.I file
as an interface file, whatever its extension is.
.TP
.BI \-impl\  file
Treat
.I file
as an implementation file, whatever its extension is.
.TP
.B \-unsafe
Generate unsafe accesses to arrays and strings.
.TP
.B \-noassert
Do not compile assertion checks.
.TP
.B \-verbose
More verbose in parsing errors.
.TP
.BI \-QD\  file
Dump in
.I file
in case of syntax error in the result of a quotation expansion.
.TP
.BI \-o\  out-file
Print the result in
.I out-file
instead of standard output. File is opened
with open_out_bin (see the documentation of the Pervasives OCaml library).
.TP
.B \-v
Print the version number and exit.
.TP
.B \-help
Print the available options and exit. The output includes the options
possibly added by the loaded object files.

.LP
The others options can be extended by loaded object files. The provided
files add the following options:

.TP
.BI \-l\  line-length
Added by pr_o.cmo and pr_r.cmo: set the line length (default 78).
.TP
.BI \-sep\  string
Added by pr_o.cmo and pr_r.cmo: print this string between phrases instead
of comments.
.TP
.BI \-no_ss
Added by pr_o.cmo: do not print double semicolons
.TP
.BI \-D\  ident
Added by pa_macro.cmo: define the ident.
.TP
.BI \-U\  ident
Added by pa_macro.cmo: undefine the ident.

.SH "PROVIDED FILES"
These files are installed in LIBDIR/camlp4.

.LP
Parsing files:
.nf
.ta 1c
	pa_o.cmo: syntax of OCaml
	pa_op.cmo: streams and parsers
	pa_oop.cmo: streams and parsers (without code optimization)
	pa_r.cmo: revised syntax
	pa_rp.cmo: streams and parsers
	pa_extend.cmo: syntax extension for grammars
	pa_extfold.cmo: extension of pa_extend with FOLD0 and FOLD1
	pa_extfun.cmo: syntax extension for extensible functions
	pa_fstream.cmo: syntax extension for functional streams
	pa_macro.cmo: add macros (ifdef, define) like in C
	pa_lefteval.cmo: left-to-right evaluation of parameters
	pa_olabl.cmo: old syntax for labels
.fi
.LP
Printing files:
.nf
.ta 1c
	pr_o.cmo: syntax of OCaml
	pr_op.cmo: try to rebuild streams and parsers syntax
	pr_r.cmo: revised syntax
	pr_rp.cmo: try to rebuild streams and parsers syntax
	pr_extend.cmo: try to rebuild EXTEND statements
	pr_extfun.cmo: try to rebuild extfun statements
	pr_dump.cmo: syntax tree
	pr_depend.cmo: file dependencies
	pr_null.cmo: no output
.fi
.LP
Quotation expanders:
.nf
.ta 1c
	q_MLast.cmo: syntax tree nodes
	q_phony.cmo: keeping quotations for pretty printing
.fi
.LP
The command
.B camlp4o
is a shortcut for:
.nf
.ta 1c
	camlp4 pa_o.cmo pa_op.cmo pr_dump.cmo
.fi
.LP
The command
.B camlp4r
is a shortcut for:
.nf
.ta 1c
	camlp4 pa_r.cmo pa_rp.cmo pr_dump.cmo
.fi
.LP
.LP
The file
.B camlp4o.cma
can be loaded in the toplevel to start camlp4 with OCaml syntax.
.LP
The file
.B camlp4r.cma
can be loaded in the toplevel to start camlp4 with revised syntax.

.SH "MKCAMLP4"

.B mkcamlp4
creates camlp4 executables with almost the same options than ocamlmktop.
The only difference is that the interfaces to be visible must be explicitly
added in the command line as ".cmi" files. For example, how to add the
the OCaml module "str":

.nf
.ta 1c 2c
	mkcamlp4 -custom str.cmi str.cma -cclib -lstr \\
		-o camlp4str
.fi

.SH "FILES"
Camlp4 library directory in the current installation:
.br
LIBDIR/camlp4

.SH "SEE ALSO"
Camlp4 - tutorial
.br
Camlp4 - reference manual
.br
ocamlc(1), ocaml(1).

.SH AUTHOR
Daniel de Rauglaudre, INRIA Rocquencourt.
