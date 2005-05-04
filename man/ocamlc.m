.TH OCAMLC 1

.SH NAME
ocamlc \- The Objective Caml bytecode compiler


.SH SYNOPSIS
.B ocamlc
[
.B \-aciv
]
[
.BI \-cclib \ libname
]
[
.BI \-ccopt \ option
]
[
.B \-custom
]
[
.B \-unsafe
]
[
.BI \-o \ exec-file
]
[
.BI \-I \ lib-dir
]
.I filename ...

.B ocamlc.opt
.I (same options)

.SH DESCRIPTION

The Objective Caml bytecode compiler
.BR ocamlc (1)
compiles Caml source files to bytecode object files and link
these object files to produce standalone bytecode executable files.
These executable files are then run by the bytecode interpreter
.BR ocamlrun (1).

The 
.BR ocamlc (1)
command has a command-line interface similar to the one of
most C compilers. It accepts several types of arguments and processes them
sequentially:

Arguments ending in .mli are taken to be source files for
compilation unit interfaces. Interfaces specify the names exported by
compilation units: they declare value names with their types, define
public data types, declare abstract data types, and so on. From the
file 
.IR x \&.mli,
the 
.BR ocamlc (1)
compiler produces a compiled interface
in the file 
.IR x \&.cmi.

Arguments ending in .ml are taken to be source files for compilation
unit implementations. Implementations provide definitions for the
names exported by the unit, and also contain expressions to be
evaluated for their side-effects.  From the file 
.IR x \&.ml,
the 
.BR ocamlc (1)
compiler produces compiled object bytecode in the file 
.IR x \&.cmo.
 
If the interface file 
.IR x \&.mli
exists, the implementation
.IR x \&.ml
is checked against the corresponding compiled interface
.IR x \&.cmi,
which is assumed to exist. If no interface
.IR x \&.mli
is provided, the compilation of 
.IR x \&.ml
produces a compiled interface file 
.IR x \&.cmi
in addition to the compiled object code file 
.IR x \&.cmo.
The file 
.IR x \&.cmi
produced
corresponds to an interface that exports everything that is defined in
the implementation 
.IR x \&.ml.

Arguments ending in .cmo are taken to be compiled object bytecode.  These
files are linked together, along with the object files obtained
by compiling .ml arguments (if any), and the Caml Light standard
library, to produce a standalone executable program. The order in
which .cmo and.ml arguments are presented on the command line is
relevant: compilation units are initialized in that order at
run-time, and it is a link-time error to use a component of a unit
before having initialized it. Hence, a given 
.IR x \&.cmo
file must come before all .cmo files that refer to the unit 
.IR x .

Arguments ending in .cma are taken to be libraries of object bytecode.
A library of object bytecode packs in a single file a set of object
bytecode files (.cmo files). Libraries are built with 
.B ocamlc \-a
(see the description of the 
.B \-a
option below). The object files
contained in the library are linked as regular .cmo files (see above), in the order specified when the .cma file was built. The only difference is that if an object file
contained in a library is not referenced anywhere in the program, then
it is not linked in.

Arguments ending in .c are passed to the C compiler, which generates a .o object file. This object file is linked with the program if the
.B \-custom
flag is set (see the description of 
.B \-custom
below).

Arguments ending in .o or .a are assumed to be C object files and
libraries. They are passed to the C linker when linking in 
.B \-custom
mode (see the description of 
.B \-custom
below).

.B ocamlc.opt
is the same compiler as
.BR ocamlc ,
but compiled with the native-code compiler
.BR ocamlopt (1).
Thus, it behaves exactly like
.BR ocamlc ,
but compiles faster.
.B ocamlc.opt
is not available in all installations of Objective Caml.

.SH OPTIONS

The following command-line options are recognized by 
.BR ocamlc (1).

.TP
.B \-a
Build a library (.cma file) with the object files (.cmo files) given on the command line, instead of linking them into an executable
file. The name of the library can be set with the 
.B \-o
option. The default name is 
.BR library.cma .
 
.TP
.B \-c
Compile only. Suppress the linking phase of the
compilation. Source code files are turned into compiled files, but no
executable file is produced. This option is useful to
compile modules separately.

.TP
.BI \-cclib\ -l libname
Pass the 
.BI \-l libname
option to the C linker when linking in
``custom runtime'' mode (see the 
.B \-custom
option). This causes the
given C library to be linked with the program.

.TP
.B \-ccopt
Pass the given option to the C compiler and linker, when linking in
``custom runtime'' mode (see the 
.B \-custom
option). For instance,
.B -ccopt -L
.I dir
causes the C linker to search for C libraries in
directory 
.IR dir .

.TP
.B \-custom
Link in ``custom runtime'' mode. In the default linking mode, the
linker produces bytecode that is intended to be executed with the
shared runtime system, 
.BR ocamlrun (1).
In the custom runtime mode, the
linker produces an output file that contains both the runtime system
and the bytecode for the program. The resulting file is larger, but it
can be executed directly, even if the 
.BR ocamlrun (1)
command is not
installed. Moreover, the ``custom runtime'' mode enables linking Caml
code with user-defined C functions.

.TP
.B \-i
Cause the compiler to print all defined names (with their inferred
types or their definitions) when compiling an implementation (.ml
file). This can be useful to check the types inferred by the
compiler. Also, since the output follows the syntax of interfaces, it
can help in writing an explicit interface (.mli file) for a file: just
redirect the standard output of the compiler to a .mli file, and edit
that file to remove all declarations of unexported names.

.TP
.BI \-I  directory
Add the given directory to the list of directories searched for
compiled interface files (.cmi) and compiled object code files
(.cmo). By default, the current directory is searched first, then the
standard library directory. Directories added with
.B -I
are searched
after the current directory, in the order in which they were given on
the command line, but before the standard library directory.

.TP
.BI \-o \ exec-file
Specify the name of the output file produced by the linker. The
default output name is 
.BR a.out ,
in keeping with the Unix tradition. If the 
.B \-a
option is given, specify the name of the library produced.

.TP
.B \-v
Print the version number of the compiler.

.TP
.B \-unsafe
Turn bound checking off on array and string accesses (the 
.B v.(i)
and
.B s.[i]
constructs). Programs compiled with 
.B \-unsafe
are therefore
slightly faster, but unsafe: anything can happen if the program
accesses an array or string outside of its bounds.

.SH SEE ALSO
.BR ocaml (1),
.BR ocamlrun (1).
.br
.I The Objective Caml user's manual,
chapter "Batch compilation".
