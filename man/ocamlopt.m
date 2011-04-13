
.TH OCAMLOPT 1

.SH NAME
ocamlopt \- The Objective Caml native-code compiler


.SH SYNOPSIS
.B ocamlopt
[
.B \-acivS
]
[
.BI \-cclib \ libname
]
[
.BI \-ccopt \ option
]
[
.B \-compact
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

.B ocamlopt.opt
.I (same options)

.SH DESCRIPTION
The Objective Caml high-performance
native-code compiler 
.BR ocamlopt (1)
compiles Caml source files to native code object files and link these
object files to produce standalone executables.

The 
.BR ocamlopt (1)
command has a command-line interface very close to that
of 
.BR ocamlc (1).
It accepts the same types of arguments and processes them
sequentially:

Arguments ending in .mli are taken to be source files for
compilation unit interfaces. Interfaces specify the names exported by
compilation units: they declare value names with their types, define
public data types, declare abstract data types, and so on. From the
file 
.IR x \&.mli,
the 
.BR ocamlopt (1)
compiler produces a compiled interface
in the file 
.IR x \&.cmi.
The interface produced is identical to that
produced by the bytecode compiler 
.BR ocamlc (1).

Arguments ending in .ml are taken to be source files for compilation
unit implementations. Implementations provide definitions for the
names exported by the unit, and also contain expressions to be
evaluated for their side-effects.  From the file 
.IR x \&.ml,
the 
.BR ocamlopt (1)
compiler produces two files: 
.IR x \&.o,
containing native object code, and 
.IR x \&.cmx,
containing extra information for linking and
optimization of the clients of the unit. The compiled implementation
should always be referred to under the name 
.IR x \&.cmx
(when given a .o file, 
.BR ocamlopt (1)
assumes that it contains code compiled from C, not from Caml).

The implementation is checked against the interface file 
.IR x \&.mli
(if it exists) as described in the manual for 
.BR ocamlc (1).

Arguments ending in .cmx are taken to be compiled object code.  These
files are linked together, along with the object files obtained
by compiling .ml arguments (if any), and the Caml Light standard
library, to produce a native-code executable program. The order in
which .cmx and .ml arguments are presented on the command line is
relevant: compilation units are initialized in that order at
run-time, and it is a link-time error to use a component of a unit
before having initialized it. Hence, a given 
.IR x \&.cmx
file must come
before all .cmx files that refer to the unit 
.IR x .

Arguments ending in .cmxa are taken to be libraries of object code.
Such a library packs in two files
.IR lib \&.cmxa
and 
.IR lib \&.a
a set of object files (.cmx/.o files). Libraries are build with
.B ocamlopt \-a
(see the description of the
.B \-a
option below). The object
files contained in the library are linked as regular .cmx files (see
above), in the order specified when the library was built. The only
difference is that if an object file contained in a library is not
referenced anywhere in the program, then it is not linked in.

Arguments ending in .c are passed to the C compiler, which generates
a .o object file. This object file is linked with the program.

Arguments ending in .o or .a are assumed to be C object files and
libraries. They are linked with the program.

The output of the linking phase is a regular Unix executable file. It
does not need 
.BR ocamlrun (1)
to run.

.B ocamlopt.opt
is the same compiler as
.BR ocamlopt ,
but compiled with itself instead of with the bytecode compiler
.BR ocamlc (1).
Thus, it behaves exactly like
.BR ocamlopt ,
but compiles faster.
.B ocamlopt.opt
is not available in all installations of Objective Caml.

.SH OPTIONS

The following command-line options are recognized by 
.BR ocamlopt (1).

.TP
.B \-a
Build a library (.cmxa/.a file) with the object files (.cmx/.o
files) given on the command line, instead of linking them into an
executable file. The name of the library can be set with the 
.B \-o
option. The default name is library.cmxa.

.TP
.B \-c
Compile only. Suppress the linking phase of the
compilation. Source code files are turned into compiled files, but no
executable file is produced. This option is useful to
compile modules separately.

.TP
.BI \-cclib\ -l libname
Pass the
.BI -l libname
option to the linker. This causes the given C library to be linked
with the program.

.TP
.BI \-ccopt \ option
Pass the given option to the C compiler and linker. For instance,
.B -ccopt -L
.I dir
causes the C linker to search for C libraries in
directory 
.IR dir .

.TP
.B \-compact
Optimize the produced code for space rather than for time. This
results in smaller but slightly slower programs. The default is to
optimize for speed.

.TP
.B \-i
Cause the compiler to print all defined names (with their inferred
types or their definitions) when compiling an implementation (.ml
file). This can be useful to check the types inferred by the
compiler. Also, since the output follows the syntax of interfaces, it
can help in writing an explicit interface (.mli file) for a file:
just redirect the standard output of the compiler to a .mli file,
and edit that file to remove all declarations of unexported names.

.TP
.BI \-I \ directory
Add the given directory to the list of directories searched for
compiled interface files (.cmi) and compiled object code files
(.cmo). By default, the current directory is searched first, then the
standard library directory. Directories added with -I are searched
after the current directory, in the order in which they were given on
the command line, but before the standard library directory.

.TP
.BI \-o \ exec-file
Specify the name of the output file produced by the linker. The
default output name is a.out, in keeping with the Unix tradition. If
the 
.B \-a
option is given, specify the name of the library produced.

.TP
.B \-pack
Build an object file (.cmx and .o files) and its associated compiled
interface (.cmi) that combines the .cmx object
files given on the command line, making them appear as sub-modules of
the output .cmx file.  The name of the output .cmx file must be
given with the
.B \-o
option.  For instance,
.B ocamlopt\ -pack\ -o\ P.cmx\ A.cmx\ B.cmx\ C.cmx
generates compiled files P.cmx, P.o and P.cmi describing a
compilation unit having three sub-modules A, B and C,
corresponding to the contents of the object files A.cmx, B.cmx and
C.cmx.  These contents can be referenced as P.A, P.B and P.C
in the remainder of the program.

The .cmx object files being combined must have been compiled with
the appropriate
.B \-for\-pack
option.  In the example above,
A.cmx, B.cmx and C.cmx must have been compiled with
.BR ocamlopt\ \-for\-pack\ P .

Multiple levels of packing can be achieved by combining
.B \-pack
with
.BR \-for\-pack .
See
.IR "The Objective Caml user's manual" ,
chapter "Native-code compilation" for more details.
.TP
.BI \-pp \ command
Cause the compiler to call the given
.I command
as a preprocessor for each source file. The output of
.I command
is redirected to
an intermediate file, which is compiled. If there are no compilation
errors, the intermediate file is deleted afterwards.
.TP
.B \-principal
Check information path during type-checking, to make sure that all
types are derived in a principal way. All programs accepted in
.B \-principal
mode are also accepted in default mode with equivalent
types, but different binary signatures.
.TP
.BI \-runtime\-variant \ suffix
Add
.I suffix
to the name of the runtime library that will be used by the program.
If OCaml was configured with option
.BR \-with\-debug\-runtime ,
then the
.B d
suffix is supported and gives a debug version of the runtime.
.TP
.B \-rectypes
Allow arbitrary recursive types during type-checking.  By default,
only recursive types where the recursion goes through an object type
are supported. Note that once you have created an interface using this
flag, you must use it again for all dependencies.
.TP
.B \-S
Keep the assembly code produced during the compilation. The assembly
code for the source file 
.IR x \&.ml
is saved in the file 
.IR x \&.s.

.TP
.B \-v
Print the version number of the compiler.

.TP
.B \-unsafe
Turn bound checking off on array and string accesses (the v.(i) and
s.[i] constructs). Programs compiled with -unsafe are therefore
faster, but unsafe: anything can happen if the program accesses an
<<<<<<< .courant
array or string outside of its bounds.

=======
array or string outside of its bounds. Additionally, turn off the
check for zero divisor in integer division and modulus operations.
With
.BR \-unsafe ,
an integer division (or modulus) by zero can halt the
program or continue with an unspecified result instead of raising a
.B Division_by_zero
exception.
.TP
.B \-v
Print the version number of the compiler and the location of the
standard library directory, then exit.
.TP
.B \-verbose
Print all external commands before they are executed, in particular
invocations of the assembler, C compiler, and linker.
.TP
.BR \-vnum or \-version
Print the version number of the compiler in short form (e.g. "3.11.0"),
then exit.
.TP
.BI \-w \ warning\-list
Enable, disable, or mark as errors the warnings specified by the argument
.IR warning\-list .
See
.BR ocamlc (1)
for the syntax of
.IR warning-list .
.TP
.BI \-warn\-error \ warning\-list
Mark as errors the warnings specified in the argument
.IR warning\-list .
The compiler will stop with an error when one of these
warnings is emitted.  The
.I warning\-list
has the same meaning as for
the
.B \-w
option: a
.B +
sign (or an uppercase letter) turns the corresponding warnings into errors, a
.B \-
sign (or a lowercase letter) turns them back into warnings, and a
.B @
sign both enables and marks the corresponding warnings.

Note: it is not recommended to use warning sets (i.e. letters) as
arguments to
.B \-warn\-error
in production code, because this can break your build when future versions
of OCaml add some new warnings.

The default setting is
.B \-warn\-error\ +a
(none of the warnings is treated as an error).
.TP
.B \-where
Print the location of the standard library, then exit.
.TP
.BI \- \ file
Process
.I file
as a file name, even if it starts with a dash (-) character.
.TP
.BR \-help \ or \ \-\-help
Display a short usage summary and exit.

.SH OPTIONS FOR THE IA32 ARCHITECTURE

The IA32 code generator (Intel Pentium, AMD Athlon) supports the
following additional option:
.TP
.B \-ffast\-math
Use the IA32 instructions to compute
trigonometric and exponential functions, instead of calling the
corresponding library routines.  The functions affected are:
.BR atan ,
.BR atan2 ,
.BR cos ,
.BR log ,
.BR log10 ,
.BR sin ,
.B sqrt
and
.BR tan .
The resulting code runs faster, but the range of supported arguments
and the precision of the result can be reduced.  In particular,
trigonometric operations
.BR cos ,
.BR sin ,
.B tan
have their range reduced to [\-2^64, 2^64].

.SH OPTIONS FOR THE AMD64 ARCHITECTURE

The AMD64 code generator (64-bit versions of Intel Pentium and AMD
Athlon) supports the following additional options:
.TP
.B \-fPIC
Generate position-independent machine code.  This is the default.
.TP
.B \-fno\-PIC
Generate position-dependent machine code.

.SH OPTIONS FOR THE SPARC ARCHITECTURE
The Sparc code generator supports the following additional options:
.TP
.B \-march=v8
Generate SPARC version 8 code.
.TP
.B \-march=v9
Generate SPARC version 9 code.
.P
The default is to generate code for SPARC version 7, which runs on all
SPARC processors.

>>>>>>> .fusion-droit.r10497
.SH SEE ALSO
.BR ocamlc (1).
.br
.I The Objective Caml user's manual,
chapter "Native-code compilation".
