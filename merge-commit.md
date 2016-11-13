```diff
commit bb280404ed3da369bcd2b0c684f66be2960a0d25
Merge: 4f9f925 7020a0b
Author: Gabriel Scherer <gabriel.scherer@gmail.com>
Date:   Sun Nov 13 14:40:12 2016 -0500

    Merge 4.04 into trunk-with-4.04 (using imerge)

diff --cc Changes
index c683101,699bc91..3e9e6df
--- a/Changes
+++ b/Changes
@@@ -144,6 -38,6 +144,9 @@@ OCaml 4.04.0 (4 Nov 2016)
    limitation should be lifted in the future (see MPR#7364).
    (Damien Doligez)
  
++- PR#7233: Support GADT equations on non-local abstract types
++  (Jacques Garrigue)
++
  ### Compiler user-interface and warnings:
  
  * PR#6475, GPR#464: interpret all command-line options before compiling any
@@@ -184,10 -78,10 +187,6 @@@
  
  ### Standard library:
  
--- GPR#473: Provide `Sys.backend_type` so that user can write backend-specific
--  code in some cases (for example,  code generator).
--  (Hongbo Zhang)
--
  - PR#6279, GPR#553: implement Set.map
    (Gabriel Scherer)
  
@@@ -195,6 -89,6 +194,10 @@@
    "transitive" heap size of a value
    (Alain Frisch, review by Mark Shinwell and Damien Doligez)
  
++- GPR#473: Provide `Sys.backend_type` so that user can write backend-specific
++  code in some cases (for example,  code generator).
++  (Hongbo Zhang)
++
  - GPR#589: Add a non-allocating function to recover the number of
    allocated minor words.
    (Pierre Chambart, review by Damien Doligez and Gabriel Scherer)
@@@ -499,6 -392,9 +502,25 @@@
  
  - GPR#880: Fix [@@inline] with default parameters in flambda (Leo White)
  
++- GPR#779: Building native runtime on Windows could fail when bootstrapping
++  FlexDLL if there was also a system-installed flexlink
++  (David Allsopp, report Michael Soegtrop)
++
++- GPR#805, GPR#815, GPR#833: check for integer overflow in String.concat
++  (Jeremy Yallop,
++   review by Damien Doligez, Alain Frisch, Daniel Bünzli, Fabrice Le Fessant)
++
++- GPR#810: check for integer overflow in Array.concat
++  (Jeremy Yallop)
++
++- GPR#814: fix the Buffer.add_substring bounds check to handle overflow
++  (Jeremy Yallop)
++
++- GPR#880: Fix [@@inline] with default parameters in flambda (Leo White)
++
+ - GPR#525: fix build on OpenIndiana
+   (Sergey Avseyev, review by Damien Doligez)
+ 
  ### Internal/compiler-libs changes:
  
  - PR#7200, GPR#539: Improve, fix, and add test for parsing/pprintast.ml
@@@ -800,11 -696,16 +822,16 @@@ OCaml 4.03.0 (25 Apr 2016)
  
  ### Runtime system:
  
- * GPR#442, caml_fill_string, and caml_create_string is deprecated
-   and will be removed in the future, please use caml_fill_bytes
-   and caml_create_bytes for migration
 -* GPR#596: make string/bytes distinguishable in the underlying
++* GPR#442, GPR#596: make string/bytes distinguishable in the underlying
+   compiler implementation; caml_fill_string and caml_create_string are
+   deprecated and will be removed in the future, please use
+   caml_fill_bytes and caml_create_bytes for migration
    (Hongbo Zhang, review by Damien Doligez, Alain Frisch, and Hugo Heuzard)
  
+ - GPR#772 %string_safe_set and %string_unsafe_set are deprecated aliases
+   for %bytes_safe_set and %bytes_unsafe_set.
+   (Hongbo Zhang and Damien Doligez)
+ 
  - PR#3612, PR#92: allow allocating custom block with finalizers
    in the minor heap.
    (Pierre Chambart)
diff --cc boot/ocamlc
index 83b5652,0c31cda..52876e3
Binary files differ
diff --cc boot/ocamldep
index 34e8776,eacfcf0..ee2c67e
Binary files differ
diff --cc boot/ocamllex
index 97b53f3,0e9b1b4..d64efda
Binary files differ
diff --cc byterun/Makefile.nt
index 2bc84ec,e74bdd9..5d11768
--- a/byterun/Makefile.nt
+++ b/byterun/Makefile.nt
@@@ -13,9 -13,10 +13,10 @@@
  #*                                                                        *
  #**************************************************************************
  
 -include Makefile.common
 +include Makefile.shared
  
  CFLAGS=-DOCAML_STDLIB_DIR='"$(LIBDIR)"' $(IFLEXDIR)
+ DFLAGS=$(CFLAGS) -DDEBUG
  
  ifdef BOOTSTRAPPING_FLEXLINK
  MAKE_OCAMLRUN=$(MKEXE_BOOT)
diff --cc driver/main.ml
index c68a2e9,e9af202..083650d
--- a/driver/main.ml
+++ b/driver/main.ml
@@@ -182,17 -124,13 +128,13 @@@ end
  let main () =
    try
      readenv ppf Before_args;
 -    Arg.parse Options.list anonymous usage;
 +    Arg.parse_expand Options.list anonymous usage;
-     if !output_name <> None && !compile_only &&
-           List.length !process_thunks > 1 then
-       fatal "Options -c -o are incompatible with compiling multiple files";
-     let final_output_name = !output_name in
-     if !output_name <> None && not !compile_only then
-       (* We're invoked like: ocamlc -o foo bar.c baz.ml.
-          Make sure the intermediate products don't clash with the final one. *)
-       output_name := None;
-     List.iter (fun f -> f ()) (List.rev !process_thunks);
-     output_name := final_output_name;
+     Compenv.process_deferred_actions
+       (ppf,
+        Compile.implementation,
+        Compile.interface,
+        ".cmo",
+        ".cma");
      readenv ppf Before_link;
      if
        List.length (List.filter (fun x -> !x)
diff --cc driver/optmain.ml
index b3f662c,2c6d60e..18a58a9
--- a/driver/optmain.ml
+++ b/driver/optmain.ml
@@@ -292,17 -231,13 +234,13 @@@ let main () 
    let ppf = Format.err_formatter in
    try
      readenv ppf Before_args;
 -    Arg.parse (Arch.command_line_options @ Options.list) anonymous usage;
 +    Arg.parse_expand (Arch.command_line_options @ Options.list) anonymous usage;
-     if !output_name <> None && !compile_only &&
-           List.length !process_thunks > 1 then
-       fatal "Options -c -o are incompatible with compiling multiple files";
-     let final_output_name = !output_name in
-     if !output_name <> None && not !compile_only then
-       (* We're invoked like: ocamlopt -o foo bar.c baz.ml.
-          Make sure the intermediate products don't clash with the final one. *)
-       output_name := None;
-     List.iter (fun f -> f ()) (List.rev !process_thunks);
-     output_name := final_output_name;
+     Compenv.process_deferred_actions
+       (ppf,
+        Optcompile.implementation ~backend,
+        Optcompile.interface,
+        ".cmx",
+        ".cmxa");
      readenv ppf Before_link;
      if
        List.length (List.filter (fun x -> !x)
diff --cc manual/README.md
index 3d9446e,f8e9339..6ee6790
--- a/manual/README.md
+++ b/manual/README.md
@@@ -89,13 -89,8 +89,14 @@@ chapters (or sometimes sections) are ma
      - The ocamlbuild compilation manager: `ocamlbuild.etex`
      - Interfacing C with OCaml: `intf-c.etex`
      - Optimisation with Flambda: `flambda.etex`
+     - Memory profiling with Spacetime: `spacetime.etex`
  
 +Note that ocamlc,ocamlopt and the toplevel options overlap a lot.
 +Consequently, these options are described together in the file
 +`unified-options.etex` and then included from `comp.etex`, `native.etex`,
 +and `top.etex`. If you need to update this list of options, the top comment
 +of `unified-options.etex` contains the relevant information.
 +
  - Part IV, The OCaml library: 'libref'
   This parts contains an brief presentation of all libraries bundled with the
   compilers and the api documentation generated for these libraries.
diff --cc manual/manual/cmds/Makefile
index 6d7e580,26da7bc..2e6d88f
--- a/manual/manual/cmds/Makefile
+++ b/manual/manual/cmds/Makefile
@@@ -1,6 -1,6 +1,7 @@@
  FILES=comp.tex top.tex runtime.tex native.tex lexyacc.tex intf-c.tex \
    depend.tex profil.tex debugger.tex browser.tex ocamldoc.tex \
-   warnings-help.tex ocamlbuild.tex flambda.tex unified-options.tex
 -  warnings-help.tex ocamlbuild.tex flambda.tex spacetime.tex
++  warnings-help.tex ocamlbuild.tex flambda.tex spacetime.tex \
++  unified-options.tex
  
  TOPDIR=../../..
  include $(TOPDIR)/Makefile.tools
@@@ -9,10 -9,6 +10,10 @@@ TRANSF=$(OCAMLRUN) ../../tools/trans
  TEXQUOTE=../../tools/texquote2
  FORMAT=../../tools/format-intf
  
- WITH_TRANSF= ocamldoc.tex top.tex intf-c.tex flambda.tex lexyacc.tex debugger.tex
++WITH_TRANSF= ocamldoc.tex top.tex intf-c.tex flambda.tex spacetime.tex lexyacc.tex debugger.tex
 +
 +etex-files: $(FILES)
 +
  all: $(FILES)
  
  clean::
diff --cc manual/manual/cmds/comp.etex
index 6fae076,36b57f5..433d072
--- a/manual/manual/cmds/comp.etex
+++ b/manual/manual/cmds/comp.etex
@@@ -105,19 -105,470 +105,31 @@@ the bytecode interpreter by itself
  
  The following command-line options are recognized by "ocamlc".
  The options "-pack", "-a", "-c" and "-output-obj" are mutually exclusive.
 -
 -
 -\begin{options}
 -
 -\item["-a"]
 -Build a library (".cma" file) with the object files (".cmo" files)
 -given on the command line, instead of linking them into an executable
 -file. The name of the library must be set with the "-o" option.
 -
 -If "-custom", "-cclib" or "-ccopt" options are passed on the command
 -line, these options are stored in the resulting ".cma" library.  Then,
 -linking with this library automatically adds back the "-custom",
 -"-cclib" and "-ccopt" options as if they had been provided on the
 -command line, unless the "-noautolink" option is given.
 -
 -\item["-absname"]
 -Force error messages to show absolute paths for file names.
 -
 -\item["-annot"]
 -Dump detailed information about the compilation (types, bindings,
 -tail-calls, etc).  The information for file \var{src}".ml"
 -is put into file \var{src}".annot".  In case of a type error, dump
 -all the information inferred by the type-checker before the error.
 -The \var{src}".annot" file can be used with the emacs commands given in
 -"emacs/caml-types.el" to display types and other annotations
 -interactively.
 -
 -\item["-bin-annot"]
 -Dump detailed information about the compilation (types, bindings,
 -tail-calls, etc) in binary format. The information for file \var{src}".ml"
 -is put into file \var{src}".cmt".  In case of a type error, dump
 -all the information inferred by the type-checker before the error.
 -The "*.cmt" files produced by "-bin-annot" contain more information
 -and are much more compact than the files produced by "-annot".
 -
 -\item["-c"]
 -Compile only. Suppress the linking phase of the
 -compilation. Source code files are turned into compiled files, but no
 -executable file is produced. This option is useful to
 -compile modules separately.
 -
 -\item["-cc" \var{ccomp}]
 -Use \var{ccomp} as the C linker when linking in ``custom runtime''
 -mode (see the "-custom" option)
 -and as the C compiler for compiling ".c" source files.
 -
 -\item["-cclib" "-l"\var{libname}]
 -Pass the "-l"\var{libname} option to the C linker when linking in
 -``custom runtime'' mode (see the "-custom" option). This causes the
 -given C library to be linked with the program.
 -
 -\item["-ccopt" \var{option}]
 -Pass the given option to the C compiler and linker. When linking in
 -``custom runtime'' mode, for instance,
 -"-ccopt -L"\var{dir} causes the C linker to search for C libraries in
 -directory \var{dir}.   (See the "-custom" option.)
 -
 -\item["-color" \var{mode}]
 -Enable or disable colors in compiler messages (especially warnings and errors).
 -The following modes are supported:
 -\begin{description}
 -  \item["auto"] use heuristics to enable colors only if the output supports them (an ANSI-compatible tty terminal);
 -  \item["always"] enable colors unconditionally;
 -  \item["never"] disable color output.
 -\end{description}
 -The default setting is 'auto', and the current heuristic
 -checks that the "TERM" environment variable exists and is
 -not empty or "dumb", and that \verb!isatty(stderr)! holds.
 -
 -
 -\item["-compat-32"]
 -Check that the generated bytecode executable can run on 32-bit
 -platforms and signal an error if it cannot. This is useful when
 -compiling bytecode on a 64-bit machine.
 -
 -\item["-config"]
 -Print the version number of "ocamlc" and a detailed summary of its
 -configuration, then exit.
 -
 -\item["-custom"]
 -Link in ``custom runtime'' mode. In the default linking mode, the
 -linker produces bytecode that is intended to be executed with the
 -shared runtime system, "ocamlrun". In the custom runtime mode, the
 -linker produces an output file that contains both the runtime system
 -and the bytecode for the program. The resulting file is larger, but it
 -can be executed directly, even if the "ocamlrun" command is not
 -installed. Moreover, the ``custom runtime'' mode enables static
 -linking of OCaml code with user-defined C functions, as described in
 -chapter~\ref{c:intf-c}.
 -\begin{unix}
 -Never use the "strip" command on executables produced by "ocamlc -custom",
 -this would remove the bytecode part of the executable.
 -\end{unix}
 -
 -\item["-dllib" "-l"\var{libname}]
 -Arrange for the C shared library "dll"\var{libname}".so"
 -("dll"\var{libname}".dll" under Windows) to be loaded dynamically
 -by the run-time system "ocamlrun" at program start-up time.
 -
 -\item["-dllpath" \var{dir}]
 -Adds the directory \var{dir} to the run-time search path for shared
 -C libraries.  At link-time, shared libraries are searched in the
 -standard search path (the one corresponding to the "-I" option).
 -The "-dllpath" option simply stores \var{dir} in the produced
 -executable file, where "ocamlrun" can find it and use it as
 -described in section~\ref{s-ocamlrun-dllpath}.
 -
 -\item["-for-pack" \var{module-path}]
 -Generate an object file (".cmo") that can later be
 -included
 -as a sub-module (with the given access path) of a compilation unit
 -constructed with "-pack".  For instance, "ocamlc -for-pack P -c A.ml"
 -will generate "a.cmo" that can later be used with
 -"ocamlc -pack -o P.cmo a.cmo".
 -Note: you can still pack a module that was compiled without
 -"-for-pack"
 -but in this case exceptions will be printed with the wrong names.
 -
 -\item["-g"]
 -Add debugging information while compiling and linking. This option is
 -required in order to be able to debug the program with "ocamldebug"
 -(see chapter~\ref{c:debugger}), and to produce stack backtraces when
 -the program terminates on an uncaught exception (see
 -section~\ref{ocamlrun-options}).
 -
 -\item["-i"]
 -Cause the compiler to print all defined names (with their inferred
 -types or their definitions) when compiling an implementation (".ml"
 -file).  No compiled files (".cmo" and ".cmi" files) are produced.
 -This can be useful to check the types inferred by the
 -compiler. Also, since the output follows the syntax of interfaces, it
 -can help in writing an explicit interface (".mli" file) for a file:
 -just redirect the standard output of the compiler to a ".mli" file,
 -and edit that file to remove all declarations of unexported names.
 -
 -\item["-I" \var{directory}]
 -Add the given directory to the list of directories searched for
 -compiled interface files (".cmi"), compiled object code files
 -(".cmo"), libraries (".cma"), and C libraries specified with
 -"-cclib -lxxx".  By default, the current directory is
 -searched first, then the standard library directory. Directories added
 -with "-I" are searched after the current directory, in the order in
 -which they were given on the command line, but before the standard
 -library directory. See also option "-nostdlib".
 -
 -If the given directory starts with "+", it is taken relative to the
 -standard library directory.  For instance, "-I +labltk" adds the
 -subdirectory "labltk" of the standard library to the search path.
 -
 -\item["-impl" \var{filename}]
 -Compile the file \var{filename} as an implementation file, even if its
 -extension is not ".ml".
 -
 -\item["-intf" \var{filename}]
 -Compile the file \var{filename} as an interface file, even if its
 -extension is not ".mli".
 -
 -\item["-intf-suffix" \var{string}]
 -Recognize file names ending with \var{string} as interface files
 -(instead of the default ".mli").
 -
 -\item["-labels"]
 -Labels are not ignored in types, labels may be used in applications,
 -and labelled parameters can be given in any order.  This is the default.
 -
 -\item["-linkall"]
 -Force all modules contained in libraries to be linked in. If this
 -flag is not given, unreferenced modules are not linked in. When
 -building a library (option "-a"), setting the "-linkall" option forces all
 -subsequent links of programs involving that library to link all the
 -modules contained in the library.
 -
 -\item["-make-runtime"]
 -Build a custom runtime system (in the file specified by option "-o")
 -incorporating the C object files and libraries given on the command
 -line.  This custom runtime system can be used later to execute
 -bytecode executables produced with the
 -"ocamlc -use-runtime" \var{runtime-name} option.
 -See section~\ref{s:custom-runtime} for more information.
 -
 -\item["-no-alias-deps"]
 -Do not record dependencies for module aliases. See
 -section~\ref{s:module-alias} for more information.
 -
 -\item["-no-app-funct"]
 -Deactivates the applicative behaviour of functors. With this option,
 -each functor application generates new types in its result and
 -applying the same functor twice to the same argument yields two
 -incompatible structures.
 -
 -\item["-noassert"]
 -Do not compile assertion checks.  Note that the special form
 -"assert false" is always compiled because it is typed specially.
 -This flag has no effect when linking already-compiled files.
 -
 -\item["-noautolink"]
 -When linking ".cma" libraries, ignore "-custom", "-cclib" and "-ccopt"
 -options potentially contained in the libraries (if these options were
 -given when building the libraries).  This can be useful if a library
 -contains incorrect specifications of C libraries or C options; in this
 -case, during linking, set "-noautolink" and pass the correct C
 -libraries and options on the command line.
 -
 -\item["-nolabels"]
 -Ignore non-optional labels in types. Labels cannot be used in
 -applications, and parameter order becomes strict.
 -
 -\item["-nostdlib"]
 -Do not include the standard library directory in the list of
 -directories searched for
 -compiled interface files (".cmi"), compiled object code files
 -(".cmo"), libraries (".cma"), and C libraries specified with
 -"-cclib -lxxx". See also option "-I".
 -
 -\item["-o" \var{exec-file}]
 -Specify the name of the output file produced by the compiler. The
 -default output name is "a.out" under Unix and "camlprog.exe" under
 -Windows. If the "-a" option is given, specify the name of the library
 -produced.  If the "-pack" option is given, specify the name of the
 -packed object file produced.  If the "-output-obj" option is given,
 -specify the name of the output file produced.  If the "-c" option is
 -given, specify the name of the object file produced for the {\em next}
 -source file that appears on the command line.
 -
 -\item["-open" \var{Module}]
 -Opens the given module before processing the interface or
 -implementation files. If several "-open" options are given,
 -they are processed in order, just as if
 -the statements "open!" \var{Module1}";;" "..." "open!" \var{ModuleN}";;"
 -were added at the top of each file.
 -
 -\item["-output-obj"]
 -Cause the linker to produce a C object file instead of a bytecode
 -executable file. This is useful to wrap OCaml code as a C library,
 -callable from any C program. See chapter~\ref{c:intf-c},
 -section~\ref{s:embedded-code}. The name of the output object file
 -must be set with the "-o" option. This
 -option can also be used to produce a C source file (".c" extension) or
 -a compiled shared/dynamic library (".so" extension, ".dll" under Windows).
 -
 -\item["-pack"]
 -Build a bytecode object file (".cmo" file) and its associated compiled
 -interface (".cmi") that combines the object
 -files given on the command line, making them appear as sub-modules of
 -the output ".cmo" file.  The name of the output ".cmo" file must be
 -given with the "-o" option.  For instance,
 -\begin{verbatim}
 -        ocamlc -pack -o p.cmo a.cmo b.cmo c.cmo
 -\end{verbatim}
 -generates compiled files "p.cmo" and "p.cmi" describing a compilation
 -unit having three sub-modules "A", "B" and "C", corresponding to the
 -contents of the object files "a.cmo", "b.cmo" and "c.cmo".  These
 -contents can be referenced as "P.A", "P.B" and "P.C" in the remainder
 -of the program.
 -
 -\item["-plugin" \var{plugin}]
 -Dynamically load the code of the given \var{plugin}
 -(a ".cmo", ".cma" or ".cmxs" file) in the compiler. \var{plugin} must exist in
 -the same kind of code as the compiler ("ocamlc.byte" must load bytecode
 -plugins, while "ocamlc.opt" must load native code plugins), and
 -extension adaptation is done automatically for ".cma" files (to ".cmxs" files
 -if the compiler is compiled in native code).
 -
 -\item["-pp" \var{command}]
 -Cause the compiler to call the given \var{command} as a preprocessor
 -for each source file. The output of \var{command} is redirected to
 -an intermediate file, which is compiled. If there are no compilation
 -errors, the intermediate file is deleted afterwards.
 -
 -\item["-ppx" \var{command}]
 -After parsing, pipe the abstract syntax tree through the preprocessor
 -\var{command}. The module "Ast_mapper", described in
 -chapter~\ref{Ast-underscoremapper}, implements the external interface
 -of a preprocessor.
 -
 -\item["-principal"]
 -Check information path during type-checking, to make sure that all
 -types are derived in a principal way.  When using labelled arguments
 -and/or polymorphic methods, this flag is required to ensure future
 -versions of the compiler will be able to infer types correctly, even
 -if internal algorithms change.
 -All programs accepted in "-principal" mode are also accepted in the
 -default mode with equivalent types, but different binary signatures,
 -and this may slow down type checking; yet it is a good idea to
 -use it once before publishing source code.
 -
 -\item["-rectypes"]
 -Allow arbitrary recursive types during type-checking.  By default,
 -only recursive types where the recursion goes through an object type
 -are supported. Note that once you have created an interface using this
 -flag, you must use it again for all dependencies.
 -
 -\item["-runtime-variant" \var{suffix}]
 -Add the \var{suffix} string to the name of the runtime library used by
 -the program.  Currently, only one such suffix is supported: "d", and
 -only if the OCaml compiler was configured with option
 -"-with-debug-runtime".  This suffix gives the debug version of the
 -runtime, which is useful for debugging pointer problems in low-level
 -code such as C stubs.
 -
 -\item["-safe-string"]
 -Enforce the separation between types "string" and "bytes",
 -thereby making strings read-only. This will become the default in
 -a future version of OCaml.
 -
 -\item["-short-paths"]
 -When a type is visible under several module-paths, use the shortest
 -one when printing the type's name in inferred interfaces and error and
 -warning messages. Identifier names starting with an underscore "_" or
 -containing double underscores "__" incur a penalty of $+10$ when computing
 -their length.
 -
 -\item["-strict-sequence"]
 -Force the left-hand part of each sequence to have type unit.
 -
 -\item["-strict-formats"]
 -Reject invalid formats that were accepted in legacy format
 -implementations. You should use this flag to detect and fix such
 -invalid formats, as they will be rejected by future OCaml versions.
 -
 -\item["-thread"]
 -Compile or link multithreaded programs, in combination with the
 -system "threads" library described in chapter~\ref{c:threads}.
 -
 -\item["-unboxed-types"]
 -When a type is unboxable (i.e. a record with a single argument or a
 -concrete datatype with a single constructor of one argument) it will
 -be unboxed unless annotated with "[@@ocaml.boxed]".
 -
 -\item["-no-unboxed-types"]
 -When a type is unboxable  it will be boxed unless annotated with
 -"[@@ocaml.unboxed]". This is the default.
 -
 -\item["-unsafe"]
 -Turn bound checking off for array and string accesses (the "v.(i)" and
 -"s.[i]" constructs). Programs compiled with "-unsafe" are therefore
 -slightly faster, but unsafe: anything can happen if the program
 -accesses an array or string outside of its bounds.
 -
 -\item["-unsafe-string"]
 -Identify the types "string" and "bytes",
 -thereby making strings writable. For reasons of backward compatibility,
 -this is the default setting for the moment, but this will change in a future
 -version of OCaml.
 -
 -\item["-use-runtime" \var{runtime-name}]
 -Generate a bytecode executable file that can be executed on the custom
 -runtime system \var{runtime-name}, built earlier with
 -"ocamlc -make-runtime" \var{runtime-name}.
 -See section~\ref{s:custom-runtime} for more information.
 -
 -\item["-v"]
 -Print the version number of the compiler and the location of the
 -standard library directory, then exit.
 -
 -\item["-verbose"]
 -Print all external commands before they are executed, in particular
 -invocations of the C compiler and linker in "-custom" mode.  Useful to
 -debug C library problems.
 -
 -\item["-vmthread"]
 -Compile or link multithreaded programs, in combination with the
 -VM-level "threads" library described in chapter~\ref{c:threads}.
 -
 -\item["-version" or "-vnum"]
 -Print the version number of the compiler in short form (e.g. "3.11.0"),
 -then exit.
 -
 -\item["-w" \var{warning-list}]
 -Enable, disable, or mark as fatal the warnings specified by the argument
 -\var{warning-list}.
 -Each warning can be {\em enabled} or {\em disabled}, and each warning
 -can be {\em fatal} or {\em non-fatal}.
 -If a warning is disabled, it isn't displayed and doesn't affect
 -compilation in any way (even if it is fatal).  If a warning is
 -enabled, it is displayed normally by the compiler whenever the source
 -code triggers it.  If it is enabled and fatal, the compiler will also
 -stop with an error after displaying it.
 -
 -The \var{warning-list} argument is a sequence of warning specifiers,
 -with no separators between them.  A warning specifier is one of the
 -following:
 -
 -\begin{options}
 -\item["+"\var{num}] Enable warning number \var{num}.
 -\item["-"\var{num}] Disable warning number \var{num}.
 -\item["@"\var{num}] Enable and mark as fatal warning number \var{num}.
 -\item["+"\var{num1}..\var{num2}] Enable warnings in the given range.
 -\item["-"\var{num1}..\var{num2}] Disable warnings in the given range.
 -\item["@"\var{num1}..\var{num2}] Enable and mark as fatal warnings in
 -the given range.
 -\item["+"\var{letter}] Enable the set of warnings corresponding to
 -\var{letter}. The letter may be uppercase or lowercase.
 -\item["-"\var{letter}] Disable the set of warnings corresponding to
 -\var{letter}. The letter may be uppercase or lowercase.
 -\item["@"\var{letter}] Enable and mark as fatal the set of warnings
 -corresponding to \var{letter}. The letter may be uppercase or
 -lowercase.
 -\item[\var{uppercase-letter}] Enable the set of warnings corresponding
 -to \var{uppercase-letter}.
 -\item[\var{lowercase-letter}] Disable the set of warnings corresponding
 -to \var{lowercase-letter}.
 -\end{options}
 -
 -Warning numbers and letters which are out of the range of warnings
 -that are currently defined are ignored. The warnings are as follows.
 -\begin{options}
 -\input{warnings-help.tex}
 -\end{options}
 -Some warnings are described in more detail in section~\ref{s:comp-warnings}.
 -
 -The default setting is "-w +a-4-6-7-9-27-29-32..39-41..42-44-45-48-50".
 -It is displayed by "ocamlc -help".
 -Note that warnings 5 and 10 are not always triggered, depending on
 -the internals of the type checker.
 -
 -\item["-warn-error" \var{warning-list}]
 -Mark as fatal the warnings specified in the argument \var{warning-list}.
 -The compiler will stop with an error when one of these warnings is
 -emitted. The \var{warning-list} has the same meaning as for
 -the "-w" option: a "+" sign (or an uppercase letter) marks the
 -corresponding warnings as fatal, a "-"
 -sign (or a lowercase letter) turns them back into non-fatal warnings, and a
 -"@" sign both enables and marks as fatal the corresponding warnings.
 -
 -Note: it is not recommended to use warning sets (i.e. letters) as
 -arguments to "-warn-error"
 -in production code, because this can break your build when future versions
 -of OCaml add some new warnings.
 -
 -The default setting is "-warn-error -a+31" (only warning 31 is fatal).
 -
 -\item["-warn-help"]
 -Show the description of all available warning numbers.
 -
 -\item["-where"]
 -Print the location of the standard library, then exit.
 -
 -\item["-" \var{file}]
 -Process \var{file} as a file name, even if it starts with a dash ("-")
 -character.
 -
 -\item["-help" or "--help"]
 -Display a short usage summary and exit.
 -%
 -\end{options}
 +% Define boolean variables used by the macros in unified-options.etex
 +\newif\ifcomp \comptrue
 +\newif\ifnat \natfalse
 +\newif\iftop \topfalse
 +% unified-options gathers all options across the native/bytecode
 +% compilers and toplevel
 +\input{unified-options.tex}
  
- \noindent
- On native Windows, the following environment variable is also consulted:
+ \paragraph{Contextual control of command-line options}
+ 
+ The compiler command line can be modified ``from the outside''
+ with the following mechanisms. These are experimental
+ and subject to change. They should be used only for experimental and
+ development work, not in released packages.
  
  \begin{options}
- \item["OCAML_FLEXLINK"]  Alternative executable to use instead of the
+ \item["OCAMLPARAM" \rm(environment variable)]
+ Arguments that will be inserted before or after the arguments from the
+ command line.
+ \item["ocaml_compiler_internal_params" \rm(file in the stdlib directory)]
+ A mapping of file names to lists of arguments that
+ will be added to the command line (and "OCAMLPARAM") arguments.
+ \item["OCAML_FLEXLINK" \rm(environment variable)]
+ Alternative executable to use on native
+ Windows for "flexlink" instead of the
  configured value. Primarily used for bootstrapping.
  \end{options}
  
diff --cc manual/manual/cmds/native.etex
index 39cec51,4291a33..96ac793
--- a/manual/manual/cmds/native.etex
+++ b/manual/manual/cmds/native.etex
@@@ -93,22 -93,437 +93,14 @@@ The following command-line options are 
  The options "-pack", "-a", "-shared", "-c" and "-output-obj" are mutually
  exclusive.
  
 -\begin{options}
 -
 -\item["-a"]
 -Build a library (".cmxa" and ".a"/".lib" files) with the object files
 -(".cmx" and ".o"/".obj" files) given on the command line, instead of
 -linking them into an executable file. The name of the library must be
 -set with the "-o" option.
 -
 -If "-cclib" or "-ccopt" options are passed on the command
 -line, these options are stored in the resulting ".cmxa" library.  Then,
 -linking with this library automatically adds back the
 -"-cclib" and "-ccopt" options as if they had been provided on the
 -command line, unless the "-noautolink" option is given.
 -
 -\item["-absname"]
 -Force error messages to show absolute paths for file names.
 -
 -\item["-annot"]
 -Dump detailed information about the compilation (types, bindings,
 -tail-calls, etc).  The information for file \var{src}".ml"
 -is put into file \var{src}".annot".  In case of a type error, dump
 -all the information inferred by the type-checker before the error.
 -The \var{src}".annot" file can be used with the emacs commands given in
 -"emacs/caml-types.el" to display types and other annotations
 -interactively.
 -
 -\item["-bin-annot"]
 -Dump detailed information about the compilation (types, bindings,
 -tail-calls, etc) in binary format. The information for file \var{src}".ml"
 -is put into file \var{src}".cmt".  In case of a type error, dump
 -all the information inferred by the type-checker before the error.
 -The "*.cmt" files produced by "-bin-annot" contain more information
 -and are much more compact than the files produced by "-annot".
 -
 -\item["-c"]
 -Compile only. Suppress the linking phase of the
 -compilation. Source code files are turned into compiled files, but no
 -executable file is produced. This option is useful to
 -compile modules separately.
 -
 -\item["-cc" \var{ccomp}]
 -Use \var{ccomp} as the C linker called to build the final executable
 -and as the C compiler for compiling ".c" source files.
 -
 -\item["-cclib" "-l"\var{libname}]
 -Pass the "-l"\var{libname} option to the linker. This causes the given
 -C library to be linked with the program.
 -
 -\item["-ccopt" \var{option}]
 -Pass the given option to the C compiler and linker. For instance,
 -"-ccopt -L"\var{dir} causes the C linker to search for C libraries in
 -directory \var{dir}.
 -
 -\item["-compact"]
 -Optimize the produced code for space rather than for time. This
 -results in slightly smaller but slightly slower programs. The default is to
 -optimize for speed.
 -
 -\item["-config"]
 -Print the version number of "ocamlopt" and a detailed summary of its
 -configuration, then exit.
 -
 -\item["-for-pack" \var{module-path}]
 -Generate an object file (".cmx" and ".o"/".obj" files) that can later be
 -included
 -as a sub-module (with the given access path) of a compilation unit
 -constructed with "-pack".  For instance, "ocamlopt -for-pack P -c A.ml"
 -will generate "a.cmx" and "a.o" files that can later be used with
 -"ocamlopt -pack -o P.cmx a.cmx".
 -
 -\item["-g"]
 -Add debugging information while compiling and linking. This option is
 -required in order to produce stack backtraces when
 -the program terminates on an uncaught exception (see
 -section~\ref{ocamlrun-options}).
 -
 -\item["-i"]
 -Cause the compiler to print all defined names (with their inferred
 -types or their definitions) when compiling an implementation (".ml"
 -file).  No compiled files (".cmo" and ".cmi" files) are produced.
 -This can be useful to check the types inferred by the
 -compiler. Also, since the output follows the syntax of interfaces, it
 -can help in writing an explicit interface (".mli" file) for a file:
 -just redirect the standard output of the compiler to a ".mli" file,
 -and edit that file to remove all declarations of unexported names.
 -
 -\item["-I" \var{directory}]
 -Add the given directory to the list of directories searched for
 -compiled interface files (".cmi"), compiled object code files
 -(".cmx"), and libraries (".cmxa"). By default, the current directory
 -is searched first, then the standard library directory. Directories
 -added with "-I" are searched after the current directory, in the order
 -in which they were given on the command line, but before the standard
 -library directory. See also option "-nostdlib".
 -
 -If the given directory starts with "+", it is taken relative to the
 -standard library directory.  For instance, "-I +labltk" adds the
 -subdirectory "labltk" of the standard library to the search path.
 -
 -\item["-impl" \var{filename}]
 -Compile the file \var{filename} as an implementation file, even if its
 -extension is not ".ml".
 -
 -\item["-inline" \var{n}]
 -Set aggressiveness of inlining to \var{n}, where \var{n} is a positive
 -integer. Specifying "-inline 0" prevents all functions from being
 -inlined, except those whose body is smaller than the call site. Thus,
 -inlining causes no expansion in code size. The default aggressiveness,
 -"-inline 1", allows slightly larger functions to be inlined, resulting
 -in a slight expansion in code size. Higher values for the "-inline"
 -option cause larger and larger functions to become candidate for
 -inlining, but can result in a serious increase in code size.
 -
 -\item["-intf" \var{filename}]
 -Compile the file \var{filename} as an interface file, even if its
 -extension is not ".mli".
 -
 -\item["-intf-suffix" \var{string}]
 -Recognize file names ending with \var{string} as interface files
 -(instead of the default ".mli").
 -
 -\item["-labels"]
 -Labels are not ignored in types, labels may be used in applications,
 -and labelled parameters can be given in any order.  This is the default.
 -
 -\item["-linkall"]
 -Force all modules contained in libraries to be linked in. If this
 -flag is not given, unreferenced modules are not linked in. When
 -building a library ("-a" flag), setting the "-linkall" flag forces all
 -subsequent links of programs involving that library to link all the
 -modules contained in the library.
 -
 -\item["-no-app-funct"]
 -Deactivates the applicative behaviour of functors. With this option,
 -each functor application generates new types in its result and
 -applying the same functor twice to the same argument yields two
 -incompatible structures.
 -
 -\item["-noassert"]
 -Do not compile assertion checks.  Note that the special form
 -"assert false" is always compiled because it is typed specially.
 -This flag has no effect when linking already-compiled files.
 -
 -\item["-noautolink"]
 -When linking ".cmxa" libraries, ignore "-cclib" and "-ccopt"
 -options potentially contained in the libraries (if these options were
 -given when building the libraries).  This can be useful if a library
 -contains incorrect specifications of C libraries or C options; in this
 -case, during linking, set "-noautolink" and pass the correct C
 -libraries and options on the command line.
 -
 -\item["-nodynlink"]
 -Allow the compiler to use some optimizations that are valid only for code
 -that is never dynlinked.
 -
 -\item["-nolabels"]
 -Ignore non-optional labels in types. Labels cannot be used in
 -applications, and parameter order becomes strict.
 -
 -\item["-nostdlib"]
 -Do not automatically add the standard library directory the list of
 -directories searched for compiled interface files (".cmi"), compiled
 -object code files (".cmx"), and libraries (".cmxa"). See also option
 -"-I".
 -
 -\item["-o" \var{exec-file}]
 -Specify the name of the output file produced by the linker. The
 -default output name is "a.out" under Unix and "camlprog.exe" under
 -Windows. If the "-a" option is given, specify the name of the library
 -produced.  If the "-pack" option is given, specify the name of the
 -packed object file produced.  If the "-output-obj" option is given,
 -specify the name of the output file produced. If the "-shared" option
 -is given, specify the name of plugin file produced.
 -
 -\item["-output-obj"]
 -Cause the linker to produce a C object file instead of an executable
 -file. This is useful to wrap OCaml code as a C library,
 -callable from any C program. See chapter~\ref{c:intf-c},
 -section~\ref{s:embedded-code}. The name of the output object file
 -must be set with the "-o" option.
 -This option can also be used to produce a compiled shared/dynamic
 -library (".so" extension, ".dll" under Windows).
 -
 -\item["-p"]
 -Generate extra code to write profile information when the program is
 -executed.  The profile information can then be examined with the
 -analysis program "gprof".  (See chapter~\ref{c:profiler} for more
 -information on profiling.)  The "-p" option must be given both at
 -compile-time and at link-time.  Linking object files not compiled with
 -"-p" is possible, but results in less precise profiling.
 -
 -\begin{unix} See the Unix manual page for "gprof(1)" for more
 -information about the profiles.
 -
 -Full support for "gprof" is only available for certain platforms
 -(currently: Intel x86 32 and 64 bits under Linux, BSD and MacOS X).
 -On other platforms, the "-p" option will result in a less precise
 -profile (no call graph information, only a time profile).
 -\end{unix}
 -
 -\begin{windows}
 -The "-p" option does not work under Windows.
 -\end{windows}
 -
 -\item["-pack"]
 -Build an object file (".cmx" and ".o"/".obj" files) and its associated compiled
 -interface (".cmi") that combines the ".cmx" object
 -files given on the command line, making them appear as sub-modules of
 -the output ".cmx" file.  The name of the output ".cmx" file must be
 -given with the "-o" option.  For instance,
 -\begin{verbatim}
 -        ocamlopt -pack -o P.cmx A.cmx B.cmx C.cmx
 -\end{verbatim}
 -generates compiled files "P.cmx", "P.o" and "P.cmi" describing a
 -compilation unit having three sub-modules "A", "B" and "C",
 -corresponding to the contents of the object files "A.cmx", "B.cmx" and
 -"C.cmx".  These contents can be referenced as "P.A", "P.B" and "P.C"
 -in the remainder of the program.
 -
 -The ".cmx" object files being combined must have been compiled with
 -the appropriate "-for-pack" option.  In the example above,
 -"A.cmx", "B.cmx" and "C.cmx" must have been compiled with
 -"ocamlopt -for-pack P".
 -
 -Multiple levels of packing can be achieved by combining "-pack" with
 -"-for-pack".  Consider the following example:
 -\begin{verbatim}
 -        ocamlopt -for-pack P.Q -c A.ml
 -        ocamlopt -pack -o Q.cmx -for-pack P A.cmx
 -        ocamlopt -for-pack P -c B.ml
 -        ocamlopt -pack -o P.cmx Q.cmx B.cmx
 -\end{verbatim}
 -The resulting "P.cmx" object file has sub-modules "P.Q", "P.Q.A"
 -and "P.B".
 -
 -\item["-plugin" \var{plugin}]
 -Dynamically load the code of the given \var{plugin}
 -(a ".cmo", ".cma" or ".cmxs" file) in the compiler. \var{plugin} must exist in
 -the same kind of code as the compiler ("ocamlopt.byte" must load bytecode
 -plugins, while "ocamlopt.opt" must load native code plugins), and
 -extension adaptation is done automatically for ".cma" files (to ".cmxs" files
 -if the compiler is compiled in native code).
 -
 -\item["-pp" \var{command}]
 -Cause the compiler to call the given \var{command} as a preprocessor
 -for each source file. The output of \var{command} is redirected to
 -an intermediate file, which is compiled. If there are no compilation
 -errors, the intermediate file is deleted afterwards.
 -
 -\item["-ppx" \var{command}]
 -After parsing, pipe the abstract syntax tree through the preprocessor
 -\var{command}. The module "Ast_mapper", described in
 -chapter~\ref{Ast-underscoremapper}, implements the external interface
 -of a preprocessor.
 -
 -\item["-principal"]
 -Check information path during type-checking, to make sure that all
 -types are derived in a principal way. All programs accepted in
 -"-principal" mode are also accepted in default mode with equivalent
 -types, but different binary signatures.
 -
 -\item["-rectypes"]
 -Allow arbitrary recursive types during type-checking.  By default,
 -only recursive types where the recursion goes through an object type
 -are supported. Note that once you have created an interface using this
 -flag, you must use it again for all dependencies.
 -
 -\item["-runtime-variant" \var{suffix}]
 -Add the \var{suffix} string to the name of the runtime library used by
 -the program.  Currently, only one such suffix is supported: "d", and
 -only if the OCaml compiler was configured with option
 -"-with-debug-runtime".  This suffix gives the debug version of the
 -runtime, which is useful for debugging pointer problems in low-level
 -code such as C stubs.
 -
 -\item["-S"]
 -Keep the assembly code produced during the compilation. The assembly
 -code for the source file \var{x}".ml" is saved in the file \var{x}".s".
 -
 -\item["-shared"]
 -Build a plugin (usually ".cmxs") that can be dynamically loaded with
 -the "Dynlink" module. The name of the plugin must be
 -set with the "-o" option. A plugin can include a number of OCaml
 -modules and libraries, and extra native objects (".o", ".obj", ".a",
 -".lib" files). Building native plugins is only supported for some
 -operating system. Under some systems (currently,
 -only Linux AMD 64), all the OCaml code linked in a plugin must have
 -been compiled without the "-nodynlink" flag. Some constraints might also
 -apply to the way the extra native objects have been compiled (under
 -Linux AMD 64, they must contain only position-independent code).
 -
 -\item["-safe-string"]
 -Enforce the separation between types "string" and "bytes",
 -thereby making strings read-only. This will become the default in
 -a future version of OCaml.
 -
 -\item["-short-paths"]
 -When a type is visible under several module-paths, use the shortest
 -one when printing the type's name in inferred interfaces and error and
 -warning messages. Identifier names starting with an underscore "_" or
 -containing double underscores "__" incur a penalty of $+10$ when computing
 -their length.
 -
 -\item["-strict-sequence"]
 -Force the left-hand part of each sequence to have type unit.
 -
 -\item["-strict-formats"]
 -Reject invalid formats that were accepted in legacy format
 -implementations. You should use this flag to detect and fix such
 -invalid formats, as they will be rejected by future OCaml versions.
 -
 -\item["-thread"]
 -Compile or link multithreaded programs, in combination with the
 -system "threads" library described in chapter~\ref{c:threads}.
 -
 -\item["-unboxed-types"]
 -When a type is unboxable (i.e. a record with a single argument or a
 -concrete datatype with a single constructor of one argument) it will
 -be unboxed unless annotated with "[@@ocaml.boxed]".
 -
 -\item["-no-unboxed-types"]
 -When a type is unboxable  it will be boxed unless annotated with
 -"[@@ocaml.unboxed]". This is the default.
 -
 -\item["-unsafe"]
 -Turn bound checking off for array and string accesses (the "v.(i)" and
 -"s.[i]" constructs). Programs compiled with "-unsafe" are therefore
 -faster, but unsafe: anything can happen if the program accesses an
 -array or string outside of its bounds.  Additionally, turn off the
 -check for zero divisor in integer division and modulus operations.
 -With "-unsafe", an integer division (or modulus) by zero can halt the
 -program or continue with an unspecified result instead of raising a
 -"Division_by_zero" exception.
 -
 -\item["-unsafe-string"]
 -Identify the types "string" and "bytes",
 -thereby making strings writable. For reasons of backward compatibility,
 -this is the default setting for the moment, but this will change in a future
 -version of OCaml.
 -
 -\item["-v"]
 -Print the version number of the compiler and the location of the
 -standard library directory, then exit.
 -
 -\item["-verbose"]
 -Print all external commands before they are executed, in particular
 -invocations of the assembler, C compiler, and linker.
 -
 -\item["-version" or "-vnum"]
 -Print the version number of the compiler in short form (e.g. "3.11.0"),
 -then exit.
 -
 -\item["-w" \var{warning-list}]
 -Enable, disable, or mark as fatal the warnings specified by the argument
 -\var{warning-list}.
 -Each warning can be {\em enabled} or {\em disabled}, and each warning
 -can be {\em fatal} or {\em non-fatal}.
 -If a warning is disabled, it isn't displayed and doesn't affect
 -compilation in any way (even if it is fatal).  If a warning is
 -enabled, it is displayed normally by the compiler whenever the source
 -code triggers it.  If it is enabled and fatal, the compiler will also
 -stop with an error after displaying it.
 -
 -The \var{warning-list} argument is a sequence of warning specifiers,
 -with no separators between them.  A warning specifier is one of the
 -following:
 -
 -\begin{options}
 -\item["+"\var{num}] Enable warning number \var{num}.
 -\item["-"\var{num}] Disable warning number \var{num}.
 -\item["@"\var{num}] Enable and mark as fatal warning number \var{num}.
 -\item["+"\var{num1}..\var{num2}] Enable warnings in the given range.
 -\item["-"\var{num1}..\var{num2}] Disable warnings in the given range.
 -\item["@"\var{num1}..\var{num2}] Enable and mark as fatal warnings in
 -the given range.
 -\item["+"\var{letter}] Enable the set of warnings corresponding to
 -\var{letter}. The letter may be uppercase or lowercase.
 -\item["-"\var{letter}] Disable the set of warnings corresponding to
 -\var{letter}. The letter may be uppercase or lowercase.
 -\item["@"\var{letter}] Enable and mark as fatal the set of warnings
 -corresponding to \var{letter}. The letter may be uppercase or
 -lowercase.
 -\item[\var{uppercase-letter}] Enable the set of warnings corresponding
 -to \var{uppercase-letter}.
 -\item[\var{lowercase-letter}] Disable the set of warnings corresponding
 -to \var{lowercase-letter}.
 -\end{options}
 -
 -Warning numbers and letters which are out of the range of warnings
 -that are currently defined are ignored. The warning are as follows.
 -\begin{options}
 -\input{warnings-help.tex}
 -\end{options}
 -
 -The default setting is "-w +a-4-6-7-9-27-29-32..39-41..42-44-45-48-50".
 -It is displayed by "ocamlopt -help".
 -Note that warnings 5 and 10 are not always triggered, depending on
 -the internals of the type checker.
 -
 -\item["-warn-error" \var{warning-list}]
 -Mark as fatal the warnings specified in the argument \var{warning-list}.
 -The compiler will stop with an error when one of these warnings is
 -emitted. The \var{warning-list} has the same meaning as for
 -the "-w" option: a "+" sign (or an uppercase letter) marks the
 -corresponding warnings as fatal, a "-"
 -sign (or a lowercase letter) turns them back into non-fatal warnings,
 -and a "@" sign both enables and marks as fatal the corresponding
 -warnings.
 -
 -Note: it is not recommended to use warning sets (i.e. letters) as
 -arguments to "-warn-error"
 -in production code, because this can break your build when future versions
 -of OCaml add some new warnings.
 -
 -The default setting is "-warn-error -a+31" (only warning 31 is fatal).
 -
 -\item["-warn-help"]
 -Show the description of all available warning numbers.
 -
 -\item["-where"]
 -Print the location of the standard library, then exit.
 -
 -\item["-" \var{file}]
 -Process \var{file} as a file name, even if it starts with a dash (-)
 -character.
 -
 -\item["-help" or "--help"]
 -Display a short usage summary and exit.
 -%
 -\end{options}
 +% Configure boolean variables used by the macros in unified-options.etex
 +\compfalse
 +\nattrue
 +\topfalse
 +% unified-options gathers all options across the native/bytecode
 +% compilers and toplevel
 +\input{unified-options.tex}
  
- \noindent
- On native Windows, the following environment variable is also consulted:
- 
- \begin{options}
- \item["OCAML_FLEXLINK"]  Alternative executable to use instead of the
- configured value. Primarily used for bootstrapping.
- \end{options}
- 
  \paragraph{Options for the IA32 architecture}
  The IA32 code generator (Intel Pentium, AMD Athlon) supports the
  following additional option:
diff --cc stdlib/bytes.ml
index ce3b9dc,24e97cc..1331332
--- a/stdlib/bytes.ml
+++ b/stdlib/bytes.ml
@@@ -224,24 -233,16 +233,30 @@@ let rec index_rec s lim i c 
    if i >= lim then raise Not_found else
    if unsafe_get s i = c then i else index_rec s lim (i + 1) c
  
+ (* duplicated in string.ml *)
  let index s c = index_rec s (length s) 0 c
  
+ (* duplicated in string.ml *)
 +let rec index_rec_opt s lim i c =
 +  if i >= lim then None else
 +  if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c
 +
++(* duplicated in string.ml *)
 +let index_opt s c = index_rec_opt s (length s) 0 c
 +
++(* duplicated in string.ml *)
  let index_from s i c =
    let l = length s in
    if i < 0 || i > l then invalid_arg "String.index_from / Bytes.index_from" else
    index_rec s l i c
  
+ (* duplicated in string.ml *)
 +let index_from_opt s i c =
 +  let l = length s in
 +  if i < 0 || i > l then invalid_arg "String.index_from_opt / Bytes.index_from_opt" else
 +  index_rec_opt s l i c
 +
++(* duplicated in string.ml *)
  let rec rindex_rec s i c =
    if i < 0 then raise Not_found else
    if unsafe_get s i = c then i else rindex_rec s (i - 1) c
@@@ -254,19 -257,8 +271,23 @@@ let rindex_from s i c 
    else
      rindex_rec s i c
  
++(* duplicated in string.ml *)
 +let rec rindex_rec_opt s i c =
 +  if i < 0 then None else
 +  if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c
 +
++(* duplicated in string.ml *)
 +let rindex_opt s c = rindex_rec_opt s (length s - 1) c
 +
++(* duplicated in string.ml *)
 +let rindex_from_opt s i c =
 +  if i < -1 || i >= length s then
 +    invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
 +  else
 +    rindex_rec_opt s i c
 +
  
+ (* duplicated in string.ml *)
  let contains_from s i c =
    let l = length s in
    if i < 0 || i > l then
diff --cc stdlib/string.ml
index 17da843,9c4a97f..1d38227
--- a/stdlib/string.ml
+++ b/stdlib/string.ml
@@@ -103,28 -113,52 +113,81 @@@ let escaped s 
    else
      s
  
- let index s c =
-   B.index (bos s) c
- let index_opt s c =
-   B.index_opt (bos s) c
- let rindex s c =
-   B.rindex (bos s) c
- let rindex_opt s c =
-   B.rindex_opt (bos s) c
- let index_from s i c=
-   B.index_from (bos s) i c
- let index_from_opt s i c=
-   B.index_from_opt (bos s) i c
+ (* duplicated in bytes.ml *)
+ let rec index_rec s lim i c =
+   if i >= lim then raise Not_found else
+   if unsafe_get s i = c then i else index_rec s lim (i + 1) c
+ 
+ (* duplicated in bytes.ml *)
+ let index s c = index_rec s (length s) 0 c
+ 
+ (* duplicated in bytes.ml *)
++let rec index_rec_opt s lim i c =
++  if i >= lim then None else
++  if unsafe_get s i = c then Some i else index_rec_opt s lim (i + 1) c
++
++(* duplicated in bytes.ml *)
++let index_opt s c = index_rec_opt s (length s) 0 c
++
++(* duplicated in bytes.ml *)
+ let index_from s i c =
+   let l = length s in
+   if i < 0 || i > l then invalid_arg "String.index_from / Bytes.index_from" else
+     index_rec s l i c
+ 
+ (* duplicated in bytes.ml *)
++let index_from_opt s i c =
++  let l = length s in
++  if i < 0 || i > l then invalid_arg "String.index_from_opt / Bytes.index_from_opt" else
++  index_rec_opt s l i c
++
++(* duplicated in bytes.ml *)
+ let rec rindex_rec s i c =
+   if i < 0 then raise Not_found else
+   if unsafe_get s i = c then i else rindex_rec s (i - 1) c
+ 
+ (* duplicated in bytes.ml *)
+ let rindex s c = rindex_rec s (length s - 1) c
+ 
+ (* duplicated in bytes.ml *)
  let rindex_from s i c =
-   B.rindex_from (bos s) i c
+   if i < -1 || i >= length s then
+     invalid_arg "String.rindex_from / Bytes.rindex_from"
+   else
+     rindex_rec s i c
+ 
+ (* duplicated in bytes.ml *)
++let rec rindex_rec_opt s i c =
++  if i < 0 then None else
++  if unsafe_get s i = c then Some i else rindex_rec_opt s (i - 1) c
++
++(* duplicated in bytes.ml *)
++let rindex_opt s c = rindex_rec_opt s (length s - 1) c
++
++(* duplicated in bytes.ml *)
 +let rindex_from_opt s i c =
-   B.rindex_from_opt (bos s) i c
- let contains s c =
-   B.contains (bos s) c
++  if i < -1 || i >= length s then
++    invalid_arg "String.rindex_from_opt / Bytes.rindex_from_opt"
++  else
++    rindex_rec_opt s i c
++
++(* duplicated in bytes.ml *)
  let contains_from s i c =
-   B.contains_from (bos s) i c
+   let l = length s in
+   if i < 0 || i > l then
+     invalid_arg "String.contains_from / Bytes.contains_from"
+   else
+     try ignore (index_rec s l i c); true with Not_found -> false
+ 
+ (* duplicated in bytes.ml *)
+ let contains s c = contains_from s 0 c
+ 
+ (* duplicated in bytes.ml *)
  let rcontains_from s i c =
-   B.rcontains_from (bos s) i c
+   if i < 0 || i >= length s then
+     invalid_arg "String.rcontains_from / Bytes.rcontains_from"
+   else
+     try ignore (rindex_rec s i c); true with Not_found -> false
  
  let uppercase_ascii s =
    B.uppercase_ascii (bos s) |> bts
diff --cc testsuite/tests/asmcomp/Makefile
index c3acf95,abcb872..c0a9633
--- a/testsuite/tests/asmcomp/Makefile
+++ b/testsuite/tests/asmcomp/Makefile
@@@ -53,15 -50,18 +53,20 @@@ lexcmm.ml: lexcmm.ml
  
  MLCASES=optargs staticalloc bind_tuples is_static register_typing \
    register_typing_switch
 +ARGS_optargs=-g
  ARGS_is_static=-I $(OTOPDIR)/byterun is_in_static_data.c
- MLCASES_FLAMBDA=is_static_flambda unrolling_flambda unrolling_flambda2
+ MLCASES_FLAMBDA=is_static_flambda unrolling_flambda unrolling_flambda2 \
+   static_float_array_flambda static_float_array_flambda_opaque
  ARGS_is_static_flambda=\
    -I $(OTOPDIR)/byterun is_in_static_data.c is_static_flambda_dep.ml
+ ARGS_static_float_array_flambda=\
+   -I $(OTOPDIR)/byterun is_in_static_data.c simple_float_const.ml
+ ARGS_static_float_array_flambda_opaque=\
+   -I $(OTOPDIR)/byterun is_in_static_data.c -opaque simple_float_const_opaque.ml
  
  CASES=fib tak quicksort quicksort2 soli \
 -      arith checkbound tagged-fib tagged-integr tagged-quicksort tagged-tak
 +      arith checkbound tagged-fib tagged-integr tagged-quicksort tagged-tak \
 +      catch-try catch-rec even-odd even-odd-spill pgcd
  ARGS_fib=-DINT_INT -DFUN=fib main.c
  ARGS_tak=-DUNIT_INT -DFUN=takmain main.c
  ARGS_quicksort=-DSORT -DFUN=quicksort main.c
@@@ -74,14 -74,10 +79,15 @@@ ARGS_tagged-fib=-DINT_INT -DFUN=fib mai
  ARGS_tagged-integr=-DINT_FLOAT -DFUN=test main.c
  ARGS_tagged-quicksort=-DSORT -DFUN=quicksort main.c
  ARGS_tagged-tak=-DUNIT_INT -DFUN=takmain main.c
 +ARGS_catch-try=-DINT_INT -DFUN=catch_exit main.c
 +ARGS_catch-rec=-DINT_INT -DFUN=catch_fact main.c
 +ARGS_even-odd=-DINT_INT -DFUN=is_even main.c
 +ARGS_even-odd-spill=-DINT_INT -DFUN=is_even main.c
 +ARGS_pgcd=-DINT_INT -DFUN=pgcd_30030 main.c
+ ARGS_staticalloc=-I $(OTOPDIR)/utils config.cmx
  
  skips:
- 	@for c in $(CASES) $(MLCASES); do \
+ 	@for c in $(CASES) $(MLCASES) $(MLCASES_FLAMBDA); do \
  	  echo " ... testing '$$c': => skipped"; \
  	done
  
diff --cc tools/Makefile
index a58c526,7ab2f11..1c995e4
--- a/tools/Makefile
+++ b/tools/Makefile
@@@ -13,378 -13,4 +13,379 @@@
  #*                                                                        *
  #**************************************************************************
  
 -include Makefile.shared
 +MAKEFLAGS := -r -R
 +include ../config/Makefile
 +INSTALL_BINDIR:=$(DESTDIR)$(BINDIR)
 +INSTALL_LIBDIR:=$(DESTDIR)$(LIBDIR)
 +INSTALL_COMPLIBDIR:=$(DESTDIR)$(COMPLIBDIR)
 +INSTALL_STUBLIBDIR:=$(DESTDIR)$(STUBLIBDIR)
 +INSTALL_MANDIR:=$(DESTDIR)$(MANDIR)
 +
 +ifeq ($(SYSTEM),unix)
 +override define shellquote
 +$i := $$(subst ",\",$$(subst $$$$,\$$$$,$$(subst `,\`,$i)))#")#
 +endef
 +$(foreach i,BINDIR LIBDIR COMPLIBDIR STUBLIBDIR MANDIR,$(eval $(shellquote)))
 +endif
 +
 +CAMLRUN ?= ../boot/ocamlrun
 +CAMLYACC ?= ../boot/ocamlyacc
 +DESTDIR ?=
 +# Setup GNU make variables storing per-target source and target,
 +# a list of installed tools, and a function to quote a filename for
 +# the shell.
 +override installed_tools := ocamldep ocamlprof ocamlcp ocamloptp \
 +                   ocamlmktop ocamlmklib ocamlobjinfo
 +
 +install_files :=
 +define byte2native
 +$(patsubst %.cmo,%.cmx,$(patsubst %.cma,%.cmxa,$1))
 +endef
 +
 +# $1 = target, $2 = OCaml object dependencies, $3 = other dependencies
 +# There is a lot of subtle code here.  The multiple layers of expansion
 +# are due to `make`'s eval() function, which evaluates the string
 +# passed to it as a makefile fragment.  So it is crucial that variables
 +# not get expanded too many times.
 +define byte_and_opt_
 +# This check is defensive programming
 +$(and $(filter-out 1,$(words $1)),$(error \
 +   cannot build file with whitespace in name))
 +$1: $3 $2
 +	$$(CAMLC) $$(LINKFLAGS) -I .. -o $$@ $2
 +
 +$1.opt: $3 $$(call byte2native,$2)
 +	$$(CAMLOPT) $$(LINKFLAGS) -I .. -o $$@ $$(call byte2native,$2)
 +
 +all: $1
 +
 +opt.opt: $1.opt
 +
 +ifeq '$(filter $(installed_tools),$1)' '$1'
 +install_files += $1
 +endif
 +clean::
 +	rm -f -- $1 $1.opt
 +
 +endef
 +
 +# Escape any $ characters in the arguments and eval the result.
 +define byte_and_opt
 +$(eval $(call \
 + byte_and_opt_,$(subst $$,$$$$,$1),$(subst $$,$$$$,$2),$(subst $$,$$$$,$3)))
 +endef
 +
 +ROOTDIR=..
 +
 +ifeq "$(wildcard $(ROOTDIR)/flexdll/Makefile)" ""
 +export OCAML_FLEXLINK:=
 +else
 +export OCAML_FLEXLINK:=$(ROOTDIR)/boot/ocamlrun $(ROOTDIR)/flexdll/flexlink.exe
 +endif
 +
 +CAMLC=$(CAMLRUN) ../boot/ocamlc -nostdlib -I ../boot \
 +      -use-prims ../byterun/primitives -I ..
 +CAMLOPT=$(CAMLRUN) ../ocamlopt -nostdlib -I ../stdlib
 +ifeq "$(UNIX_OR_WIN32)" "win32"
 +  ifneq "$(wildcard ../flexdll/Makefile)" ""
 +    CAMLOPT := OCAML_FLEXLINK="../boot/ocamlrun ../flexdll/flexlink.exe" \
 +      $(CAMLOPT)
 +  endif
 +endif
 +CAMLLEX=$(CAMLRUN) ../boot/ocamllex
 +INCLUDES=-I ../utils -I ../parsing -I ../typing -I ../bytecomp -I ../asmcomp \
 +         -I ../middle_end -I ../middle_end/base_types -I ../driver \
 +         -I ../toplevel
 +COMPFLAGS= -absname -w +a-4-9-41-42-44-45-48 -strict-sequence -warn-error A \
 + -safe-string -strict-formats -bin-annot $(INCLUDES)
 +LINKFLAGS=$(INCLUDES)
 +VPATH := $(filter-out -I,$(INCLUDES))
 +
 +# scrapelabels addlabels
 +
 +.PHONY: all opt.opt
 +
 +# The dependency generator
 +
 +CAMLDEP_OBJ=ocamldep.cmo
 +CAMLDEP_IMPORTS=timings.cmo misc.cmo config.cmo identifiable.cmo numbers.cmo \
 +  arg_helper.cmo clflags.cmo terminfo.cmo \
 +  warnings.cmo location.cmo longident.cmo docstrings.cmo \
 +  syntaxerr.cmo ast_helper.cmo parser.cmo lexer.cmo parse.cmo \
 +  ccomp.cmo ast_mapper.cmo ast_iterator.cmo \
 +  builtin_attributes.cmo ast_invariants.cmo \
 +  pparse.cmo compenv.cmo depend.cmo
 +
 +ocamldep: LINKFLAGS += -compat-32
 +$(call byte_and_opt,ocamldep,$(CAMLDEP_IMPORTS) $(CAMLDEP_OBJ),)
 +ocamldep: depend.cmi
 +ocamldep.opt: depend.cmi
 +
 +# ocamldep is precious: sometimes we are stuck in the middle of a
 +# bootstrap and we need to remake the dependencies
 +clean::
 +	if test -f ocamldep; then mv -f ocamldep ocamldep.bak; else :; fi
 +	rm -f ocamldep.opt
 +
 +
 +# The profiler
 +
 +CSLPROF=ocamlprof.cmo
 +CSLPROF_IMPORTS=misc.cmo config.cmo identifiable.cmo numbers.cmo \
 +  arg_helper.cmo clflags.cmo terminfo.cmo \
 +  warnings.cmo location.cmo longident.cmo docstrings.cmo \
 +  syntaxerr.cmo ast_helper.cmo parser.cmo lexer.cmo parse.cmo
 +
 +$(call byte_and_opt,ocamlprof,$(CSLPROF_IMPORTS) profiling.cmo $(CSLPROF),)
 +
 +ocamlcp_cmos = misc.cmo warnings.cmo config.cmo identifiable.cmo numbers.cmo \
 +	       arg_helper.cmo clflags.cmo main_args.cmo
 +
 +$(call byte_and_opt,ocamlcp,$(ocamlcp_cmos) ocamlcp.cmo,)
 +$(call byte_and_opt,ocamloptp,$(ocamlcp_cmos) ocamloptp.cmo,)
 +
 +opt:: profiling.cmx
 +
 +install::
 +	cp -- profiling.cmi profiling.cmo profiling.cmt profiling.cmti "$(INSTALL_LIBDIR)"
 +
 +installopt::
 +	cp -- profiling.cmx profiling.$(O) "$(INSTALL_LIBDIR)"
 +
 +# To help building mixed-mode libraries (OCaml + C)
 +
 +$(call byte_and_opt,ocamlmklib,ocamlmklibconfig.cmo config.cmo \
 +	         ocamlmklib.cmo,)
 +
 +
 +ocamlmklibconfig.ml: ../config/Makefile Makefile
 +	(echo 'let bindir = "$(BINDIR)"'; \
 +         echo 'let supports_shared_libraries = $(SUPPORTS_SHARED_LIBRARIES)';\
 +         echo 'let byteccrpath = "$(BYTECCRPATH)"'; \
 +         echo 'let nativeccrpath = "$(NATIVECCRPATH)"'; \
 +         echo 'let mksharedlibrpath = "$(MKSHAREDLIBRPATH)"'; \
 +         echo 'let toolpref = "$(TOOLPREF)"'; \
 +         sed -n -e 's/^#ml //p' ../config/Makefile) \
 +        > ocamlmklibconfig.ml
 +
 +beforedepend:: ocamlmklibconfig.ml
 +
 +clean::
 +	rm -f ocamlmklibconfig.ml
 +
 +# To make custom toplevels
 +
 +OCAMLMKTOP=ocamlmktop.cmo
 +OCAMLMKTOP_IMPORTS=misc.cmo identifiable.cmo numbers.cmo config.cmo \
 +		   arg_helper.cmo clflags.cmo ccomp.cmo
 +
 +$(call byte_and_opt,ocamlmktop,$(OCAMLMKTOP_IMPORTS) $(OCAMLMKTOP),)
 +
 +# Converter olabl/ocaml 2.99 to ocaml 3
 +
 +OCAML299TO3= lexer299.cmo ocaml299to3.cmo
 +LIBRARY3= misc.cmo warnings.cmo location.cmo
 +
 +ocaml299to3: $(OCAML299TO3)
 +	$(CAMLC) $(LINKFLAGS) -o ocaml299to3 $(LIBRARY3) $(OCAML299TO3)
 +
 +lexer299.ml: lexer299.mll
 +	$(CAMLLEX) lexer299.mll
 +
 +#install::
 +#	cp ocaml299to3 "$(INSTALL_BINDIR)/ocaml299to3$(EXE)"
 +
 +clean::
 +	rm -f ocaml299to3 lexer299.ml
 +
 +# Label remover for interface files (upgrade 3.02 to 3.03)
 +
 +SCRAPELABELS= lexer301.cmo scrapelabels.cmo
 +
 +scrapelabels: $(SCRAPELABELS)
 +	$(CAMLC) $(LINKFLAGS) -o scrapelabels $(LIBRARY3) $(SCRAPELABELS)
 +
 +lexer301.ml: lexer301.mll
 +	$(CAMLLEX) lexer301.mll
 +
 +#install::
 +#	cp scrapelabels "$(INSTALL_LIBDIR)"
 +
 +clean::
 +	rm -f scrapelabels lexer301.ml
 +
 +# Insert labels following an interface file (upgrade 3.02 to 3.03)
 +
 +ADDLABELS_IMPORTS=misc.cmo config.cmo arg_helper.cmo clflags.cmo \
 +  identifiable.cmo numbers.cmo terminfo.cmo \
 +  warnings.cmo location.cmo longident.cmo docstrings.cmo \
 +  syntaxerr.cmo ast_helper.cmo parser.cmo lexer.cmo parse.cmo
 +
 +addlabels: addlabels.cmo
 +	$(CAMLC) $(LINKFLAGS) -w sl -o addlabels \
 +		$(ADDLABELS_IMPORTS) addlabels.cmo
 +
 +#install::
 +#	cp addlabels "$(INSTALL_LIBDIR)"
 +
 +ifeq ($(UNIX_OR_WIN32),unix)
 +LN := ln -sf
 +else
 +LN := cp -pf
 +endif
 +
 +install::
 +	for i in $(install_files); \
 +	do \
 +	  cp -- "$$i" "$(INSTALL_BINDIR)/$$i.byte$(EXE)" && \
 +	  if test -f "$$i".opt; then \
 +	    cp -- "$$i.opt" "$(INSTALL_BINDIR)/$$i.opt$(EXE)" && \
 +	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.opt$(EXE)" "$$i$(EXE)"); \
 +	  else \
 +	    (cd "$(INSTALL_BINDIR)/" && $(LN) "$$i.byte$(EXE)" "$$i$(EXE)"); \
 +	  fi; \
 +	done
 +
 +clean::
 +	rm -f addlabels
 +
 +# The preprocessor for asm generators
 +
 +CVT_EMIT=cvt_emit.cmo
 +
 +cvt_emit: $(CVT_EMIT)
 +	$(CAMLC) $(LINKFLAGS) -o cvt_emit $(CVT_EMIT)
 +
 +# cvt_emit is precious: sometimes we are stuck in the middle of a
 +# bootstrap and we need to remake the dependencies
 +.PRECIOUS: cvt_emit
 +clean::
 +	if test -f cvt_emit; then mv -f cvt_emit cvt_emit.bak; else :; fi
 +
 +cvt_emit.ml: cvt_emit.mll
 +	$(CAMLLEX) cvt_emit.mll
 +
 +clean::
 +	rm -f cvt_emit.ml
 +
 +beforedepend:: cvt_emit.ml
 +
 +# Reading cmt files
 +
 +READ_CMT= \
 +          ../compilerlibs/ocamlcommon.cma \
 +          ../compilerlibs/ocamlbytecomp.cma \
 +          \
 +          cmt2annot.cmo read_cmt.cmo
 +
 +# Reading cmt files
 +$(call byte_and_opt,read_cmt,$(READ_CMT),)
 +
 +
 +# The bytecode disassembler
 +
 +DUMPOBJ=opnames.cmo dumpobj.cmo
 +
 +$(call byte_and_opt,dumpobj,misc.cmo identifiable.cmo numbers.cmo tbl.cmo \
 +                    config.cmo ident.cmo opcodes.cmo bytesections.cmo \
 +		    $(DUMPOBJ),)
 +
 +opnames.ml: ../byterun/caml/instruct.h
 +	unset LC_ALL || : ; \
 +	unset LC_CTYPE || : ; \
 +	unset LC_COLLATE LANG || : ; \
 +	sed -e '/[/][*]/d' \
 +	    -e '/^#/d' \
 +	    -e 's/enum \(.*\) {/let names_of_\1 = [|/' \
 +	    -e 's/.*};$$/ |]/' \
 +	    -e 's/\([A-Z][A-Z_0-9a-z]*\)/"\1"/g' \
 +	    -e 's/,/;/g' \
 +	../byterun/caml/instruct.h > opnames.ml
 +
 +clean::
 +	rm -f opnames.ml
 +
 +beforedepend:: opnames.ml
 +
 +# Display info on compiled files
 +
 +ifeq "$(SYSTEM)" "macosx"
 +DEF_SYMBOL_PREFIX = '-Dsymbol_prefix="_"'
 +else
 +DEF_SYMBOL_PREFIX = '-Dsymbol_prefix=""'
 +endif
 +
 +ifeq "$(CCOMPTYPE)" "msvc"
 +CCOUT = -Fe
 +else
 +EMPTY =
 +CCOUT = -o $(EMPTY)
 +endif
 +
 +objinfo_helper$(EXE): objinfo_helper.c ../config/s.h
 +	$(BYTECC) $(CCOUT)objinfo_helper$(EXE) $(BYTECCCOMPOPTS) \
 +          $(DEF_SYMBOL_PREFIX) $(LIBBFD_INCLUDE) objinfo_helper.c $(LIBBFD_LINK)
 +
 +OBJINFO=../compilerlibs/ocamlcommon.cma \
 +        ../compilerlibs/ocamlbytecomp.cma \
 +        ../compilerlibs/ocamlmiddleend.cma \
 +        ../asmcomp/printclambda.cmo \
 +        ../asmcomp/export_info.cmo \
 +        objinfo.cmo
 +
 +$(call byte_and_opt,ocamlobjinfo,$(OBJINFO),objinfo_helper$(EXE))
 +
 +install::
 +	cp objinfo_helper$(EXE) "$(INSTALL_LIBDIR)/objinfo_helper$(EXE)"
 +
 +# Scan object files for required primitives
 +$(call byte_and_opt,primreq,config.cmo primreq.cmo,)
 +
 +clean::
 +	rm -f "objinfo_helper$(EXE)"
++	rm -f "objinfo_helper$(EXE).manifest"
 +
 +
 +# Copy a bytecode executable, stripping debug info
 +
 +stripdebug=../compilerlibs/ocamlcommon.cma \
 +           ../compilerlibs/ocamlbytecomp.cma \
 +           stripdebug.cmo
 +
 +$(call byte_and_opt,stripdebug,$(stripdebug),)
 +
 +# Compare two bytecode executables
 +
 +CMPBYT=../compilerlibs/ocamlcommon.cma \
 +       ../compilerlibs/ocamlbytecomp.cma \
 +       cmpbyt.cmo
 +
 +$(call byte_and_opt,cmpbyt,$(CMPBYT),)
 +
 +ifeq "$(RUNTIMEI)" "true"
 +install::
 +	cp ocaml-instr-graph ocaml-instr-report "$(INSTALL_BINDIR)/"
 +endif
 +
 +# Common stuff
 +
 +.SUFFIXES:
 +
 +%.cmo: %.ml
 +	$(CAMLC) -c $(COMPFLAGS) - $<
 +
 +%.cmi: %.mli
 +	$(CAMLC) -c $(COMPFLAGS) - $<
 +
 +%.cmx: %.ml
 +	$(CAMLOPT) $(COMPFLAGS) -c - $<
 +
 +clean::
 +	rm -f *.cmo *.cmi *.cma *.dll *.so *.lib *.a
 +
 +depend: beforedepend
 +	$(CAMLRUN) ./ocamldep -slash $(INCLUDES) *.mli *.ml > .depend
 +
 +.PHONY: clean install beforedepend depend
 +
 +include .depend
diff --cc tools/dumpobj.ml
index 1416090,e4c8186..c3d60bf
--- a/tools/dumpobj.ml
+++ b/tools/dumpobj.ml
@@@ -543,12 -546,7 +546,13 @@@ let dump_exe ic 
  
  let arg_list = [
    "-noloc", Arg.Clear print_locations, " : don't print source information";
+   "-reloc", Arg.Set print_reloc_info, " : print relocation information";
 +  "-args", Arg.Expand Arg.read_arg,
 +     "<file> Read additional newline separated command line arguments \n\
 +     \      from <file>";
 +  "-args0", Arg.Expand Arg.read_arg0,
 +     "<file> Read additional NUL separated command line arguments from \n\
 +     \      <file>";
  ]
  let arg_usage =
    Printf.sprintf "%s [OPTIONS] FILES : dump content of bytecode files"
diff --cc utils/config.mlp
index da78871,e821ef0..8c7fa94
--- a/utils/config.mlp
+++ b/utils/config.mlp
@@@ -74,10 -84,10 +74,10 @@@ and cmxa_magic_number 
      "Caml1999Z015"
    else
      "Caml1999Z014"
- and ast_impl_magic_number = "Caml1999M019"
+ and ast_impl_magic_number = "Caml1999M020"
  and ast_intf_magic_number = "Caml1999N018"
  and cmxs_magic_number = "Caml2007D002"
 -and cmt_magic_number = "Caml2012T008"
 +and cmt_magic_number = "Caml2012T009"
  
  let load_path = ref ([] : string list)
  
```