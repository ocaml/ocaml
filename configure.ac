#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Sebastien Hinderer, projet Gallium, INRIA Paris             *
#*                                                                        *
#*   Copyright 2018 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# Process this file with autoconf to produce a configure script.

# Require Autoconf 2.71 for repeatability in CI
AC_PREREQ([2.71])
AC_INIT([OCaml],
        [OCAML__VERSION],
        [caml-list@inria.fr],
        [ocaml],
        [http://www.ocaml.org])

AC_MSG_NOTICE([Configuring OCaml version AC_PACKAGE_VERSION])

# Configuration variables

## Command-line arguments passed to configure
CONFIGURE_ARGS="$*"

# Command to build executables
# Ultimately, MKEXE may build an executable using the C compiler or it may use
# a wrapper for a linker (i.e. flexlink). The build system must therefore not
# rely on $CFLAGS because these cannot be processed by flexlink (and are not
# passed)
mkexe_cmd='$(CC)'
mkexe_extra_flags=''
mkexe_via_cc_extra_cmd=''
mkexe_ldflags_prefix=''
mkexe_via_cc_ldflags_prefix=''

# Flags for building executable files with debugging symbols
mkexedebugflag="-g"
common_cflags=""
common_cppflags=""
internal_cflags=""
internal_cppflags=""
bytecode_cflags=""
bytecode_cppflags=""
oc_ldflags=""
oc_dll_ldflags=""
oc_exe_ldflags=""

tsan=false
# TSan flags for each compilation stage
tsan_cppflags="-DWITH_THREAD_SANITIZER"
tsan_cflags="-fsanitize=thread"
tsan_distinguish_volatile_cflags=""
tsan_ldflags="-fsanitize=thread"

# The C# compiler and its flags
CSC=""
CSCFLAGS=""

ostype="Unix"
SO="so"
toolchain="cc"
reserved_header_bits=0
instrumented_runtime=false
instrumented_runtime_libs=""
bootstrapping_flexdll=false
flexdll_dir=
lib_dynlink=false
lib_str=false
lib_systhreads=false
lib_unix=false
optional_libraries=''
optional_bytecode_tools=''
optional_native_tools=''
ocamltest_libunix=None
ocamltest_unix_impl="dummy"
unix_library=""
unix_directory=""

# Information about the package

## Source directory
AC_CONFIG_SRCDIR([runtime/interp.c])

## Directory containing auxiliary scripts used during build
AC_CONFIG_AUX_DIR([build-aux])

## Output variables

AC_SUBST([CONFIGURE_ARGS])
AC_SUBST([native_compiler])
AC_SUBST([default_build_target])
AC_SUBST([VERSION], [AC_PACKAGE_VERSION])
AC_SUBST([OCAML_DEVELOPMENT_VERSION], [OCAML__DEVELOPMENT_VERSION])
AC_SUBST([OCAML_RELEASE_EXTRA], [OCAML__RELEASE_EXTRA])
AC_SUBST([OCAML_VERSION_MAJOR], [OCAML__VERSION_MAJOR])
AC_SUBST([OCAML_VERSION_MINOR], [OCAML__VERSION_MINOR])
AC_SUBST([OCAML_VERSION_PATCHLEVEL], [OCAML__VERSION_PATCHLEVEL])
AC_SUBST([OCAML_VERSION_EXTRA], [OCAML__VERSION_EXTRA])
AC_SUBST([OCAML_VERSION_SHORT], [OCAML__VERSION_SHORT])
AC_DEFINE([MAGIC_NUMBER_PREFIX], ["][MAGIC_NUMBER__PREFIX]["])
AC_DEFINE([MAGIC_NUMBER_VERSION], ["][MAGIC_NUMBER__VERSION]["])
AC_DEFINE([EXEC_MAGIC_LENGTH], [MAGIC_NUMBER__LENGTH])
AC_SUBST([MAGIC_LENGTH], [MAGIC_NUMBER__LENGTH])
AC_DEFINE([EXEC_FORMAT], ["][EXEC__FORMAT]["])
AC_SUBST([EXEC_MAGIC_NUMBER], [EXEC__MAGIC_NUMBER])
AC_SUBST([CMI_MAGIC_NUMBER], [CMI__MAGIC_NUMBER])
AC_SUBST([CMO_MAGIC_NUMBER], [CMO__MAGIC_NUMBER])
AC_SUBST([CMA_MAGIC_NUMBER], [CMA__MAGIC_NUMBER])
AC_SUBST([CMX_MAGIC_NUMBER])
AC_SUBST([CMXA_MAGIC_NUMBER])
AC_SUBST([AST_IMPL_MAGIC_NUMBER], [AST_IMPL__MAGIC_NUMBER])
AC_SUBST([AST_INTF_MAGIC_NUMBER], [AST_INTF__MAGIC_NUMBER])
AC_SUBST([CMXS_MAGIC_NUMBER], [CMXS__MAGIC_NUMBER])
AC_SUBST([CMT_MAGIC_NUMBER], [CMT__MAGIC_NUMBER])
AC_SUBST([LINEAR_MAGIC_NUMBER], [LINEAR__MAGIC_NUMBER])
AC_SUBST([CC])
AC_SUBST([DIFF_FLAGS])
AC_SUBST([CSC])
AC_SUBST([CSCFLAGS])
# Note: This is present for the flexdll bootstrap where it exposed as the old
# TOOLPREF variable. It would be better if flexdll where updated to require
# WINDRES instead.
AC_SUBST([ac_tool_prefix])
AC_SUBST([exeext])
AC_SUBST([OBJEXT])
AC_SUBST([libext])
AC_SUBST([S])
AC_SUBST([runtime_asm_objects])
AC_SUBST([SO])
AC_SUBST([arch])
AC_SUBST([arch_specific_SOURCES])
AC_SUBST([arch64])
AC_SUBST([model])
AC_SUBST([system])
AC_SUBST([bytecode_cflags])
AC_SUBST([bytecode_cppflags])
AC_SUBST([native_cflags])
AC_SUBST([native_cppflags])
AC_SUBST([oc_bytecode_cflags])
AC_SUBST([oc_bytecode_cppflags])
AC_SUBST([oc_native_cflags])
AC_SUBST([oc_native_cppflags])
AC_SUBST([systhread_support])
AC_SUBST([ocamlsrcdir])
AC_SUBST([unix_or_win32])
AC_SUBST([ln])
AC_SUBST([ocamlyacc_wstr_module])
AC_SUBST([outputexe])
AC_SUBST([outputobj])
AC_SUBST([syslib])
AC_SUBST([fpic])
AC_SUBST([mkexe])
AC_SUBST([mkexe_exp])
AC_SUBST([mkexedebugflag])
AC_SUBST([mkexe_via_cc_ldflags])
AC_SUBST([mkexe_via_cc_extra_cmd])
AC_SUBST([ccomptype])
AC_SUBST([toolchain])
AC_SUBST([common_cflags])
AC_SUBST([oc_cflags])
AC_SUBST([tsan])
AC_SUBST([tsan_native_runtime_c_sources])
AC_SUBST([common_cppflags])
AC_SUBST([oc_cppflags])
AC_SUBST([oc_ldflags])
AC_SUBST([oc_dll_ldflags])
AC_SUBST([cclibs])
AC_SUBST([native_ldflags])
AC_SUBST([zstd_libs])
AC_SUBST([bytecode_cflags])
AC_SUBST([bytecode_cppflags])
AC_SUBST([flexdll_source_dir])
AC_SUBST([bootstrapping_flexdll])
AC_SUBST([flexdll_dir])
AC_SUBST([winpthreads_source_dir])
AC_SUBST([winpthreads_source_include_dir])
AC_SUBST([shebangscripts])
AC_SUBST([AR])
AC_SUBST([mklib])
AC_SUBST([supports_shared_libraries])
AC_SUBST([natdynlink])
AC_SUBST([natdynlinkopts])
AC_SUBST([natdynlink_archive])
AC_SUBST([cmxs])
AC_SUBST([debug_runtime])
AC_SUBST([instrumented_runtime])
AC_SUBST([instrumented_runtime_libs])
AC_SUBST([otherlibraries])
AC_SUBST([otherlibs])
AC_SUBST([lib_dynlink])
AC_SUBST([lib_runtime_events])
AC_SUBST([lib_str])
AC_SUBST([lib_systhreads])
AC_SUBST([lib_unix])
AC_SUBST([optional_libraries])
AC_SUBST([optional_bytecode_tools])
AC_SUBST([optional_native_tools])
AC_SUBST([ocamltest_CPP])
AC_SUBST([ocamltest_libunix])
AC_SUBST([unix_library])
AC_SUBST([unix_directory])
AC_SUBST([cc_has_debug_prefix_map])
AC_SUBST([as_has_debug_prefix_map])
AC_SUBST([with_debugger]) # TODO: rename this variable
AC_SUBST([build_ocamldebug])
AC_SUBST([build_ocamltex])
AC_SUBST([build_ocamldoc])
AC_SUBST([ocamldoc_target])
AC_SUBST([ocamldoc_opt_target])
AC_SUBST([with_ocamldoc])
AC_SUBST([documentation_tool])
AC_SUBST([documentation_tool_cmd])
AC_SUBST([build_ocamlobjinfo])
AC_SUBST([build_ocamltest])
AC_SUBST([ocamltest])
AC_SUBST([ocamltest_target])
AC_SUBST([ocamltest_opt_target])
AC_SUBST([ASPP])
AC_SUBST([endianness])
AC_SUBST([AS])
AC_SUBST([asm_cfi_supported])
AC_SUBST([sharedlib_cflags])
AC_SUBST([rpath])
AC_SUBST([mkdll])
AC_SUBST([mkdll_exp])
AC_SUBST([mkmaindll])
AC_SUBST([mkmaindll_exp])
AC_SUBST([mksharedlibrpath])
AC_SUBST([install_bytecode_programs])
AC_SUBST([install_source_artifacts])
AC_SUBST([install_ocamlnat])
AC_SUBST([reserved_header_bits])
AC_SUBST([frame_pointers])
AC_SUBST([flambda])
AC_SUBST([flambda_invariants])
AC_SUBST([cmm_invariants])
AC_SUBST([windows_unicode])
AC_SUBST([align_double])
AC_SUBST([align_int64])
AC_SUBST([flat_float_array])
AC_SUBST([function_sections])
AC_SUBST([oc_native_compflags])
AC_SUBST([afl])
AC_SUBST([flexdll_chain])
AC_SUBST([PACKLD])
AC_SUBST([build_libraries_manpages])
AC_SUBST([compute_deps])
AC_SUBST([ocaml_bindir])
AC_SUBST([ocaml_libdir])
AC_SUBST([QS])
AC_SUBST([ar_supports_response_files])

## Generated files

AC_CONFIG_FILES([Makefile.build_config])
AC_CONFIG_FILES([Makefile.config])
AC_CONFIG_FILES([stdlib/sys.ml])
AC_CONFIG_FILES([manual/src/version.tex])
AC_CONFIG_FILES([manual/src/html_processing/src/common.ml])
AC_CONFIG_FILES([ocamltest/ocamltest_config.ml])
AC_CONFIG_FILES([otherlibs/dynlink/dynlink_config.ml])
AC_CONFIG_FILES([utils/config.common.ml])
AC_CONFIG_FILES([utils/config.generated.ml])
AC_CONFIG_HEADERS([runtime/caml/exec.h])
AC_CONFIG_HEADERS([runtime/caml/m.h])
AC_CONFIG_HEADERS([runtime/caml/s.h])
AC_CONFIG_HEADERS([runtime/caml/version.h])
AC_CONFIG_FILES([compilerlibs/META])
AC_CONFIG_FILES([otherlibs/dynlink/META])
AC_CONFIG_FILES([otherlibs/runtime_events/META])
AC_CONFIG_FILES([stdlib/META])

# Definitions related to the version of OCaml
AC_DEFINE([OCAML_VERSION_MAJOR], [OCAML__VERSION_MAJOR])
AC_DEFINE([OCAML_VERSION_MINOR], [OCAML__VERSION_MINOR])
AC_DEFINE([OCAML_VERSION_PATCHLEVEL], [OCAML__VERSION_PATCHLEVEL])
m4_if([OCAML__VERSION_EXTRA],[], [],
  [AC_DEFINE([OCAML_VERSION_ADDITIONAL], ["][OCAML__VERSION_EXTRA]["])
  AC_DEFINE([OCAML_VERSION_EXTRA], ["][OCAML__VERSION_EXTRA]["])])
AC_DEFINE([OCAML_VERSION], [OCAML__VERSION_NUMBER])
AC_DEFINE([OCAML_VERSION_STRING], ["][OCAML__VERSION]["])

# Works out how many "o"s are needed in quoted strings
AC_CONFIG_COMMANDS_PRE(OCAML_QUOTED_STRING_ID)

# Checks for system types

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# Ensure that AC_CONFIG_LINKS will either create symlinks which are compatible
# with native Windows (i.e. NTFS symlinks, not WSL or Cygwin-emulated ones) or
# use its fallback mechanisms. Native Windows versions of ocamlc/ocamlopt cannot
# interpret either WSL or Cygwin-emulated symlinks.
AS_CASE([$host],
  [*-pc-windows|*-w64-mingw32*],
    [AC_CONFIG_COMMANDS([native-symlinks], [],
      [export CYGWIN="\$CYGWIN\${CYGWIN:+ }winsymlinks:nativestrict"
      export MSYS="\$MSYS\${MSYS:+ }winsymlinks:nativestrict"])])

# Systems that are known not to work, even in bytecode only.

AS_CASE([$host],
  [i386-*-solaris*],
    [AC_MSG_ERROR([Building for 32 bits target is not supported. \
If your host is 64 bits, you can try with './configure CC="gcc -m64"' \
(or "cc -m64" if you don't have GCC).])])

# MSVC special case

AS_CASE([$host],
  [*-pc-windows],
    [AS_IF([test -z "$CC"], [CC=cl])
    ccomptype=msvc
    S=asm
    SO=dll
    outputexe=-Fe
    syslib='$(1).lib'],
  [ccomptype=cc
  S=s
  SO=so
  outputexe='-o '
  syslib='-l$(1)'])

AC_CHECK_PROG([csc],[csc],[csc])
AS_IF([test -n "$csc"],
  [AS_CASE([$host],
    [*-*-mingw32*|*-pc-windows],
      [CSC=csc
      CSCFLAGS="/nologo /nowarn:1668"
      AS_CASE([$host_cpu], [i*86], [CSCFLAGS="$CSCFLAGS /platform:x86"])],
    [AC_MSG_WARN([Mono is not yet supported - C sharp tests disabled])])])

# Environment variables that are taken into account

AC_ARG_VAR([AS], [which assembler to use])
AC_ARG_VAR([ASPP], [which assembler (with preprocessor) to use])
AC_ARG_VAR([PARTIALLD], [how to build partial (relocatable) object files])

AC_ARG_VAR([COMPILER_BYTECODE_CFLAGS],
  [CFLAGS for compiling C files to be linked with bytecode])
AC_ARG_VAR([COMPILER_BYTECODE_CPPFLAGS],
  [CPPFLAGS for compiling C files to be linked with bytecode])
AC_ARG_VAR([COMPILER_NATIVE_CFLAGS],
  [CFLAGS for compiling C files to be linked with native code])
AC_ARG_VAR([COMPILER_NATIVE_CPPFLAGS],
  [CPPFLAGS for compiling C files to be linked with native code])

# Command-line arguments to configure

AC_ARG_ENABLE([debug-runtime],
  [AS_HELP_STRING([--disable-debug-runtime],
    [do not build runtime with debugging support])])

AC_ARG_ENABLE([ocamldebug],
  [AS_HELP_STRING([--enable-ocamldebug],
    [build ocamldebug @<:@default=auto@:>@])],
  [],
  [enable_ocamldebug=auto])

AC_ARG_ENABLE([debugger],
  [AS_HELP_STRING([--enable-debugger],
    [alias for --enable-ocamldebug])],
  [enable_ocamldebug=$enableval],
  [])

AC_ARG_ENABLE([dependency-generation],
  [AS_HELP_STRING([--disable-dependency-generation],
    [do not compute dependency information for C sources])],
  [],
  [enable_dependency_generation=auto])

AC_ARG_VAR([DLLIBS],
  [which libraries to use (in addition to -ldl) to load dynamic libs])

AC_ARG_ENABLE([instrumented-runtime],
  [AS_HELP_STRING([--enable-instrumented-runtime],
    [build the instrumented runtime @<:@default=auto@:>@])],
  [],
  [enable_instrumented_runtime=auto])

AC_ARG_ENABLE([vmthreads], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR(m4_normalize([The vmthreads library is no longer available.
      It was deleted in OCaml 4.09.]))])],
  [])

AC_ARG_ENABLE([systhreads],
  [AS_HELP_STRING([--disable-systhreads],
    [disable the Win32/POSIX threads library])])

AC_ARG_ENABLE([tsan],
  [AS_HELP_STRING([--enable-tsan],
    [enable ThreadSanitizer support @<:@default=no@:>@])],
  [],
  [enable_tsan=no])

# Preprocessor and linker flags for libunwind (currently only used when TSan is
# enabled)

AC_ARG_VAR([LIBUNWIND_CPPFLAGS],
  [C preprocessor flags for libunwind (e.g. -I<location of libunwind headers>)])

AC_ARG_VAR([LIBUNWIND_LDFLAGS],
  [linker flags for libunwind (e.g. -L<location of libunwind>])

AC_ARG_ENABLE([graph-lib], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR(m4_normalize([The graphics library is no longer distributed
      with OCaml since version 4.09. It is now distributed as a separate
      "graphics" package: https://github.com/ocaml/graphics]))])],
  [])

AC_ARG_ENABLE([str-lib],
  [AS_HELP_STRING([--disable-str-lib],
    [do not build the str library])])

AC_ARG_ENABLE([unix-lib],
  [AS_HELP_STRING([--disable-unix-lib],
    [do not build the unix library])])

AC_ARG_ENABLE([bigarray-lib], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR(m4_normalize([The bigarray-lib option was deleted in OCaml
      5.0, as the Bigarray module is now part of the standard library.]))])],
  [])

AC_ARG_ENABLE([ocamldoc],
  [AS_HELP_STRING([--disable-ocamldoc],
    [do not build the ocamldoc documentation system])],
  [],
  [enable_ocamldoc='auto'])

AC_ARG_WITH([odoc],
  [AS_HELP_STRING([--with-odoc],
    [build documentation with odoc])])

AC_ARG_ENABLE([ocamlobjinfo],
  [AS_HELP_STRING([--disable-ocamlobjinfo],
    [do not build ocamlobjinfo])])

AC_ARG_ENABLE([ocamltest],
  [AS_HELP_STRING([--disable-ocamltest],
    [do not build ocamltest])])

AC_ARG_ENABLE([native-toplevel],
  [AS_HELP_STRING([--enable-native-toplevel],
    [build the native toplevel])])

AC_ARG_ENABLE([frame-pointers],
  [AS_HELP_STRING([--enable-frame-pointers],
    [use frame pointers in runtime and generated code])])

AC_ARG_ENABLE([naked-pointers], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR([Naked pointers were prohibited in OCaml 5.0.])])],
  [])

AC_ARG_ENABLE([naked-pointers-checker], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR([The naked pointers checker was removed in OCaml 5.0.])])],
  [])

AC_ARG_ENABLE([spacetime], [],
  [AS_IF([test "x$enableval" != 'xno'],
    [AC_MSG_ERROR([spacetime profiling was deleted in OCaml 4.12.])])],
  [])

AC_ARG_ENABLE([cfi],
  [AS_HELP_STRING([--disable-cfi],
    [disable the CFI directives in assembly files])])

AC_ARG_ENABLE([imprecise-c99-float-ops],
  [AS_HELP_STRING([--enable-imprecise-c99-float-ops],
    [enables potentially imprecise replacement implementations
     of C99 float ops if unavailable on this platform])])

AC_ARG_ENABLE([installing-source-artifacts],
  [AS_HELP_STRING([--enable-installing-source-artifacts],
    [install *.cmt* and *.mli files])])
AC_ARG_ENABLE([installing-bytecode-programs],
  [AS_HELP_STRING([--enable-installing-bytecode-programs],
    [also install the bytecode versions of programs])])

AC_ARG_ENABLE([native-compiler],
  [AS_HELP_STRING([--disable-native-compiler],
    [do not build the native compiler])])

AC_ARG_ENABLE([flambda],
  [AS_HELP_STRING([--enable-flambda],
    [enable flambda optimizations])])

AC_ARG_ENABLE([flambda-invariants],
  [AS_HELP_STRING([--enable-flambda-invariants],
    [enable invariants checks in flambda])])

AC_ARG_ENABLE([cmm-invariants],
  [AS_HELP_STRING([--enable-cmm-invariants],
    [enable invariants checks in Cmm])])

AC_ARG_WITH([target-bindir],
  [AS_HELP_STRING([--with-target-bindir],
    [location of the runtime binaries on the target system])],
  [target_bindir=$withval],
  [target_bindir=''])

AC_ARG_WITH([target-sh],
  [AS_HELP_STRING([--with-target-sh],
    [location of Posix sh on the target system])],
  [AS_IF([test x"$withval" = 'xno'],
    [target_launch_method='exe'],
    [target_launch_method="$withval"])],
  [target_launch_method=''])

AC_ARG_ENABLE([reserved-header-bits],
  [AS_HELP_STRING([--enable-reserved-header-bits=BITS],
    [reserve BITS (between 0 and 31) bits in block headers])],
  [AS_CASE([$enable_reserved_header_bits],
    [[[0-9]]|[[1-2]][[0-9]]|3[[0-1]]],
      [reserved_header_bits=$enable_reserved_header_bits],
  [AC_MSG_ERROR([invalid argument to --enable-reserved-header-bits])])])

AC_ARG_ENABLE([stdlib-manpages],
  [AS_HELP_STRING([--disable-stdlib-manpages],
    [do not build or install the library man pages])],
  [],
  [enable_stdlib_manpages='auto'])

AC_ARG_ENABLE([warn-error],
  [AS_HELP_STRING([--enable-warn-error],
    [treat C compiler warnings as errors])])

AC_ARG_VAR([WINDOWS_UNICODE_MODE],
  [how to handle Unicode under Windows: ansi, compatible])

AC_ARG_ENABLE([force-safe-string], [],
  [AS_IF([test "x$enableval" = 'xno'],
    [AC_MSG_ERROR([Support for mutable strings was removed in OCaml 5.0.])])],
  [])

AC_ARG_VAR([DEFAULT_STRING], [])
AS_IF([test x"$DEFAULT_STRING" = "xunsafe"],
  [AC_MSG_ERROR([Support for mutable strings was removed in OCaml 5.0.])])

AC_ARG_ENABLE([flat-float-array],
  [AS_HELP_STRING([--disable-flat-float-array],
    [do not use flat float arrays])])

AC_ARG_ENABLE([function-sections],
  [AS_HELP_STRING([--disable-function-sections],
    [do not emit each function in a separate section])],
  [],
  [enable_function_sections=auto])

AC_ARG_ENABLE([mmap-map-stack],
  [AS_HELP_STRING([--enable-mmap-map-stack],
    [use mmap to allocate stacks instead of malloc])])

AC_ARG_WITH([afl],
  [AS_HELP_STRING([--with-afl],
    [use the AFL fuzzer])])

AC_ARG_WITH([flexdll],
  [AS_HELP_STRING([--with-flexdll],
    [bootstrap FlexDLL from the given sources])],
  [AS_IF([test x"$withval" = 'xyes'],[with_flexdll=flexdll])])

AC_ARG_WITH([winpthreads-msvc],
  [AS_HELP_STRING([--with-winpthreads-msvc],
    [build winpthreads (only for the MSVC port) from the given sources])],
  [AS_IF([test x"$withval" = 'xyes'], [with_winpthreads_msvc=winpthreads])])

AC_ARG_WITH([zstd],
  [AS_HELP_STRING([--without-zstd],
    [disable compression of compilation artefacts])])

AS_IF([test x"$enable_unix_lib" = "xno"],
  [AS_IF([test x"$enable_ocamldebug" = "xyes"],
    [AC_MSG_ERROR([ocamldebug requires the unix library])],
    [enable_ocamldebug="no"])])

AS_IF([test x"$enable_unix_lib" = "xno" || test x"$enable_str_lib" = "xno"],
  [AS_IF([test x"$enable_ocamldoc" = "xyes"],
    [AC_MSG_ERROR([ocamldoc requires the unix and str libraries])],
    [enable_ocamldoc="no"
     build_ocamltex=false])],
  [build_ocamltex=true])

AS_IF([test x"$enable_ocamldoc" = "xno"],
  [AS_IF([test x"$enable_stdlib_manpages" = "xyes"],
    [AC_MSG_ERROR([--enable-stdlib-manpages requires ocamldoc])])
  ocamldoc_target=""
  ocamldoc_opt_target=""
  with_ocamldoc=""
  enable_stdlib_manpages=no
  build_ocamldoc=false],
  [ocamldoc_target=ocamldoc
  ocamldoc_opt_target=ocamldoc.opt
  with_ocamldoc=ocamldoc
  build_ocamldoc=true
  optional_libraries="$optional_libraries ocamldoc/odoc_info"
  AC_CONFIG_FILES([ocamldoc/META])])

# Initialization of libtool
# Allow the MSVC linker to be found even if ld isn't installed.
# User-specified LD still takes precedence.
AC_CHECK_TOOLS([LD],[ld link])
# libtool expects host_os=mingw for native Windows
# Also, it has been observed that, on some platforms (e.g. msvc) LT_INIT
# alters the CFLAGS variable, so we save its value before calling the macro
# and restore it after the call
old_host_os=$host_os
AS_IF([test x"$host_os" = "xwindows"],[host_os=mingw])
saved_CFLAGS="$CFLAGS"
LT_INIT
CFLAGS="$saved_CFLAGS"
host_os=$old_host_os

AS_CASE([$host],
  [sparc-sun-solaris*],
    [DEP_CC="false"],
  [*-pc-windows],
    [AC_CHECK_TOOLS(
      [DEP_CC],
      [$DEP_CC gcc cc x86_64-w64-mingw32-gcc i686-w64-mingw32-gcc],
      [false])],
  [DEP_CC="$CC"])

AS_CASE([$enable_dependency_generation],
  [yes],
    [AS_IF([test "$DEP_CC" = "false"],
      [AC_MSG_ERROR(m4_normalize([The MSVC ports cannot generate dependency
        information. Install gcc (or another CC-like compiler)]))],
      [compute_deps=true])],
  [no], [compute_deps=false],
  [AS_IF([test -e .git],
    [AS_IF([test "$DEP_CC" = "false"],
      [compute_deps=false],
      [compute_deps=true])],
    [compute_deps=false])])

AS_CASE([$host],
  # In config/Makefile.mingw*, we had:
  # TARGET=i686-w64-mingw32 and x86_64-w64-mingw32
  # TOOLPREF=$(TARGET)-
  # ARCMD=$(TOOLPREF)ar
  # However autoconf and libtool seem to use ar
  # So we let them do, at the moment
  [*-pc-windows],
    [
      mkexe_via_cc_extra_cmd=' && $(call MERGEMANIFESTEXE,$(1))'
      libext=lib
      AR=""
      AS_IF([test "$host_cpu" = "x86_64" ],
        [machine="-machine:AMD64 "],
        [machine=""])
      mklib="link -lib -nologo $machine /out:\$(1) \$(2)"
    ],
  [
    mklib="rm -f \$(1) && ${AR} rc \$(1) \$(2)"
  ])

## Find vendor of the C compiler
OCAML_CC_VENDOR

## In cross-compilation mode, can we run executables produced?
# At the moment, it's required, but the fact is used in C99 function detection
OCAML_HOST_IS_EXECUTABLE

# Determine how to call the C preprocessor directly.
# Most of the time, calling the C preprocessor through the C compiler is
# desirable and even important.
# In some cases, though, we want to use the C preprocessor only to
# expand macros. In such cases, it is much more convenient to be able
# to invoke it directly rather than through the C compiler, for instance
# because, when invoked directly, the C preprocessor does not require
# to be invoked on a file with a '.c' extension
# We thus figure out how to invoke the C preprocessor directly but
# let the CPP variable untouched, except for the MSVC port where we set it
# manually to make sure the backward compatibility is preserved
AS_CASE([$ocaml_cc_vendor],
  [xlc-*],
    [CPP="$CC -E -qnoppline" # suppress incompatible XLC line directives
    ocamltest_CPP="$CPP"],
  [sunc-*],
    [CPP="$CC -E -Qn" # suppress generation of Sun PRO ident string
    ocamltest_CPP="$CPP"],
  [msvc-*],
    [CPP="$CC -nologo -EP -TC"
    ocamltest_CPP="$CPP 2> nul"],
  [CPP="$CC -E -P"
  ocamltest_CPP="$CPP"])

# Absolute path to the toplevel source directory

# A sentinel character (X) is added at the end of the directory name
# and then removed, in case the directory name finishes with an odd character.
# (The $(...) construct strips trailing \n characters.)
# Also, it is important to use `printf` rather than `echo` because the
# behaviour of `echo` is indeterminate and uncontrollable if the
# absolute path contains backslashes, which will definitely be the case
# on Windows
# See https://lists.gnu.org/archive/html/autoconf/2019-07/msg00002.html
# for the detailed explanation.

ocamlsrcdir=$(unset CDPATH; cd -- "$srcdir" && printf %sX "$PWD") || fail
ocamlsrcdir=${ocamlsrcdir%X}

# Whether ar supports @FILE arguments

AS_CASE([$lt_cv_ar_at_file],
  [no], [ar_supports_response_files=false],
  [ar_supports_response_files=true])

# Libraries to build depending on the host

AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows],
    [unix_or_win32="win32"
    OCAML_CHECK_LN_ON_WINDOWS
    ocamltest_libunix="Some false"
    ocamlsrcdir="$(LC_ALL=C.UTF-8 cygpath -w -- "$ocamlsrcdir")"
    ocamlyacc_wstr_module="yacc/wstr"],
  [unix_or_win32="unix"
  ln='ln -sf'
  ocamltest_libunix="Some true"
  ocamlyacc_wstr_module=""])

AS_CASE([$host],
  [*-*-cygwin*|*-*-mingw32*|*-pc-windows],
    [exeext=".exe"],
  [exeext=''])

otherlibraries="dynlink runtime_events"
otherlibs="runtime_events"
optional_libraries="$optional_libraries otherlibs/dynlink/dynlink"
lib_dynlink=true
dldir=otherlibs/dynlink
AC_CONFIG_LINKS([
  $dldir/dynlink_cmo_format.mli:file_formats/cmo_format.mli
  $dldir/dynlink_cmxs_format.mli:file_formats/cmxs_format.mli
  $dldir/dynlink_platform_intf.mli:$dldir/dynlink_platform_intf.ml
])
lib_runtime_events=true
AS_IF([test x"$enable_unix_lib" != "xno"],
  [enable_unix_lib=yes
  AC_CONFIG_FILES([otherlibs/unix/META])
  otherlibraries="$otherlibraries unix"
  otherlibs="$otherlibs unix"
  lib_unix=true
  AC_CONFIG_LINKS([
    otherlibs/unix/unix.ml:otherlibs/unix/unix_${unix_or_win32}.ml
  ])
  ocamltest_unix_impl="real"
  unix_directory="otherlibs/unix"
  unix_library="${unix_directory}/unix"],
  [ocamltest_libunix="None"])

AS_IF([test x"$enable_str_lib" != "xno"],
  [otherlibraries="$otherlibraries str"
  otherlibs="$otherlibs str"
  lib_str=true
  AC_CONFIG_FILES([otherlibs/str/META])])

# Checks for system services

## Test whether #! scripts are supported
## TODO: have two values, one for host and one for target
AC_SYS_INTERPRETER

shebangscripts=false
launch_method='exe'
AS_IF([test "x$interpval" = "xyes"],
  [AS_CASE([$host],
    [*-cygwin],
      [# Cygwin supports shebangs, which we use for the compiler itself, but
      # partially for legacy, and partially so that executables can be easily
      # run from outside Cygwin, the executables ocamlc and ocamlopt produce do
      # not use shebang scripts
      shebangscripts=true
      launch_method='sh'
      AS_IF([test x"$target_launch_method" = 'x'],
        [target_launch_method='exe'])],
    [*-*-mingw32*|*-pc-windows], [],
    [shebangscripts=true
    launch_method='sh']
  )]
)

# stdlib/runtime.info and stdlib/target_runtime.info are generated by commands
# in config.status, rather than by the .in mechanism, since the latter cannot
# reliably process binary files.
AC_CONFIG_COMMANDS([shebang],
  [printf '%s\n%s\000\n' "$launch_method" "$ocaml_bindir" \
    > stdlib/runtime.info
  printf '%s\n%s\000\n' "$target_launch_method" "$target_bindir" \
    > stdlib/target_runtime.info],
dnl These declarations are put in a here-document in configure, so the command
dnl in '$(...)' _is_ evaluated as the content is written to config.status (by
dnl standard interpretation of a here-document). The sed commands quote any
dnl nefarious single quotes which may appear in any of the strings.
  [launch_method='$(echo "$launch_method" | sed -e "s/'/'\"'\"'/g")'
  target_launch_method=\
'$(echo "$target_launch_method" | sed -e "s/'/'\"'\"'/g")'
  ocaml_bindir='$(echo "$ocaml_bindir" | sed -e "s/'/'\"'\"'/g")'
  target_bindir='$(echo "$target_bindir" | sed -e "s/'/'\"'\"'/g")'])

# Are we building a cross-compiler

AS_IF(
  [test x"$host" = x"$target"],
    [cross_compiler=false],
    [cross_compiler=true])

# Checks for programs

## Check for the C compiler: done by libtool
## AC_PROG_CC

## Check for C99 support: done by libtool
## AC_PROG_CC_C99

## Determine which flags to use for the C compiler

AS_CASE([$ocaml_cc_vendor],
  [xlc-*],
    [outputobj='-o '
    warn_error_flag=''
    cc_warnings='-qflag=i:i'], # all warnings enabled
  [sunc-*],
    [outputobj='-o '; cc_warnings=""],
  [msvc-*],
    [outputobj='-Fo'
    AS_CASE([$ocaml_cc_vendor],
      [msvc-*-clang-*],
        [cc_warnings='-W4 -Wno-unused-parameter -Wno-sign-compare -Wundef'
         warn_error_flag='-WX'],
      [cc_warnings='-W2'
       warn_error_flag='-WX -options:strict'])],
  [outputobj='-o '
  warn_error_flag='-Werror'
  cc_warnings="-Wall -Wint-conversion -Wstrict-prototypes \
-Wold-style-definition -Wundef"])

# Use -Wold-style-declaration if supported
AX_CHECK_COMPILE_FLAG([-Wold-style-declaration],
  [cc_warnings="$cc_warnings -Wold-style-declaration"], [],
  [$warn_error_flag])

# Use -Wimplicit-fallthrough if supported
for flag in '-Wimplicit-fallthrough=5' '-Wimplicit-fallthrough'; do
  AX_CHECK_COMPILE_FLAG([$flag],
    [cc_warnings="$cc_warnings $flag"; break], [], [$warn_error_flag])
done

AS_CASE([$enable_warn_error,OCAML__DEVELOPMENT_VERSION],
  [yes,*|,true],
    [cc_warnings="$cc_warnings $warn_error_flag"])

AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows],
    [AS_CASE([$WINDOWS_UNICODE_MODE],
      [ansi],
        [windows_unicode=0],
      [compatible|""],
        [windows_unicode=1],
      [AC_MSG_ERROR([unexpected windows unicode mode])])],
  [windows_unicode=0])

# We select high optimization levels, provided we can turn off:
# - strict type-based aliasing analysis (too risky for the OCaml runtime)
# - strict no-overflow conditions on signed integer arithmetic
#   (the OCaml runtime assumes Java-style behavior of signed integer arith.)
# Concerning optimization level, -O3 is somewhat risky, so take -O2.
# Concerning language version, recent enough versions of GCC and Clang
# default to gnu11 (C11 + GNU extensions) or gnu17, which is fine.
AS_CASE([$ocaml_cc_vendor],
  [clang-*],
    [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
    internal_cflags="$cc_warnings -fno-common"],
  [*gcc-[[0123]]-*|*gcc-4-[[0-8]]],
    # No C11 support
    [AC_MSG_ERROR(m4_normalize([This version of GCC is too old.
      Please use GCC version 4.9 or above.]), 69)],
  [gcc-*],
    [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
    internal_cflags="$cc_warnings -fno-common -fexcess-precision=standard \
-Wvla"],
  [mingw-*-*-gcc-*],
    [internal_cflags="-Wno-unused $cc_warnings \
-fexcess-precision=standard"
    # TODO: see whether the code can be fixed to avoid -Wno-unused
    common_cflags="-O2 -fno-strict-aliasing -fwrapv -mms-bitfields"
    internal_cppflags='-D__USE_MINGW_ANSI_STDIO=0 -DUNICODE -D_UNICODE'
    internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE=$windows_unicode"],
  [mingw-*],
    [AC_MSG_ERROR([Unsupported C compiler for a MinGW-w64 build])],
  [msvc-0*|msvc-1[[0-8]]*|msvc-19[[012]]*|msvc-193[[0-7]]],
    # No C11 atomics support
    [AC_MSG_ERROR(m4_normalize([This version of MSVC is too old.
    Please use Visual Studio version 17.8 or above.]), 69)],
  [msvc-*],
    [common_cflags='-nologo -O2 -Gy- -MD'
    common_cppflags="-D_CRT_SECURE_NO_DEPRECATE"
    internal_cflags="$cc_warnings"
    internal_cppflags='-DUNICODE -D_UNICODE -D_CRT_NONSTDC_NO_WARNINGS'
    AX_CHECK_COMPILE_FLAG([-volatileMetadata-],
      [internal_cflags="$internal_cflags -volatileMetadata-"], [],
      [$warn_error_flag])
    internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE=$windows_unicode"],
  [xlc-*],
    [common_cflags="-O5 -qtune=balanced -qnoipa -qinline";
    internal_cflags="$cc_warnings"],
  [sunc-*], # Optimization should be >= O4 to inline functions
            # and prevent unresolved externals
    [common_cflags="-O4 -xc99=all"
    common_cppflags="-D_XPG6"
    internal_cflags="$cc_warnings"],
  [common_cflags="-O"])

# Enable SSE2 on x86 mingw to avoid using 80-bit registers.
AS_CASE([$host],
  [i686-*-mingw32*],
    [internal_cflags="$internal_cflags -mfpmath=sse -msse2"])

# Use 64-bit file offset if possible
# See also AC_SYS_LARGEFILE
# Problem: flags are added to CC rather than CPPFLAGS
AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows], [],
  [common_cppflags="$common_cppflags -D_FILE_OFFSET_BITS=64"])

# Adjust according to target

# On Windows we do not take $enable_shared because it does not seem
# to work. This should be better understood later
#AS_CASE([$target],
#  [*-pc-windows],
#    [enable_shared=yes])

AS_IF([test x"$enable_shared" = "xno"],
  [supports_shared_libraries=false
  AS_CASE([$host],
    [*-pc-windows|*-w64-mingw32*],
    [AC_MSG_ERROR([Cannot build native Win32 with --disable-shared])])],
  [supports_shared_libraries=true])

# Define flexlink chain and flags correctly for the different Windows ports
flexlink_flags=''
AS_CASE([$host],
  [i686-*-cygwin],
    [flexdll_chain='cygwin'
    flexlink_flags='-merge-manifest -stack 16777216'],
  [x86_64-*-cygwin],
    [flexdll_chain='cygwin64'
    flexlink_flags='-merge-manifest -stack 16777216'],
  [*-*-cygwin*],
    [AC_MSG_ERROR([unknown cygwin variant])],
  [i686-w64-mingw32*],
    [flexdll_chain='mingw'
    flexlink_flags='-stack 16777216'],
  [x86_64-w64-mingw32*],
    [flexdll_chain='mingw64'
    flexlink_flags='-stack 33554432'],
  [i686-pc-windows],
    [flexdll_chain='msvc'
    flexlink_flags='-merge-manifest -stack 16777216'],
  [x86_64-pc-windows],
    [flexdll_chain='msvc64'
    flexlink_flags='-merge-manifest -stack 33554432'],
  [flexdll_chain=''])

AS_IF([test x"$supports_shared_libraries" != 'xfalse'], [
  AC_MSG_CHECKING([for flexdll sources])
  AS_IF([test x"$with_flexdll" = "xno"],
    [flexdll_source_dir=''
    AC_MSG_RESULT([disabled])],
    [flexmsg=''
    AS_CASE([$target],
      [*-*-cygwin*|*-w64-mingw32*|*-pc-windows],
      [dnl When bootstrapping from the git submodule (flexdll directory), just
       dnl use that, however if another directory has been specified with
       dnl --with-flexdll=<path> then copy the contents of <path> to
       dnl flexdll-sources. This allows us to guarantee that flexdll is always
       dnl built at one directory level below the root, so we can specify
       dnl ROOTDIR=..
      AS_IF([m4_normalize([test x"$with_flexdll" = 'x'
                            || test x"$with_flexdll" = 'xflexdll'])],
        [AS_IF([test -f 'flexdll/flexdll.h'],
          [flexdll_source_dir=flexdll],
          [AS_IF([test x"$with_flexdll" != 'x'],
            [AC_MSG_RESULT([requested but not available])
            AC_MSG_ERROR([exiting])])])],
        [rm -rf flexdll-sources
        AS_IF([test -f "$with_flexdll/flexdll.h"],
          [mkdir -p flexdll-sources
          cp -r "$with_flexdll"/* flexdll-sources/
          flexdll_source_dir='flexdll-sources'
          flexmsg=" (from $with_flexdll)"],
          [AC_MSG_RESULT([requested but not available])
          AC_MSG_ERROR([exiting])])])
      AS_IF([test x"$flexdll_source_dir" = 'x'],
        [AC_MSG_RESULT([no])],
        [AC_MSG_RESULT([$flexdll_source_dir$flexmsg])
        bootstrapping_flexdll=true
        flexdll_dir=\"+flexdll\"])],
      [AS_IF([test x"$with_flexdll" != 'x'],
        [AC_MSG_RESULT([requested but not supported])
        AC_MSG_ERROR([exiting])])])])

  AC_CHECK_PROG([flexlink],[flexlink],[flexlink])

  AS_IF([test -n "$flexlink" && test -z "$flexdll_source_dir"],[
    OCAML_TEST_FLEXLINK([$flexlink], [$flexdll_chain],
                        [$internal_cppflags], [$host])

    # When building on Cygwin/MSYS2, flexlink may be a shell script which
    # then cannot be executed by ocamlc/ocamlopt. Having located flexlink,
    # ensure it can be executed from a native Windows process. The check
    # is only necessary when cross-compiling.
    AS_IF([test x"$build" != x"$host"],[
      AS_CASE([$build],
        [*-pc-msys|*-pc-cygwin*],
          [flexlink_where="$(cmd /c "$flexlink" -where 2>/dev/null)"
          AS_IF([test -z "$flexlink_where"],
            [AC_MSG_ERROR(m4_normalize([$flexlink is not executable from a
            native Win32 process]))])])
    ])
  ])

  OCAML_TEST_FLEXDLL_H([$flexdll_source_dir])

  AS_IF([test -n "$flexlink" && test x"$have_flexdll_h" = 'xno'],
    [OCAML_TEST_FLEXLINK_WHERE([$flexlink])
    AS_IF([test "x$have_flexdll_h" = 'xyes'],
      [internal_cppflags="$internal_cppflags -I \"$flexlink_where\""])
  ])
])

AS_CASE([$have_flexdll_h,$supports_shared_libraries,$host],
 [no,true,*-*-cygwin*],
   [supports_shared_libraries=false
   AC_MSG_WARN([flexdll.h not found: shared library support disabled.])],
 [no,*,*-w64-mingw32*|no,*,*-pc-windows],
   [AC_MSG_ERROR([flexdll.h is required for native Win32])])

AS_CASE([$flexdll_source_dir,$supports_shared_libraries,$flexlink,$host],
  [,true,,*-*-cygwin*],
    [supports_shared_libraries=false
    AC_MSG_WARN([flexlink not found: shared library support disabled.])],
  [,*,,*-w64-mingw32*|,*,,*-pc-windows],
    [AC_MSG_ERROR([flexlink is required for native Win32])])

mkexe_cmd_exp="$CC"

AS_CASE([$ocaml_cc_vendor,$host],
  [*,x86_64-*-darwin*],
    [oc_ldflags='-Wl,-no_compact_unwind';
    AC_DEFINE([HAS_ARCH_CODE32], [1])],
  [*,aarch64-*-darwin*|arm64-*-darwin*],
    AC_DEFINE([HAS_ARCH_CODE32], [1]),
  [*,*-*-cygwin*],
    [common_cppflags="$common_cppflags -U_WIN32"
    AS_IF([$supports_shared_libraries],
      [mkexe_cmd_exp="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
      mkexe_cmd="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
      mkexe_ldflags_prefix='-link '],
      [mkexe_extra_flags=''
      oc_ldflags='-Wl,--stack,16777216']
    )
    ostype="Cygwin"],
  [*,*-*-mingw32*],
    [AS_CASE([$host],
      [i686-*-*], [oc_dll_ldflags="-static-libgcc"])
    ostype="Win32"
    toolchain="mingw"
    mkexe_cmd_exp="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
    mkexe_cmd="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
    mkexe_ldflags_prefix='-link '
    oc_exe_ldflags='-municode'
    mkexe_extra_flags="$mkexe_ldflags_prefix$oc_exe_ldflags"
    SO="dll"],
  [*,*-pc-windows],
    [toolchain=msvc
    ostype="Win32"
    mkexe_cmd_exp="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
    mkexe_cmd="flexlink -exe -chain ${flexdll_chain} ${flexlink_flags}"
    mkexe_ldflags_prefix='-link '
    mkexe_via_cc_ldflags_prefix='/link '
    oc_exe_ldflags='/ENTRY:wmainCRTStartup'
    mkexe_extra_flags="$mkexe_ldflags_prefix$oc_exe_ldflags"
    mkexedebugflag=''],
  [*,x86_64-*-linux*],
    AC_DEFINE([HAS_ARCH_CODE32], [1]),
  [xlc-*,powerpc-ibm-aix*],
    [oc_ldflags='-brtl -bexpfull'
    AC_DEFINE([HAS_ARCH_CODE32], [1])],
  [gcc-*,powerpc-*-linux*],
    [oc_ldflags="-mbss-plt"])

# Winpthreads emulation library for the MSVC port
AC_MSG_CHECKING([for winpthreads sources])
AS_IF([test x"$with_winpthreads_msvc" = "xno"],
  [winpthreads_source_dir=''
  AC_MSG_RESULT([disabled])],
  [winpthreadmsg=''
  AS_CASE([$target],
    [*-pc-windows],
    [dnl When bootstrapping from the git submodule (winpthreads directory),
     dnl just use that, however if another directory has been specified with
     dnl --with-winpthreads-msvc=<path> then copy the contents of <path> to
     dnl winpthreads-sources.
    AS_IF([m4_normalize([test x"$with_winpthreads_msvc" = 'x'
                          || test x"$with_winpthreads_msvc" = x'winpthreads'])],
      [AS_IF([test -f 'winpthreads/src/winpthread_internal.h'],
        [winpthreads_source_dir=winpthreads],
        [AC_MSG_RESULT([required but not available (uninitialized submodule?)])
        AC_MSG_ERROR([exiting])])],
      [rm -rf winpthreads-sources
      AS_IF([test -f "$with_winpthreads_msvc/src/winpthread_internal.h"],
        [mkdir -p winpthreads-sources/src winpthreads-sources/include
        cp "$with_winpthreads_msvc"/src/*.c winpthreads-sources/src
        cp "$with_winpthreads_msvc"/src/*.h winpthreads-sources/src
        cp "$with_winpthreads_msvc"/include/*.h winpthreads-sources/include
        winpthreads_source_dir='winpthreads-sources'
        winpthreadsmsg=" (from $with_winpthreads_msvc)"],
        [AC_MSG_RESULT([requested but not available])
        AC_MSG_ERROR([exiting])])])
    AS_IF([test x"$winpthreads_source_dir" = 'x'],
      [AC_MSG_RESULT([no])],
      [AC_MSG_RESULT([$winpthreads_source_dir$winpthreadsmsg])
      winpthreads_source_include_dir="$winpthreads_source_dir/include"
      OCAML_TEST_WINPTHREADS_PTHREAD_H([$winpthreads_source_include_dir])])],
    [AS_IF([test x"$with_winpthreads_msvc" != 'x'],
      [AC_MSG_RESULT([requested but not supported])
      AC_MSG_ERROR([exiting])],
      [AC_MSG_RESULT([skipping on that platform])])])])

## Program to use to install files
AC_PROG_INSTALL

# Checks for libraries

## Mathematical library
AC_SEARCH_LIBS([cos], [m],
  [AS_CASE([$ac_cv_search_cos],
    [no], [AC_MSG_ERROR([Could not locate C math functions])],
    ['none required'], [mathlib=""],
    [mathlib="$ac_cv_search_cos"])])

# Checks for header files

# Don't check for unistd.h on Windows
AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows], [],
  [AC_CHECK_HEADERS([unistd.h],[AC_DEFINE([HAS_UNISTD], [1])])])

AC_CHECK_HEADER([math.h])
AC_CHECK_HEADER([pthread_np.h],[AC_DEFINE([HAS_PTHREAD_NP_H], [1])])
AC_CHECK_HEADER([dirent.h], [AC_DEFINE([HAS_DIRENT], [1])], [],
  [#include <sys/types.h>])

AC_CHECK_HEADER([sys/select.h], [AC_DEFINE([HAS_SYS_SELECT_H], [1])], [],
  [#include <sys/types.h>])

AC_CHECK_HEADER([sys/mman.h], [AC_DEFINE([HAS_SYS_MMAN_H], [1])])

AS_CASE([$host],
  [*-*-linux*],
    [AC_CHECK_HEADER([linux/futex.h], [AC_DEFINE([HAS_LINUX_FUTEX_H])])])

# Checks for types

## off_t
AC_TYPE_OFF_T

# Checks for structures

# Checks for compiler characteristics

AC_CHECK_SIZEOF(int)
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long *)
AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(long long)

AS_IF(
  [test "x$ac_cv_sizeof_long_p" = "x4" ],
    [bits=32; arch64=false],
  [test "x$ac_cv_sizeof_long_p" = "x8" ],
    [bits=64; arch64=true
    AC_DEFINE([ARCH_SIXTYFOUR], [1])],
  [AC_MSG_ERROR([Neither 32 nor 64 bits architecture.])]
)

AS_IF([test "x$ac_cv_sizeof_int" != "x4" && test "x$ac_cv_sizeof_long" != "x4" \
       && test "x$ac_cv_sizeof_short" != "x4"],
  [AC_MSG_ERROR([Sorry, we can't find a 32-bit integer type.])]
)

AS_IF(
  [test "x$ac_cv_sizeof_long" != "x8" &&
   test "x$ac_cv_sizeof_long_long" != "x8"],
  [AC_MSG_ERROR([Sorry, we can't find a 64-bit integer type.])]
)

AC_DEFINE_UNQUOTED([SIZEOF_PTR], [$ac_cv_sizeof_long_p])
AC_DEFINE_UNQUOTED([SIZEOF_LONGLONG], [$ac_cv_sizeof_long_long])

AC_MSG_NOTICE([Target is a $bits bits architecture])

AC_C_BIGENDIAN(
  [
    AC_DEFINE([ARCH_BIG_ENDIAN], [1])
    [endianness="be"]
  ],
  [endianness="le"],
  [AC_MSG_ERROR([could not determine endianness.])],
  [AC_MSG_ERROR([unable to handle universal endianness])]
)

AC_CHECK_ALIGNOF([double])
AC_CHECK_ALIGNOF([long])
AC_CHECK_ALIGNOF([long long])

align_double=false
align_int64=false

AS_IF([! $arch64],
  [AS_CASE([$target_cpu],
    [i686], [],
    [AS_IF([test "$ac_cv_alignof_double" -gt 4],
      [align_double=true
       AC_DEFINE([ARCH_ALIGN_DOUBLE], [1])])
     AS_IF([test "x$ac_cv_sizeof_long" = "x8" &&
            test "$ac_cv_alignof_long" -gt 4],
      [align_int64=true
       AC_DEFINE([ARCH_ALIGN_INT64], [1])],
      [AS_IF([test "x$ac_cv_sizeof_long_long" = "x8" &&
              test "$ac_cv_alignof_long_long" -gt 4],
      [align_int64=true
       AC_DEFINE([ARCH_ALIGN_INT64], [1])])])
    ])])

AC_CHECK_TYPES([max_align_t], [], [], [[#include <stddef.h>]])

# Atomics library

AS_IF([! $arch64],
  [AS_CASE([$ocaml_cc_vendor],
    [gcc-*], [cclibs="$cclibs -latomic"])])

# Support for C11 atomic types

OCAML_CC_SUPPORTS_ATOMIC([], [$cclibs])
AS_CASE([$cc_supports_atomic,$ocaml_cc_vendor],
  [false,msvc-*],
    [OCAML_CC_SUPPORTS_ATOMIC([-std:c11])
     AS_IF([$cc_supports_atomic],
       [common_cflags="$common_cflags -std:c11"],
       [OCAML_CC_SUPPORTS_ATOMIC([-std:c11 -experimental:c11atomics])
        AS_IF([$cc_supports_atomic],
          [common_cflags="$common_cflags -std:c11 -experimental:c11atomics"])
])])
AS_IF([! $cc_supports_atomic],
  [AC_MSG_FAILURE([C11 atomic support is required, use another C compiler])])

# Full support for thread local storage
# macOS and MinGW-w64 have problems with thread local storage accessed from DLLs

AS_CASE([$host],
  [*-apple-darwin*|*-mingw32*|*-pc-windows], [],
  [AC_DEFINE([HAS_FULL_THREAD_VARIABLES], [1])]
)

# Shared library support

sharedlib_cflags=''
mkdll='shared-libs-not-available'
mkdll_exp='shared-libs-not-available'
rpath=''
mksharedlibrpath=''
natdynlinkopts=""

AS_IF([test x"$enable_shared" != "xno"],
  [mkdll=''
  AS_CASE([$host],
    [x86_64-apple-darwin*],
      [mkdll_flags=\
'-shared -undefined dynamic_lookup -Wl,-no_compact_unwind -Wl,-w'
      supports_shared_libraries=true],
    [aarch64-apple-darwin*|arm64-apple-darwin*],
      [mkdll_flags='-shared -undefined dynamic_lookup -Wl,-w'
      supports_shared_libraries=true],
    [*-*-mingw32*|*-pc-windows|*-*-cygwin*],
      [mkdll_flags="-chain ${flexdll_chain} ${flexlink_flags}"
      mkdll_exp="flexlink ${mkdll_flags}"
      mkdll="flexlink ${mkdll_flags}"
      mkmaindll_exp="flexlink -maindll ${mkdll_flags}"
      mkmaindll="flexlink -maindll ${mkdll_flags}"],
    [powerpc-ibm-aix*],
      [AS_CASE([$ocaml_cc_vendor],
        [xlc*],
          [mkdll_flags='-qmkshrobj -G'
          supports_shared_libraries=true])],
    [*-*-solaris*],
      [sharedlib_cflags="-fPIC"
      mkdll_flags='-shared'
      rpath="-Wl,-rpath,"
      mksharedlibrpath="-Wl,-rpath,"
      supports_shared_libraries=true],
    [[*-*-linux*|*-*-freebsd[3-9]*|*-*-freebsd[1-9][0-9]*\
    |*-*-openbsd*|*-*-netbsd*|*-*-dragonfly*|*-*-gnu*|*-*-haiku*]],
      [sharedlib_cflags="-fPIC"
       AS_CASE([$ocaml_cc_vendor,$host],
         [gcc-*,powerpc-*-linux*],
           [mkdll_flags='-shared -mbss-plt'],
         [[*,i[3456]86-*]],
           # Disable DT_TEXTREL warnings on Linux and BSD i386
           # See https://github.com/ocaml/ocaml/issues/9800
           [mkdll_flags='-shared -Wl,-z,notext'],
         [mkdll_flags='-shared'])
       AS_CASE([$host],
         [[*-*-openbsd7.[3-9]|*-*-openbsd[89].*]],
           [mkdll_flags="${mkdll_flags} -Wl,--no-execute-only"])
      oc_ldflags="$oc_ldflags -Wl,-E"
      rpath="-Wl,-rpath,"
      mksharedlibrpath="-Wl,-rpath,"
      natdynlinkopts="-Wl,-E"
      supports_shared_libraries=true],
      [mkdll='shared-libs-not-available'])
  AS_IF([test -z "$mkdll"],
    [mkdll="\$(CC) $mkdll_flags"
    mkdll_exp="$CC $mkdll_flags"])])

AS_IF([test -z "$mkmaindll"],
  [mkmaindll_exp="$mkdll_exp"
  mkmaindll="$mkdll"])

# Make sure code sections in OCaml-generated executables are readable
# (required for marshaling of function closures)

AS_CASE([$host],
  [[*-*-openbsd7.[3-9]|*-*-openbsd[89].*]],
    [oc_ldflags="$oc_ldflags -Wl,--no-execute-only"
     natdynlinkopts="$natdynlinkopts -Wl,--no-execute-only"])

# Disable control flow integrity

AS_CASE([$host],
  [[*-*-openbsd7.[4-9]|*-*-openbsd[89].*]],
    [oc_ldflags="$oc_ldflags -Wl,-z,nobtcfi"
     natdynlinkopts="$natdynlinkopts -Wl,-z,nobtcfi"])


# Configure native dynlink

natdynlink=false

AS_IF([test x"$supports_shared_libraries" = 'xtrue'],
  [AS_CASE(["$host"],
    [*-*-cygwin*], [natdynlink=true],
    [*-*-mingw32*], [natdynlink=true],
    [*-pc-windows], [natdynlink=true],
    [[i[3456]86-*-linux*]], [natdynlink=true],
    [[i[3456]86-*-gnu*]], [natdynlink=true],
    [[x86_64-*-linux*]], [natdynlink=true],
    [arm64-*-darwin*], [natdynlink=true],
    [aarch64-*-darwin*], [natdynlink=true],
    [x86_64-*-darwin*], [natdynlink=true],
    [s390x*-*-linux*], [natdynlink=true],
    [powerpc*-*-linux*], [natdynlink=true],
    [x86_64-*-solaris*], [natdynlink=true],
    [i686-*-kfreebsd*], [natdynlink=true],
    [x86_64-*-kfreebsd*], [natdynlink=true],
    [x86_64-*-dragonfly*], [natdynlink=true],
    [[i[3456]86-*-freebsd*]], [natdynlink=true],
    [x86_64-*-freebsd*], [natdynlink=true],
    [[i[3456]86-*-openbsd*]], [natdynlink=true],
    [x86_64-*-openbsd*], [natdynlink=true],
    [[i[3456]86-*-netbsd*]], [natdynlink=true],
    [x86_64-*-netbsd*], [natdynlink=true],
    [i386-*-gnu0.3], [natdynlink=true],
    [[i[3456]86-*-haiku*]], [natdynlink=true],
    [arm*-*-linux*], [natdynlink=true],
    [arm*-*-freebsd*], [natdynlink=true],
    [earm*-*-netbsd*], [natdynlink=true],
    [aarch64-*-linux*], [natdynlink=true],
    [aarch64-*-freebsd*], [natdynlink=true],
    [aarch64-*-openbsd*], [natdynlink=true],
    [aarch64-*-netbsd*], [natdynlink=true],
    [riscv*-*-linux*], [natdynlink=true])])

AS_CASE([$enable_native_toplevel,$natdynlink],
  [yes,false],
    [AC_MSG_ERROR(m4_normalize([
      The native toplevel requires native dynlink support]))],
  [yes,*],
    [install_ocamlnat=true],
  [install_ocamlnat=false])

# Try to work around the Skylake/Kaby Lake processor bug.
AS_CASE(["$ocaml_cc_vendor,$host"],
  [*gcc*,x86_64-*|*gcc*,i686-*],
    [AX_CHECK_COMPILE_FLAG([-fno-tree-vrp],
      [internal_cflags="$internal_cflags -fno-tree-vrp"], [],
      [$warn_error_flag])])

## Check whether __attribute__((optimize("tree-vectorize")))) is supported
OCAML_CC_SUPPORTS_TREE_VECTORIZE

# Check whether the C compiler supports the labels as values extension.
OCAML_CC_SUPPORTS_LABELS_AS_VALUES

# Configure the native-code compiler

arch=none
model=default
system=unknown

# In all the cases below, if $arch is set so then so too must $system,
# preserving $arch = 'none' <=> $system = 'unknown'
has_native_backend=no
native_ldflags=""
AS_CASE([$host],
  [[i[3456]86-*-linux*]],
    [arch=i386; system=linux],
  [[i[3456]86-*-freebsd*]],
    [arch=i386; system=freebsd],
  [[i[3456]86-*-netbsd*]],
    [arch=i386; system=netbsd],
  [[i[3456]86-*-openbsd*]],
    [arch=i386; system=openbsd],
  [[i[3456]86-*-haiku*]],
    [arch=i386; system=beos],
  [[i[3456]86-*-cygwin]],
    [arch=i386; system=cygwin],
  [[i[3456]86-*-gnu*]],
    [arch=i386; system=gnu],
  [[i[3456]86-*-mingw32*]],
    [arch=i386; system=mingw],
  [i686-pc-windows],
    [arch=i386; system=win32],
  [x86_64-pc-windows],
    [has_native_backend=yes; arch=amd64; system=win64],
  [[powerpc64le*-*-linux*]],
    [has_native_backend=yes; arch=power; model=ppc64le; system=linux],
  [[powerpc64*-*-linux-musl*]],
    [has_native_backend=yes; arch=power; model=ppc64; system=linux],
  [[s390x*-*-linux*]],
    [has_native_backend=yes; arch=s390x; model=z10; system=linux],
  # expected to match "gnueabihf" as well as "musleabihf"
  [armv6*-*-linux-*eabihf],
    [arch=arm; model=armv6; system=linux],
  [armv7*-*-linux-*eabihf],
    [arch=arm; model=armv7; system=linux],
  [armv8*-*-linux-*eabihf],
    [arch=arm; model=armv8; system=linux],
  [armv8*-*-linux-*eabi],
    [arch=arm; model=armv8; system=linux],
  [armv7*-*-linux-*eabi],
    [arch=arm; model=armv7; system=linux],
  [armv6t2*-*-linux-*eabi],
    [arch=arm; model=armv6t2; system=linux],
  [armv6*-*-linux-*eabi],
    [arch=arm; model=armv6; system=linux],
  [armv6*-*-freebsd*],
    [arch=arm; model=armv6; system=freebsd],
  [earmv6*-*-netbsd*],
    [arch=arm; model=armv6; system=netbsd],
  [earmv7*-*-netbsd*],
    [arch=arm; model=armv7; system=netbsd],
  [armv5te*-*-linux-*eabi],
    [arch=arm; model=armv5te; system=linux],
  [armv5*-*-linux-*eabi],
    [arch=arm; model=armv5; system=linux],
  [arm*-*-linux-*eabihf],
    [arch=arm; system=linux],
  [arm*-*-linux-*eabi],
    [arch=arm; system=linux],
  [arm*-*-openbsd*],
    [arch=arm; system=openbsd],
  [zaurus*-*-openbsd*],
    [arch=arm; system=openbsd],
  [x86_64-*-linux*],
    [has_native_backend=yes; arch=amd64; system=linux],
  [x86_64-*-gnu*],
    [arch=amd64; system=gnu],
  [x86_64-*-dragonfly*],
    [arch=amd64; system=dragonfly],
  [x86_64-*-solaris*],
    [has_native_backend=yes; arch=amd64; system=solaris],
  [x86_64-*-freebsd*],
    [has_native_backend=yes; arch=amd64; system=freebsd],
  [x86_64-*-netbsd*],
    [has_native_backend=yes; arch=amd64; system=netbsd],
  [x86_64-*-openbsd*],
    [has_native_backend=yes; arch=amd64; system=openbsd],
  [x86_64-*-haiku*],
    [arch=amd64; system=beos],
  [arm64-*-darwin*],
    [has_native_backend=yes; arch=arm64; system=macosx],
  [aarch64-*-darwin*],
    [has_native_backend=yes; arch=arm64; system=macosx],
  [x86_64-*-darwin*],
    [has_native_backend=yes; arch=amd64; system=macosx],
  [x86_64-*-mingw32*],
    [has_native_backend=yes; arch=amd64; system=mingw64],
  [aarch64-*-linux*],
    [AS_IF([$arch64],
       [has_native_backend=yes; arch=arm64; system=linux],
       [arch=arm; model=armv8; system=linux])],
  [aarch64-*-freebsd*],
    [has_native_backend=yes; arch=arm64; system=freebsd],
  [aarch64-*-openbsd*],
    [has_native_backend=yes; arch=arm64; system=openbsd],
  [aarch64-*-netbsd*],
    [has_native_backend=yes; arch=arm64; system=netbsd],
  [x86_64-*-cygwin*],
    [has_native_backend=yes; arch=amd64; system=cygwin],
  [riscv64-*-linux*],
    [has_native_backend=yes; arch=riscv; model=riscv64; system=linux]
)

AS_CASE([$arch],
  [amd64],
    [arch_specific_SOURCES='$(intel_SOURCES)'],
  [arch_specific_SOURCES=''])

native_cflags=''
oc_native_cflags=''
oc_native_cppflags="-DNATIVE_CODE\
 -DTARGET_${arch} -DMODEL_${model} -DSYS_${system}"
AS_CASE([$ccomptype],
  [msvc],
    [runtime_asm_objects=${arch}nt.${OBJEXT}],
  [runtime_asm_objects=${arch}.${OBJEXT}])

AS_CASE([$enable_native_compiler,$has_native_backend],
  [no,*],
    [native_compiler=false
    AC_MSG_NOTICE([the native compiler is disabled])],
  [yes,no],
    [AC_MSG_ERROR([the native compiler is not supported on this platform], 69)],
  [*,no],
    [native_compiler=false
    AC_MSG_NOTICE([the native compiler is not supported on this platform])],
  [native_compiler=true])

AS_IF([$native_compiler],
  [default_build_target=world.opt],
  [default_build_target=world])

AS_IF([! $native_compiler], [natdynlink=false])

AS_IF([$natdynlink], [cmxs="cmxs"], [cmxs="cmx"])
AS_IF([$natdynlink],
  [natdynlink_archive="dynlink.cmxa"],
  [natdynlink_archive=""])

AC_DEFINE_UNQUOTED([OCAML_OS_TYPE], ["$ostype"])

AC_CHECK_TOOL([DIRECT_LD],[ld])
AS_IF([test -z "$PARTIALLD"],
  [AS_CASE(["$host,$ocaml_cc_vendor"],
    [x86_64-*-darwin*,gcc-*], [PACKLD_FLAGS=' -arch x86_64'],
    [powerpc64le*-*-linux*,gcc-*], [PACKLD_FLAGS=' -m elf64lppc'],
    [powerpc*-*-linux*,gcc-*],
       [AS_IF([$arch64],
              [PACKLD_FLAGS=' -m elf64ppc'],
              [PACKLD_FLAGS=' -m elf32ppclinux'])],
    [PACKLD_FLAGS=''])
  # The string for PACKLD must be capable of being concatenated with the
  # output filename. Don't assume that all C compilers understand GNU -ofoo
  # form, so ensure that the definition includes a space at the end (which is
  # achieved using the $(EMPTY) expansion trick in Makefile.config.in).
  AS_CASE(["$ocaml_cc_vendor"],
    [msvc-*],
      # For the Microsoft C compiler there must be no space at the end of the
      # string.
      [PACKLD="link -lib -nologo $machine -out:"],
    [PACKLD="$DIRECT_LD -r$PACKLD_FLAGS -o "])],
  [PACKLD="$PARTIALLD -o "])

# Disable PIE at link time when ocamlopt does not produce position-independent
# code and the system produces PIE executables by default and demands PIC
# object files to do so.
# This issue does not affect amd64 (x86_64), aarch64 (arm64)
# and s390x (Z systems), since ocamlopt produces PIC object files by default.
# Currently the problem is known for Alpine Linux on other platforms
# (issue #7562), and probably affects all Linux distributions that use
# the musl standard library and dynamic loader.
# Other systems have PIE by default but can cope with non-PIC object files,
# e.g. Ubuntu >= 17.10 for i386, which uses the glibc dynamic loader.

AS_CASE([$arch],
  [amd64|arm64|s390x|none],
    # ocamlopt generates PIC code or doesn't generate code at all
    [],
  [AS_CASE([$host],
    # expected to match "*-linux-musl" as well as "*-linux-musleabi*"
    [*-linux-musl*],
      # Alpine and other musl-based Linux distributions
      [common_cflags="-no-pie $common_cflags"])])

# Assembler

AS_IF([test -n "$target_alias"],
  [toolpref="${target_alias}-"
  as_target="$target"
  as_cpu="$target_cpu"],
  [AS_IF([test -n "$host_alias"],
    [toolpref="${host_alias}-"
    as_target="$host"
    as_cpu="$host_cpu"],
    [toolpref=""
    as_target="$build"
    as_cpu="$build_cpu"])])

# Finding the assembler
# The OCaml build system distinguishes two different assemblers:
# 1. AS, used to assemble the code generated by the ocamlopt native compiler
# 2. ASPP, to assemble other assembly files that may require preprocessing
# In general, "$CC -c" is used as a default value for both AS and ASPP.
# On a few platforms (Windows) both values are overridden.
# On other platforms, (Linux with GCC) the assembler AS is called directly
# to avoiding forking a C compiler process for each compilation by ocamlopt.
# Both AS and ASPP can be overridden by the user.

default_as="$CC -c"
default_aspp="$CC -c"

AS_CASE([$as_target,$ocaml_cc_vendor],
  [*-*-linux*,gcc-*],
    [AS_CASE([$as_cpu],
      [x86_64|arm*|aarch64*|i[[3-6]]86|riscv*],
        [default_as="${toolpref}as"])],
  [i686-pc-windows,*],
    [default_as="ml -nologo -coff -Cp -c -Fo"
    default_aspp="$default_as"],
  [x86_64-pc-windows,*],
    [default_as="ml64 -nologo -Cp -c -Fo"
    default_aspp="$default_as"],
  [*-*-darwin*,clang-*],
    [default_as="$default_as -Wno-trigraphs"
    default_aspp="$default_as"])

AS_IF([test "$with_pic"],
  [fpic=true
  AC_DEFINE([CAML_WITH_FPIC], [1])
  internal_cflags="$internal_cflags $sharedlib_cflags"
  default_aspp="$default_aspp $sharedlib_cflags"],
  [fpic=false])

AS_IF([test -z "$AS"], [AS="$default_as"])

AS_IF([test -z "$ASPP"], [ASPP="$default_aspp"])

# Utilities
AC_CHECK_PROG([rlwrap],[rlwrap],[rlwrap])
AS_CASE([$rlwrap,$system],
  [rlwrap,win*|rlwrap,mingw*],
    [AC_MSG_NOTICE([rlwrap doesn't work with native win32 - disabling])
     rlwrap=''])

# Checks for library functions

## Check the semantics of signal handlers
OCAML_SIGNAL_HANDLERS_SEMANTICS

## Check for C99 float ops

has_c99_float_ops=true
AC_CHECK_FUNCS(m4_normalize([expm1 log1p hypot fma exp2 log2 cbrt acosh asinh
  atanh erf erfc trunc round copysign]), [], [has_c99_float_ops=false])

AS_IF([$has_c99_float_ops],
  [AC_DEFINE([HAS_C99_FLOAT_OPS], [1])
  # Check whether round works (known bug in mingw-w64)
  OCAML_C99_CHECK_ROUND
  # Check whether fma works (regressed in mingw-w64 8.0.0; and present but
  # unimplemented in Cygwin64)
  OCAML_C99_CHECK_FMA],
  [AS_IF([test x"$enable_imprecise_c99_float_ops" != "xyes" ],
    [AC_MSG_ERROR(m4_normalize([C99 float ops unavailable, enable replacements
    with --enable-imprecise-c99-float-ops]))])])

## getentropy
AC_CHECK_FUNC([getentropy], [AC_DEFINE([HAS_GETENTROPY], [1])])

## getrusage
AC_CHECK_FUNC([getrusage], [AC_DEFINE([HAS_GETRUSAGE], [1])])

## times
AC_CHECK_FUNC([times], [AC_DEFINE([HAS_TIMES], [1])])

## secure_getenv and __secure_getenv

saved_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="-D_GNU_SOURCE $CPPFLAGS"

AC_CHECK_FUNC([secure_getenv],
  [AC_DEFINE([HAS_SECURE_GETENV], [1])],
  [AC_CHECK_FUNC([__secure_getenv], [AC_DEFINE([HAS___SECURE_GETENV], [1])])])

CPPFLAGS="$saved_CPPFLAGS"

## issetugid

AC_CHECK_FUNC([issetugid], [AC_DEFINE([HAS_ISSETUGID], [1])])

## Checking for monotonic clock source
## On Windows MSVC, QueryPerformanceCounter and QueryPerformanceFrequency
## are always available.
## On Unix platforms, we check for the appropriate POSIX feature-test macros.
## On MacOS clock_gettime's CLOCK_MONOTONIC flag is not actually monotonic.

AS_CASE([$host],
  [*-*-windows],
    [has_monotonic_clock=true],
  [*-apple-darwin*], [
    AC_CHECK_FUNCS([clock_gettime_nsec_np],
      [
        has_monotonic_clock=true
        AC_DEFINE([HAS_CLOCK_GETTIME_NSEC_NP], [1])
      ],
      [has_monotonic_clock=false])],
  [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([[
    #include <unistd.h>
    #include <time.h>
    #if !(defined(_POSIX_TIMERS) && defined(_POSIX_MONOTONIC_CLOCK)   \
       && _POSIX_MONOTONIC_CLOCK != (-1))
      #error "no monotonic clock source"
    #endif
    ]])],
    [
      has_monotonic_clock=true
      AC_DEFINE([HAS_POSIX_MONOTONIC_CLOCK], [1])
    ],
    [has_monotonic_clock=false])
  ]
)

# The instrumented runtime is built by default
# if the proper clock source is found.
# If asked via --enable-instrumented-runtime, configuration fails if the proper
# clock source is missing.
AS_IF([test "x$enable_instrumented_runtime" != "xno" ],
  [
    AS_CASE([$host],
    [sparc-sun-solaris*],
      [instrumented_runtime=false],
    [*-*-windows],
      [instrumented_runtime=true],
    [*-apple-darwin*], [
      AS_CASE([$enable_instrumented_runtime,$has_monotonic_clock],
        [*,true],
          [instrumented_runtime=true],
        [yes,false],
          [AC_MSG_ERROR(m4_normalize([Instrumented runtime support requested
            but no proper monotonic clock source was found.]))],
        [auto,false],
          [instrumented_runtime=false]
    )],
    [AC_SEARCH_LIBS([clock_gettime], [rt],
      [has_clock_gettime=true],
      [has_clock_gettime=false])
      AS_CASE(
        [$enable_instrumented_runtime,$has_clock_gettime,$has_monotonic_clock],
        [auto,false,*], [instrumented_runtime=false],
        [auto,*,false], [instrumented_runtime=false],
        [*,true,true],
          [
            instrumented_runtime=true
            AS_IF([test "x$ac_cv_search_clock_gettime" = "xnone required"],
              [instrumented_runtime_libs=""],
              [instrumented_runtime_libs=$ac_cv_search_clock_gettime]
            )
          ],
        [yes,false,*],
          [AC_MSG_ERROR(m4_normalize([Instrumented runtime support requested
            but clock_gettime is missing.]))],
        [yes,*,false],
          [AC_MSG_ERROR(m4_normalize([Instrumented runtime support requested
            but no proper monotonic clock source was found.]))]
      )]
    )]
)

# ThreadSanitizer support is currently only available for Linux/FreeBSD/macOS
# on amd64, Linux/macOS on arm64 and Linux on POWER, riscv64 and s390x.
AS_IF([test "x$enable_tsan" = "xyes" ],
  [AS_CASE(["$arch"],
    [amd64],
      [AS_CASE(["$system"],
        [freebsd|linux|macosx],
        [AS_CASE(["$ocaml_cc_vendor"],
          [gcc-*|clang-*], [tsan=true],
          [AC_MSG_ERROR(m4_normalize([thread sanitizer not supported with
            vendor=$ocaml_cc_vendor"]))]
         )],
        [AC_MSG_ERROR([thread sanitizer not supported on system $system])]
       )],
    [arm64],
      [AS_CASE(["$system"],
        [linux|macosx],
        [AS_CASE(["$ocaml_cc_vendor"],
          [gcc-*|clang-*], [tsan=true],
          [AC_MSG_ERROR(m4_normalize([thread sanitizer not supported with
            vendor=$ocaml_cc_vendor"]))]
         )],
        [AC_MSG_ERROR([thread sanitizer not supported on system $system])]
       )],
    [power],
      [AS_CASE(["$system"],
        [linux],
        [AS_CASE(["$ocaml_cc_vendor"],
          [gcc-*|clang-*], [tsan=true],
          [AC_MSG_ERROR(m4_normalize([thread sanitizer not supported with
            vendor=$ocaml_cc_vendor"]))]
         )],
        [AC_MSG_ERROR([thread sanitizer not supported on system $system])]
       )],
    [riscv],
      [AS_CASE(["$system"],
        [linux],
        [AS_CASE(["$ocaml_cc_vendor"],
          [gcc-*|clang-*], [tsan=true],
          [AC_MSG_ERROR(m4_normalize([thread sanitizer not supported with
            vendor=$ocaml_cc_vendor"]))]
         )],
        [AC_MSG_ERROR([thread sanitizer not supported on system $system])]
       )],
    [s390x],
      [AS_CASE(["$system"],
        [linux],
        [AS_CASE(["$ocaml_cc_vendor"],
          [gcc-*|clang-*], [tsan=true],
          [AC_MSG_ERROR(m4_normalize([thread sanitizer not supported with
            vendor=$ocaml_cc_vendor"]))]
         )],
        [AC_MSG_ERROR([thread sanitizer not supported on system $system])]
       )],
      [AC_MSG_ERROR([thread sanitizer not supported on arch $arch])]
  )],
  [tsan=false])

AS_IF([$tsan],
  [AC_MSG_NOTICE([using thread sanitizer with vendor=$ocaml_cc_vendor])

  # Check for libtsan library files (necessary on some Linux distributions)
  SAVED_LDFLAGS="$LDFLAGS"
  LDFLAGS="$LDFLAGS -fsanitize=thread"
  AC_LINK_IFELSE([AC_LANG_PROGRAM([void __tsan_func_entry(void *retaddr);],
      [__tsan_func_entry((void *)0);])],
    [],
    [AC_MSG_ERROR([libtsan is necessary for TSan but cannot be found.
                   Try installing it on your system.])])
  LDFLAGS="$SAVED_LDFLAGS"

  AS_CASE([$ocaml_cc_vendor],
    [gcc-[[0123456789]]-*|gcc-10-*|clang-*],
      [],
    [tsan_cflags="$tsan_cflags -Wno-tsan"])
  AS_CASE([$ocaml_cc_vendor],
    [gcc*],
      [tsan_distinguish_volatile_cflags="--param=tsan-distinguish-volatile=1"],
    [clang*],
      [tsan_distinguish_volatile_cflags="-mllvm -tsan-distinguish-volatile"])
  AX_CHECK_COMPILE_FLAG([-fsanitize=thread $tsan_distinguish_volatile_cflags],
    [],
    [AC_MSG_ERROR(m4_normalize([The C compiler does not support the
      `$tsan_distinguish_volatile_cflags' flag. Try upgrading to GCC >= 11, or
      to Clang >= 11.]))], [$warn_error_flag])
  tsan_cflags="$tsan_cflags $tsan_distinguish_volatile_cflags"
  oc_native_cppflags="$oc_native_cppflags $tsan_cppflags"
  oc_native_cflags="$oc_native_cflags $tsan_cflags"
  native_cflags="$native_cflags $tsan_cflags"
  native_ldflags="$native_ldflags $tsan_ldflags"
  tsan_native_runtime_c_sources="tsan"],
  [AC_MSG_NOTICE([not using thread sanitizer])
  tsan_native_runtime_c_sources=""]
)

# libunwind detection when TSan is enabled
libunwind_cppflags=""
libunwind_ldflags=""

AS_IF([$tsan],
  [AS_IF([test "$system" = "macosx"],
    [AS_IF([test x"$LIBUNWIND_CPPFLAGS" != x -o \
                 x"$LIBUNWIND_LDFLAGS" != x],
      [AC_MSG_WARN(m4_normalize([
        On MacOSX, specifying paths for libunwind headers or libraries is
        strongly discouraged. It is recommended to rely on the defaults
        provided by the configure script
      ]))])])

  AS_IF([test x"$LIBUNWIND_CPPFLAGS" != x],
    [libunwind_cppflags="$LIBUNWIND_CPPFLAGS"],
    [libunwind_cppflags=""])

  AS_CASE(["$system"],
    ["freebsd"], [libunwind_ldflags="-lgcc_eh"],
    ["macosx"], [libunwind_ldflags="-framework System"],
    [AS_CASE(["$arch"],
      ["amd64"], [libunwind_ldflags="-lunwind -lunwind-x86_64"],
      ["arm64"], [libunwind_ldflags="-lunwind -lunwind-aarch64"],
      ["power"], [libunwind_ldflags="-lunwind -lunwind-ppc64"],
      ["riscv"], [libunwind_ldflags="-lunwind -lunwind-riscv"],
      ["s390x"], [libunwind_ldflags="-lunwind -lunwind-s390x"])])

  AS_IF([test x"$LIBUNWIND_LDFLAGS" != x],
    [libunwind_ldflags="$LIBUNWIND_LDFLAGS $libunwind_ldflags"])

  oc_native_cppflags="$oc_native_cppflags $libunwind_cppflags"
  native_ldflags="$native_ldflags $libunwind_ldflags"

  OCAML_CHECK_LIBUNWIND

  AS_IF([! $libunwind_available],
    [AC_MSG_ERROR([libunwind is necessary for TSan but cannot be found])])

    # We need unwinding information at runtime, but since we use
    # -no_compact_unwind, we also need -keep_dwarf_unwind otherwise
    # the OS X linker will chuck away the DWARF-like (.eh_frame)
    # information.  (Older versions of OS X don't provide this.)

    AS_IF([$libunwind_available && test x"$system" = "xmacosx"],
      [extra_flags="-Wl,-keep_dwarf_unwind"
      mkexe="$mkexe $extra_flags"
      mksharedlib="$mksharedlib $extra_flags"])
  ])

## Sockets

## TODO: check whether the different libraries are really useful

sockets=true

AS_CASE([$host],
  [*-*-mingw32*],
    [cclibs="$cclibs -lws2_32"
    AC_SEARCH_LIBS([socket], [ws2_32])
    AC_CHECK_FUNC([socketpair], [AC_DEFINE([HAS_SOCKETPAIR], [1])])],
  [*-pc-windows],
    [cclibs="$cclibs ws2_32.lib"
    AC_SEARCH_LIBS([socket], [ws2_32])
    AC_CHECK_FUNC([socketpair], [AC_DEFINE([HAS_SOCKETPAIR], [1])])],
  [*-*-haiku],
    [cclibs="$cclibs -lnetwork"
    AC_SEARCH_LIBS([socket], [network])],
  [*-*-solaris*],
    [cclibs="$cclibs -lsocket -lnsl"
    AC_SEARCH_LIBS([socket], [socket])
    AC_SEARCH_LIBS([inet_ntop], [nsl])],
  [
    AC_CHECK_FUNCS(
      [socket socketpair bind listen accept connect],
      [],
      [sockets=false])
  ]
)

AS_IF([$sockets], [AC_DEFINE([HAS_SOCKETS], [1])])

## socklen_t

AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows],
    [AC_CHECK_TYPE([socklen_t], [AC_DEFINE([HAS_SOCKLEN_T], [1])], [],
      [#include <ws2tcpip.h>])],
  [AC_CHECK_TYPE([socklen_t], [AC_DEFINE([HAS_SOCKLEN_T], [1])], [],
    [#include <sys/socket.h>])])

AC_CHECK_FUNC([inet_aton], [AC_DEFINE([HAS_INET_ATON], [1])])

## Unix domain sockets support on Windows

AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows],
    [AC_CHECK_HEADERS([afunix.h], [AC_DEFINE([HAS_AFUNIX_H], [1])], [],
      [#include <winsock2.h>])])

## IPv6 support

ipv6=true

AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows],
    [AC_CHECK_TYPE(
      [struct sockaddr_in6], [], [ipv6=false], [#include <ws2tcpip.h>])],
  [AC_CHECK_TYPE(
    [struct sockaddr_in6], [], [ipv6=false],
[
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
]
  )]
)

AS_IF([$ipv6],
  [AC_CHECK_FUNCS([getaddrinfo getnameinfo inet_pton inet_ntop], [],
    [ipv6=false])])
AS_IF([$ipv6],
  [AC_DEFINE([HAS_IPV6], [1])])

AC_CHECK_FUNC([rewinddir], [AC_DEFINE([HAS_REWINDDIR], [1])])

AC_CHECK_FUNC([lockf], [AC_DEFINE([HAS_LOCKF], [1])])

AC_CHECK_FUNC([mkfifo], [AC_DEFINE([HAS_MKFIFO], [1])])

AC_CHECK_FUNC([getcwd], [AC_DEFINE([HAS_GETCWD], [1])])

AC_CHECK_FUNC([system], [AC_DEFINE([HAS_SYSTEM], [1])])

## utime
## Note: this was defined in config/s-nt.h but the autoconf macros do not
# seem to detect it properly on Windows so we hardcode the definition
# of HAS_UTIME on Windows but this will probably need to be clarified
AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows], [AC_DEFINE([HAS_UTIME], [1])],
  [AC_CHECK_HEADER([sys/types.h],
    [AC_CHECK_HEADER([utime.h],
      [AC_CHECK_FUNC([utime], [AC_DEFINE([HAS_UTIME], [1])])])])])

AC_CHECK_FUNC([utimes], [AC_DEFINE([HAS_UTIMES], [1])])

AC_CHECK_FUNC([fchmod],
  [AC_CHECK_FUNC([fchown], [AC_DEFINE([HAS_FCHMOD], [1])])])

AC_CHECK_FUNC([truncate],
  [AC_CHECK_FUNC([ftruncate], [AC_DEFINE([HAS_TRUNCATE], [1])])])

## select
AC_CHECK_FUNC([select],
  [AC_CHECK_TYPE([fd_set],
    [AC_DEFINE([HAS_SELECT], [1])], [], [
#include <sys/types.h>
#include <sys/select.h>
  ])])

AC_CHECK_FUNC([nanosleep], [AC_DEFINE([HAS_NANOSLEEP], [1])])

AC_CHECK_FUNC([symlink],
  [AC_CHECK_FUNC([readlink],
    [AC_CHECK_FUNC([lstat], [AC_DEFINE([HAS_SYMLINK], [1])])])])

AC_CHECK_FUNC([realpath], [AC_DEFINE([HAS_REALPATH], [1])])

# wait
AC_CHECK_FUNC(
  [waitpid],
  [
    wait=true
    AC_DEFINE([HAS_WAITPID], [1])
  ],
  [wait=false])

AC_CHECK_FUNC(
  [wait4],
  [
    has_wait=true
    AC_DEFINE([HAS_WAIT4], [1])
  ])

## getgroups
AC_CHECK_FUNC([getgroups], [AC_DEFINE([HAS_GETGROUPS], [1])])

## setgroups
AC_CHECK_FUNC([setgroups], [AC_DEFINE([HAS_SETGROUPS], [1])])

## initgroups
AC_CHECK_FUNC([initgroups], [AC_DEFINE([HAS_INITGROUPS], [1])])

## termios

AC_CHECK_HEADER([termios.h],
  [AC_CHECK_FUNC([tcgetattr],
    [AC_CHECK_FUNC([tcsetattr],
      [AC_CHECK_FUNC([tcsendbreak],
        [AC_CHECK_FUNC([tcflush],
          [AC_CHECK_FUNC([tcflow], [AC_DEFINE([HAS_TERMIOS], [1])])])])])])])

## setitimer

AC_CHECK_FUNC([setitimer],
  [
    setitimer=true
    AC_DEFINE([HAS_SETITIMER], [1])
  ],
  [setitimer=false])

## gethostname
# Note: detection fails on Windows so hardcoding the result
# (should be debugged later)
AS_CASE([$host],
  [*-*-mingw32*|*-pc-windows], [AC_DEFINE([HAS_GETHOSTNAME], [1])],
  [AC_CHECK_FUNC([gethostname], [AC_DEFINE([HAS_GETHOSTNAME], [1])])])

## uname

AC_CHECK_HEADER([sys/utsname.h],
  [AC_CHECK_FUNC([uname], [AC_DEFINE([HAS_UNAME], [1])])])

## gettimeofday

AC_CHECK_FUNC([gettimeofday],
  [
    gettimeofday=true
    AC_DEFINE([HAS_GETTIMEOFDAY], [1])
  ],
  [gettimeofday=false])

## mktime

AC_CHECK_FUNC([mktime], [AC_DEFINE([HAS_MKTIME], [1])])

## setsid

AS_CASE([$host],
  [*-cygwin|*-*-mingw32*|*-pc-windows], [],
  [AC_CHECK_FUNC([setsid], [AC_DEFINE([HAS_SETSID], [1])])])

## putenv

AC_CHECK_FUNC([putenv], [AC_DEFINE([HAS_PUTENV], [1])])

## setenv and unsetenv

AC_CHECK_FUNC([setenv],
  [AC_CHECK_FUNC([unsetenv], [AC_DEFINE([HAS_SETENV_UNSETENV], [1])])])

## newlocale() and <locale.h>
# Note: the detection fails on msvc so we hardcode the result
# (should be debugged later)
AS_CASE([$host],
  [*-pc-windows], [AC_DEFINE([HAS_LOCALE_H], [1])],
  [AC_CHECK_HEADER([locale.h],
    [AC_CHECK_FUNC([newlocale],
      [AC_CHECK_FUNC([freelocale],
        [AC_CHECK_FUNC([uselocale], [AC_DEFINE([HAS_LOCALE_H], [1])])])])])])

AC_CHECK_HEADER([xlocale.h],
  [AC_CHECK_FUNC([newlocale],
    [AC_CHECK_FUNC([freelocale],
      [AC_CHECK_FUNC([uselocale], [AC_DEFINE([HAS_XLOCALE_H], [1])])])])])

## strtod_l
# Note: not detected on MSVC so hardcoding the result
# (should be debugged later)
AS_CASE([$host],
  [*-pc-windows], [AC_DEFINE([HAS_STRTOD_L], [1])],
  [AC_CHECK_FUNC([strtod_l], [AC_DEFINE([HAS_STRTOD_L], [1])])])

## shared library support
AS_IF([$supports_shared_libraries],
  [AS_CASE([$host],
    [*-*-mingw32*|*-pc-windows|*-*-cygwin*],
      [DLLIBS=""],
    [AC_CHECK_FUNC([dlopen],
      [supports_shared_libraries=true DLLIBS=""],
      [AC_CHECK_LIB([dl], [dlopen],
        [supports_shared_libraries=true DLLIBS="-ldl $DLLIBS"],
        [supports_shared_libraries=false])])])],
  [supports_shared_libraries=false])

AS_IF([$supports_shared_libraries],
  [AC_MSG_NOTICE([Dynamic loading of shared libraries is supported.])
  AC_DEFINE([SUPPORT_DYNAMIC_LINKING], [1])],
  [AC_MSG_NOTICE([Dynamic loading of shared libraries is not supported.])])

## mmap

AC_CHECK_HEADER([sys/mman.h],
  [AC_CHECK_FUNC([mmap],
    [AC_CHECK_FUNC([munmap], [AC_DEFINE([HAS_MMAP], [1])])])])

## pwrite

AC_CHECK_FUNC([pwrite], [AC_DEFINE([HAS_PWRITE], [1])])

## -fdebug-prefix-map support by the C compiler
AS_CASE([$ocaml_cc_vendor,$host],
  [*,*-*-mingw32*], [cc_has_debug_prefix_map=false],
  [*,*-pc-windows], [cc_has_debug_prefix_map=false],
  [xlc*,powerpc-ibm-aix*], [cc_has_debug_prefix_map=false],
  [sunc*,sparc-sun-*], [cc_has_debug_prefix_map=false],
  [AX_CHECK_COMPILE_FLAG([-fdebug-prefix-map=old=new],
    [cc_has_debug_prefix_map=true], [cc_has_debug_prefix_map=false],
    [$warn_error_flag])])

## Does stat support nanosecond precision

stat_has_ns_precision=false
AC_CHECK_MEMBERS([struct stat.st_atim.tv_nsec, struct stat.st_atimespec.tv_nsec,
  struct stat.st_atimensec],
  [stat_has_ns_precision=true], [], [
    AC_INCLUDES_DEFAULT
    #include <sys/stat.h>])
AS_IF([$stat_has_ns_precision],
  [AC_MSG_NOTICE([stat supports nanosecond precision])],
  [AC_MSG_NOTICE([stat does not support nanosecond precision])])

# Number of arguments of gethostbyname_r

AX_FUNC_WHICH_GETHOSTBYNAME_R

AS_CASE([$ac_cv_func_which_gethostbyname_r],
  [six], [AC_DEFINE([HAS_GETHOSTBYNAME_R],[6])],
  [five], [AC_DEFINE([HAS_GETHOSTBYNAME_R],[5])],
  [three], [AC_MSG_WARN([OCaml does not support this variant])])

# Number of arguments of gethostbyaddr_r

AX_FUNC_WHICH_GETHOSTBYADDR_R

AS_CASE([$ac_cv_func_which_gethostbyaddr_r],
  [eight], [AC_DEFINE([HAS_GETHOSTBYADDR_R],[8])],
  [seven], [AC_DEFINE([HAS_GETHOSTBYADDR_R],[7])])

## mkstemp

AC_CHECK_FUNC([mkstemp], [AC_DEFINE([HAS_MKSTEMP], [1])])

## nice

AC_CHECK_FUNC([nice], [AC_DEFINE([HAS_NICE], [1])])

## dup3

AC_CHECK_FUNC([dup3], [AC_DEFINE([HAS_DUP3], [1])])

## pipe2

AC_CHECK_FUNC([pipe2], [AC_DEFINE([HAS_PIPE2], [1])])

## accept4

AC_CHECK_FUNC([accept4], [AC_DEFINE([HAS_ACCEPT4], [1])])

## getauxval

AC_CHECK_FUNC([getauxval], [AC_DEFINE([HAS_GETAUXVAL], [1])])

## shmat
AC_CHECK_HEADER([sys/shm.h],
  [
    AC_DEFINE([HAS_SYS_SHM_H], [1])
    AC_CHECK_FUNC([shmat], [AC_DEFINE([HAS_SHMAT], [1])])
  ])

## execvpe

AC_CHECK_FUNC([execvpe], [AC_DEFINE([HAS_EXECVPE], [1])])

## posix_spawn

AC_CHECK_HEADER([spawn.h],
  [AC_CHECK_FUNC([posix_spawn],
    [AC_CHECK_FUNC([posix_spawnp], [AC_DEFINE([HAS_POSIX_SPAWN], [1])])])])

AC_PATH_TOOL([PKG_CONFIG], [pkg-config], [false])

## ZSTD compression library

zstd_status=""
zstd_libs=""

AS_IF([test x"$with_zstd" != "xno"],
  # Try pkg-config first, as it gives the most reliable results
  [AS_IF([${PKG_CONFIG} libzstd 2>/dev/null],
    # Now check the version
    [AS_IF([${PKG_CONFIG} --atleast-version 1.4 libzstd],
      [zstd_libs=`${PKG_CONFIG} --libs libzstd`
       zstd_flags=`${PKG_CONFIG} --cflags libzstd`
       zstd_status="ok"],
      [zstd_status="zstd library too old: version 1.4 or later is needed"])],
  # Otherwise, try to find zstd the old way,
  # assuming it is installed in default places
    [AC_CHECK_LIB([zstd], [ZSTD_createCCtx],
      [AC_CHECK_DECL([ZSTD_compressStream2],
        [zstd_libs="-lzstd"
         zstd_flags=""
         zstd_status="ok"],
        [zstd_status="zstd library too old: version 1.4 or later is needed"],
        [[#include <zstd.h>]])],
      [zstd_status="zstd library not found"])])])

# When building the mingw-w64 port in Cygwin, it is very easy to have the
# library available, but not have the DLL in PATH. This then causes the build to
# fail as soon as ocamlrun is first executed. This check avoids automatically
# enabling zstd when the resulting executable doesn't actually work.
AS_CASE([$host],
  [*-w64-mingw32*|*-pc-windows],
    [check_zstd_runs=$host_runnable],
  [check_zstd_runs=false])

AS_IF([test x"$zstd_status" = "xok"],[
  AC_MSG_CHECKING([whether programs can be linked with zstd])
  OCAML_CC_SAVE_VARIABLES
  LIBS="$LIBS $zstd_libs"
  CFLAGS="$CFLAGS $zstd_flags"
  AC_LINK_IFELSE(
    [AC_LANG_SOURCE([[
#include <zstd.h>
int main(void) {
  return (ZSTD_versionNumber() == 0);
}
    ]])],
    [AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([whether programs linked with zstd can execute])
    AS_IF([$check_zstd_runs],
      [AS_IF([ac_fn_c_try_run LINENO],
        [AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])
        zstd_libs=''
        zstd_flags=''
        zstd_status=\
"programs linked with zstd do not appear to be executable."])],
      [AC_MSG_RESULT([skipped])])],
    [AC_MSG_RESULT([no])
    zstd_libs=''
    zstd_flags=''
    zstd_status="zstd found, but programs cannot be linked with it."])
  OCAML_CC_RESTORE_VARIABLES])

AS_IF([test x"$zstd_status" = "xok"],
  [AC_MSG_NOTICE([compressed compilation artefacts supported])
   internal_cppflags="$internal_cppflags $zstd_flags"
   AC_DEFINE([HAS_ZSTD], [1])],
  [AS_CASE(["$with_zstd"],
     [no],
       [],
     [yes],
       [AC_MSG_ERROR([$zstd_status])],
     [AC_MSG_WARN([$zstd_status])
      AC_MSG_WARN([compressed compilation artefacts not supported])])])

## Determine whether the debugger should/can be built

AS_CASE([$enable_ocamldebug],
  [no],
    [with_debugger=""
    build_ocamldebug=false
    AC_MSG_NOTICE([ocamldebug disabled])],
  [AS_IF([$sockets],
    [with_debugger="ocamldebug"
    build_ocamldebug=true
    optional_bytecode_tools="$optional_bytecode_tools debugger/ocamldebug"
    AC_MSG_NOTICE([ocamldebug supported])],
    [with_debugger=""
    build_ocamldebug=false
    AC_MSG_NOTICE([ocamldebug not supported])])
  ])

## Should the runtime with debugging support be built
AS_CASE([$enable_debug_runtime],
  [no], [debug_runtime=false],
  [debug_runtime=true])

## Determine how to link with the POSIX threads library

AS_CASE([$host],
  [*-*-mingw32*],
    [link_gcc_eh=''
     AC_CHECK_LIB([gcc_eh], [printf], [link_gcc_eh="-lgcc_eh"])
     PTHREAD_LIBS="-l:libpthread.a $link_gcc_eh"],
  [*-pc-windows],
    [PTHREAD_LIBS=''],
  [AX_PTHREAD(
    [
    # The two following lines add flags and libraries for pthread to the
    # global CFLAGS and LIBS variables. This means that all the subsequent
    # tests can rely on the assumption that pthread is enabled.
    CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
    LIBS="$LIBS $PTHREAD_LIBS"],
    [AC_MSG_ERROR(m4_normalize([POSIX threads are required but not supported on
      this platform]))])])

## pthread_getaffinity_np, args differ from GNU and BSD
AC_MSG_CHECKING([arguments for pthread_getaffinity_np])
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(
    [[#define _GNU_SOURCE
      #include <sched.h>
      #ifdef HAS_PTHREAD_NP_H
      #include <pthread_np.h>
      #endif
      #include <pthread.h>]],
    [[cpu_set_t cs;
      CPU_ZERO(&cs);
      CPU_COUNT(&cs);
      pthread_getaffinity_np(pthread_self(), sizeof(cs), &cs);]])],
  [AC_MSG_RESULT([GNU])
  AC_DEFINE([HAS_GNU_GETAFFINITY_NP], [1])],
  [AC_LINK_IFELSE(
    [AC_LANG_PROGRAM(
      [[#include <pthread.h>
        #include <pthread_np.h>
        #include <sys/cpuset.h>]],
      [[cpuset_t cs;
        /* Not every BSD has CPU_ZERO and CPU_COUNT (NetBSD) */
        CPU_ZERO(&cs);
        CPU_COUNT(&cs);
        pthread_getaffinity_np(pthread_self(), sizeof(cs), &cs);]])],
    [AC_MSG_RESULT([BSD])
    AC_DEFINE([HAS_BSD_GETAFFINITY_NP], [1])],
    [AC_MSG_RESULT([pthread_getaffinity_np not found])])])

## Activate the systhread library

AS_CASE([$enable_systhreads,$enable_unix_lib],
  [yes,no],
    [systhread_support=false
    AC_MSG_ERROR([the threads library requires the unix library])],
  [no,*|*,no],
    [systhread_support=false
    AC_MSG_NOTICE([the threads library is disabled])],
  [systhread_support=true
  AC_CONFIG_FILES([otherlibs/systhreads/META])
  otherlibraries="$otherlibraries systhreads"
  otherlibs="$otherlibs systhreads"
  lib_systhreads=true
  AC_MSG_NOTICE([the threads library is supported])])

## Does the assembler support debug prefix map and CFI directives
as_has_debug_prefix_map=false
asm_cfi_supported=false
AS_IF([$native_compiler],
  [AS_CASE([$host],
    [*-*-mingw32*|*-pc-windows], [],
    [OCAML_AS_HAS_DEBUG_PREFIX_MAP
    OCAML_AS_HAS_CFI_DIRECTIVES])])

## Frame pointers

AS_IF([test x"$enable_frame_pointers" = "xyes"],
  [AS_CASE([$host],
    [x86_64-*-linux*|x86_64-*-darwin*|aarch64-*-linux*|aarch64-*-darwin*],
     [AS_CASE([$ocaml_cc_vendor],
        [clang-*|gcc-*],
         [common_cflags="$common_cflags -g  -fno-omit-frame-pointer"
         frame_pointers=true
         AC_DEFINE([WITH_FRAME_POINTERS], [1])
         AC_MSG_NOTICE([using frame pointers])],
         [AC_MSG_ERROR([frame pointers not supported on this platform])]
      )],
    [AC_MSG_ERROR([frame pointers not supported on this platform])]
  )],
  [AC_MSG_NOTICE([not using frame pointers])
  frame_pointers=false])

## Check for mmap support for huge pages and contiguous heap
OCAML_MMAP_SUPPORTS_HUGE_PAGES

AC_DEFINE_UNQUOTED([HEADER_RESERVED_BITS], [$reserved_header_bits])

AS_IF([test x"$enable_installing_bytecode_programs" = "xno"],
  [install_bytecode_programs=false],
  [install_bytecode_programs=true])

AS_IF([test x"$enable_installing_source_artifacts" = "xno"],
  [install_source_artifacts=false],
  [install_source_artifacts=true])

AS_IF([test x"$enable_stdlib_manpages" != "xno"],
  [build_libraries_manpages=true],[build_libraries_manpages=false])

documentation_tool_cmd=''
AC_ARG_WITH([odoc],
  [AS_HELP_STRING([--with-odoc])],
  [AS_CASE([$withval],
    [yes],[documentation_tool='odoc'],
    [no],[documentation_tool='ocamldoc'],
    [documentation_tool_cmd="$withval"
    documentation_tool='odoc'])],
  [documentation_tool='ocamldoc'])
AS_IF([test "x$documentation_tool_cmd" = 'x']
 [documentation_tool_cmd="$documentation_tool"])

AS_IF([test x"$enable_ocamlobjinfo" != 'xno'],
  [build_ocamlobjinfo=true],
  [build_ocamlobjinfo=false])

AS_CASE([$enable_ocamltest,OCAML__DEVELOPMENT_VERSION],
  [yes,*|,true],
    [build_ocamltest=true
    ocamltest_target=ocamltest
    ocamltest_opt_target=ocamltest.opt
    ocamltest='ocamltest'
    testsuite_tools='testsuite/tools/codegen testsuite/tools/expect'
    optional_bytecode_tools="$optional_bytecode_tools $testsuite_tools"],
  [build_ocamltest=false
  ocamltest=''])

AS_IF([$build_ocamltest],
  [optional_libraries="$optional_libraries testsuite/lib/testing"
  ocamltest_unix_mod="ocamltest/ocamltest_unix_${ocamltest_unix_impl}.ml"
  AC_CONFIG_LINKS([
    ocamltest/ocamltest_unix.ml:${ocamltest_unix_mod}
  ])
  AC_CHECK_PROGS([DIFF], [patdiff diff], [none])
  DIFF_FLAGS=""
  flags=""
  AS_CASE([$DIFF],
    [diff], [flags="--strip-trailing-cr -u"],
    [none],
      [AC_MSG_ERROR([ocamltest requires a diff tool])])
  for flag in ${flags}; do
    AC_MSG_CHECKING([whether $DIFF supports $flag]);
    AS_IF([$DIFF $DIFF_FLAGS $flag $0 $0 > /dev/null 2>&1],
      [DIFF_FLAGS="$DIFF_FLAGS $flag"
      AC_MSG_RESULT([yes])],
      [AC_MSG_RESULT([no])])
  done])

AS_IF([test x"$enable_flambda" = "xyes"],
  [flambda=true
  AS_IF([test x"$enable_flambda_invariants" = "xyes"],
    [flambda_invariants=true],
    [flambda_invariants=false])],
  [flambda=false
  flambda_invariants=false])

AS_IF([$flambda],
  [CMX_MAGIC_NUMBER=CMX_FLAMBDA__MAGIC_NUMBER
  CMXA_MAGIC_NUMBER=CMXA_FLAMBDA__MAGIC_NUMBER],
  [CMX_MAGIC_NUMBER=CMX_CLAMBDA__MAGIC_NUMBER
  CMXA_MAGIC_NUMBER=CMXA_CLAMBDA__MAGIC_NUMBER])

AS_IF([test x"$enable_cmm_invariants" = "xyes"],
  [cmm_invariants=true],
  [cmm_invariants=false])

AS_IF([test x"$enable_flat_float_array" = "xno"],
  [flat_float_array=false],
  [AC_DEFINE([FLAT_FLOAT_ARRAY], [1])
  flat_float_array=true])

OCAML_MMAP_SUPPORTS_MAP_STACK
AS_IF([test x"$enable_mmap_map_stack" = "xyes"],
  [AS_IF([test x"$has_mmap_map_stack" = "xtrue"],
    [AS_CASE([$target],
       [*-freebsd*],
         [AC_MSG_ERROR([mmap MAP_STACK not supported on FreeBSD])],
       [with_mmap_map_stack=true;
        AC_DEFINE([USE_MMAP_MAP_STACK], [1])])],
    [AC_MSG_ERROR([mmap MAP_STACK requested but not found on $target])])
  ],
  [AS_CASE([$target],
    [*-openbsd*],
     [with_mmap_map_stack=true;
      AC_DEFINE([USE_MMAP_MAP_STACK], [1])
      AC_MSG_NOTICE([using MAP_STACK on OpenBSD due to stack checking])],
    [with_mmap_map_stack=false])
  ])

oc_native_compflags=''

AS_IF([test x"$enable_function_sections" = "xno"],
  [function_sections=false],
  [AS_CASE([$arch],
    [amd64|arm64|power|riscv|s390x], # not supported on arm32, see issue #9124.
     [AS_CASE([$target],
        [*-cygwin*|*-mingw*|*-windows|*-apple-darwin*],
          [function_sections=false;
           AC_MSG_NOTICE([No support for function sections on $target.])],
        [AS_CASE([$ocaml_cc_vendor],
          [gcc-[0123]-*|gcc-4-[01234567]],
            [function_sections=false;
            AC_MSG_NOTICE([Function sections are not
            supported in GCC prior to version 4.8.])],
          [clang-[012]-*|clang-3-[01234]],
            [function_sections=false;
            AC_MSG_NOTICE([Function sections are not supported
            in Clang prior to version 3.5.])],
          [gcc-*|clang-*],
            [function_sections=true;
            oc_native_compflags='-function-sections'
            internal_cflags="$internal_cflags -ffunction-sections";
            AC_DEFINE([FUNCTION_SECTIONS], [1])],
          [function_sections=false;
          AC_MSG_NOTICE([Function sections are not supported by
          $ocaml_cc_vendor.])])])],
    [function_sections=false]);
  AS_IF([test x"$function_sections" = "xfalse"],
    [AS_IF([test x"$enable_function_sections" = "xyes"],
      [AC_MSG_ERROR([Function sections are not supported.])],
      [AC_MSG_NOTICE([Disabling function sections.])])],
    [])])

AS_IF([test x"$with_afl" = "xyes"],
  [afl=true],
  [afl=false])

# Enable debugging support
# TODO: add a --disable-debug-info configure option that disables debugging
# info for both C and OCaml
AS_IF([test "$ccomptype" != "msvc"],
  [internal_cflags="-g $internal_cflags"])

oc_cflags="$common_cflags $internal_cflags"
oc_cppflags="$common_cppflags $internal_cppflags"

oc_bytecode_cflags="$oc_cflags"
oc_bytecode_cppflags="$oc_cppflags"

oc_native_cflags="$oc_cflags $oc_native_cflags"
native_cflags="$common_cflags $native_cflags"
oc_native_cppflags="$oc_cppflags $oc_native_cppflags"

bytecode_cflags="$common_cflags $sharedlib_cflags\
 $PTHREAD_CFLAGS $COMPILER_BYTECODE_CFLAGS"
native_cflags="$native_cflags $PTHREAD_CFLAGS $COMPILER_NATIVE_CFLAGS"

bytecode_cppflags="$common_cppflags $COMPILER_BYTECODE_CPPFLAGS"
native_cppflags="$common_cppflags $COMPILER_NATIVE_CPPFLAGS"

AS_CASE([$host],
  [*-*-mingw32*],
    [cclibs="$cclibs -lole32 -luuid -lversion -lshlwapi -lsynchronization"],
  [*-pc-windows],
    [# For whatever reason, flexlink includes -ladvapi32 and -lshell32 for
    # mingw-w64, but doesn't include advapi32.lib and shell32.lib for MSVC
    cclibs="$cclibs ole32.lib uuid.lib advapi32.lib shell32.lib version.lib \
shlwapi.lib synchronization.lib"])

AC_CONFIG_COMMANDS_PRE([cclibs="$cclibs $mathlib $DLLIBS $PTHREAD_LIBS"])

AS_IF([test x"$libdir" = x'${exec_prefix}/lib'],
  [libdir="$libdir"/ocaml])

AS_IF([test x"$mandir" = x'${datarootdir}/man'],
  [mandir='${prefix}/man'])

# Define default prefix correctly for the different Windows ports
AS_IF([test x"$prefix" = "xNONE"],
  [AS_CASE([$host],
    [i686-w64-mingw32*], [prefix='C:/ocamlmgw'],
    [x86_64-w64-mingw32*], [prefix='C:/ocamlmgw64'],
    [i686-pc-windows], [prefix='C:/ocamlms'],
    [x86_64-pc-windows], [prefix='C:/ocamlms64'])],
  [AS_IF([test x"$unix_or_win32" = "xwin32" \
          && test "$host_vendor-$host_os" != "$build_vendor-$build_os" ],
    [AS_CASE([$build],
      [*-pc-cygwin], [prefix="$(LC_ALL=C.UTF-8 cygpath -m "$prefix")"])])])

# Define a few macros that were defined in config/m-nt.h
# but whose value is not guessed properly by configure
# (all this should be understood and fixed)
AS_CASE([$host],
  [*-*-mingw32*],
    [AC_DEFINE([HAS_BROKEN_PRINTF], [1])
    AC_DEFINE([HAS_IPV6], [1])
    AC_DEFINE([HAS_NICE], [1])],
  [*-pc-windows],
    [AC_DEFINE([HAS_BROKEN_PRINTF], [1])
    AC_DEFINE([HAS_IPV6], [1])
    AC_DEFINE([HAS_NICE], [1])],
  [*-*-solaris*],
    # This is required as otherwise floats are printed
    # as "Infinity" and "Inf" instead of the expected "inf"
    [AC_DEFINE([HAS_BROKEN_PRINTF], [1])])

# Do not permanently cache the result of flexdll.h
unset ac_cv_header_flexdll_h

# Evaluate install directories
# (this is needed for the OCaml configuration module)
AC_CONFIG_COMMANDS_PRE([
  saved_exec_prefix="$exec_prefix"
  saved_prefix="$prefix"
  AS_IF([test "x$prefix" = "xNONE"],[prefix="$ac_default_prefix"])
  AS_IF([test "x$exec_prefix" = "xNONE"],[exec_prefix="$prefix"])
  eval "exec_prefix=\"$exec_prefix\""
  eval "ocaml_bindir=\"$bindir\""
  eval "ocaml_libdir=\"$libdir\""
  AS_IF([test x"$target_bindir" = 'x'],[target_bindir="$ocaml_bindir"])
  AS_IF([test x"$target_launch_method" = 'x'],
    [target_launch_method="$launch_method"])
  prefix="$saved_prefix"
  exec_prefix="$saved_exec_prefix"])

# Just before config.status is generated, determine the final values for MKEXE,
# MKDLL, MKMAINDLL and MKEXE_VIA_CC. The final variables controlling these are:
# $mkexe - the linking command and munged CFLAGS + any extra flexlink flags
# $mkexe_exp: same as above but expanded
# $mkdll - the full linking command for DLLs
# $mkdll_exp: same as above but expanded
# $mkmaindll - the full linking command for main program in DLL
# $oc_ldflags $oc_dll_ldflags are handled as $(OC_LDFLAGS) and
# $(OC_DLL_LDFLAGS) and can be overridden in the build.
AC_CONFIG_COMMANDS_PRE([
  # Construct $mkexe
  mkexe="$mkexe_cmd"
  mkexe_exp="$mkexe_cmd_exp"
  AS_IF([test -n "$mkexe_extra_flags"],
    [mkexe="$mkexe $mkexe_extra_flags"
    mkexe_exp="$mkexe_exp $mkexe_extra_flags"])
  # If $mkexe_ldflags_prefix is given, apply it to $oc_ldflags and $oc_dllflags
  # and ensure that LDFLAGS is passed using it. This is used for flexlink. where
  # linker flags need to be prefixed with -link.
  AS_IF([test -n "$mkexe_ldflags_prefix"],[
    AS_IF([test -n "$mkexedebugflag"],
      [mkexedebugflag="${mkexe_ldflags_prefix}${mkexedebugflag}"])
    mkdll_ldflags=""
    AS_IF([test -n "${LDFLAGS}"],
      [for flag in ${LDFLAGS}; do
        mkdll_ldflags="${mkdll_ldflags} ${mkexe_ldflags_prefix}${flag}"
      done
      mkdll_ldflags_exp="$mkdll_ldflags"])
    tmp_oc_ldflags=""
    AS_IF([test -n "${oc_ldflags}"],
      [for flag in ${oc_ldflags}; do
        tmp_oc_ldflags="$tmp_oc_ldflags ${mkexe_ldflags_prefix}${flag}"
      done
      mkdll_ldflags_exp="$mkdll_ldflags_exp $tmp_oc_ldflags"])
    tmp_oc_dll_ldflags=""
    AS_IF([test -n "${oc_dll_ldflags}"],
      [for flag in ${oc_dll_ldflags}; do
        tmp_oc_dll_ldflags="$tmp_oc_dll_ldflags ${mkexe_ldflags_prefix}${flag}"
      done
      mkdll_ldflags_exp="$mkdll_ldflags_exp $tmp_oc_dll_ldflags"])
    mkexe_ldflags=\
"\$(addprefix ${mkexe_ldflags_prefix},\$(OC_LDFLAGS))${mkdll_ldflags}"
    mkexe_ldflags_exp=\
"${tmp_oc_ldflags} ${mkdll_ldflags}"

    mkdll_ldflags="\$(addprefix ${mkexe_ldflags_prefix},\$(OC_DLL_LDFLAGS)) \
${mkdll_ldflags}"
  ],[
    mkdll_ldflags='$(OC_DLL_LDFLAGS) $(LDFLAGS)'
    mkdll_ldflags_exp="${oc_dll_ldflags}"
    AS_IF([test -n "${LDFLAGS}"],
      [mkdll_ldflags_exp="$mkdll_ldflags_exp $LDFLAGS"])
    mkexe_ldflags="\$(OC_LDFLAGS) \$(LDFLAGS)"
    mkexe_ldflags_exp="${oc_ldflags} ${LDFLAGS}"
  ])
  mkexe="$mkexe $mkexe_ldflags"
  mkexe_exp="$mkexe_exp $mkexe_ldflags_exp"
  mkdll="$mkdll $mkdll_ldflags"
  mkdll_exp="$mkdll_exp $mkdll_ldflags_exp"
  mkmaindll="$mkmaindll $mkdll_ldflags_exp"
  mkmaindll_exp="$mkmaindll_exp $mkdll_ldflags_exp"
  mkexe_via_cc_ldflags="${mkexe_via_cc_ldflags_prefix}"
  AS_IF([test -n "${oc_exe_ldflags}"],
    [mkexe_via_cc_ldflags="${mkexe_via_cc_ldflags}${oc_exe_ldflags} "])
  mkexe_via_cc_ldflags="${mkexe_via_cc_ldflags}\$(OC_LDFLAGS) \$(LDFLAGS)"
  # cl requires linker flags after the objects.
  AS_IF([test "$ccomptype" = 'msvc'],
    [mkexe_via_cc_ldflags=\
"/nologo \$(OUTPUTEXE)\$(1) \$(2) $mkexe_via_cc_ldflags"],
    [mkexe_via_cc_ldflags=\
"$mkexe_via_cc_ldflags \$(OUTPUTEXE)\$(1) \$(2)"])
])

AC_OUTPUT
