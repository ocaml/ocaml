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

# Require Autoconf 2.69 for repeatability in CI
AC_PREREQ([2.69])
AC_INIT([OCaml],
        [OCAML__VERSION],
        [caml-list@inria.fr],
        [ocaml],
        [http://www.ocaml.org])

AC_MSG_NOTICE([Configuring OCaml version AC_PACKAGE_VERSION])

# Configuration variables

## Command-line arguments passed to configure
CONFIGURE_ARGS="$*"

# Command to build executalbes
# In general this command is supposed to use the CFLAGs- and LDFLAGS-
# related variables (OC_CFLAGS and OC_LDFLAGS for ocaml-specific
# flags, CFLAGS and LDFLAGS for generic flags chosen by the user), but
# at the moment they are not taken into account on Windows, because
# flexlink, which is used to build executables on this platform, can
# not handle them.
mkexe="\$(CC) \$(OC_CFLAGS) \$(CFLAGS) \$(OC_LDFLAGS) \$(LDFLAGS)"

# Flags for building executable files with debugging symbols
mkexedebugflag="-g"
common_cflags=""
common_cppflags=""
internal_cflags=""
internal_cppflags=""
ocamlc_cflags=""
ocamlc_cppflags=""
oc_ldflags=""
oc_dll_ldflags=""
ostype="Unix"
SO="so"
toolchain="cc"
profinfo=false
profinfo_width=0
extralibs=
instrumented_runtime=false
instrumented_runtime_libs=""
bootstrapping_flexdll=false

# Information about the package

## Source directory
AC_CONFIG_SRCDIR([runtime/interp.c])

## Directory containing auxiliary scripts used during build
AC_CONFIG_AUX_DIR([build-aux])

## Output variables

AC_SUBST([CONFIGURE_ARGS])
AC_SUBST([native_compiler])
AC_SUBST([VERSION], [AC_PACKAGE_VERSION])
AC_SUBST([OCAML_DEVELOPMENT_VERSION], [OCAML__DEVELOPMENT_VERSION])
AC_SUBST([OCAML_RELEASE_EXTRA], [OCAML__RELEASE_EXTRA])
AC_SUBST([OCAML_VERSION_MAJOR], [OCAML__VERSION_MAJOR])
AC_SUBST([OCAML_VERSION_MINOR], [OCAML__VERSION_MINOR])
AC_SUBST([OCAML_VERSION_PATCHLEVEL], [OCAML__VERSION_PATCHLEVEL])
AC_SUBST([OCAML_VERSION_EXTRA], [OCAML__VERSION_EXTRA])
AC_SUBST([OCAML_VERSION_SHORT], [OCAML__VERSION_SHORT])
AC_SUBST([CC])
# Note: This is present for the flexdll bootstrap where it exposed as the old
# TOOLPREF variable. It would be better if flexdll where updated to require
# WINDRES instead.
AC_SUBST([DIRECT_CPP])
AC_SUBST([ac_tool_prefix])
AC_SUBST([exeext])
AC_SUBST([OBJEXT])
AC_SUBST([libext])
AC_SUBST([S])
AC_SUBST([SO])
AC_SUBST([arch])
AC_SUBST([arch64])
AC_SUBST([model])
AC_SUBST([system])
AC_SUBST([systhread_support])
AC_SUBST([unix_or_win32])
AC_SUBST([unixlib])
AC_SUBST([outputexe])
AC_SUBST([outputobj])
AC_SUBST([syslib])
AC_SUBST([extralibs])
AC_SUBST([fpic])
AC_SUBST([mkexe])
AC_SUBST([mkexedebugflag])
AC_SUBST([ccomptype])
AC_SUBST([toolchain])
AC_SUBST([oc_cflags])
AC_SUBST([oc_cppflags])
AC_SUBST([oc_ldflags])
AC_SUBST([oc_dll_ldflags])
AC_SUBST([bytecclibs])
AC_SUBST([nativecclibs])
AC_SUBST([ocamlc_cflags])
AC_SUBST([ocamlc_cppflags])
AC_SUBST([flexdir])
AC_SUBST([bootstrapping_flexdll])
AC_SUBST([long_shebang])
AC_SUBST([shebangscripts])
AC_SUBST([AR])
AC_SUBST([mklib])
AC_SUBST([supports_shared_libraries])
AC_SUBST([natdynlink])
AC_SUBST([natdynlinkopts])
AC_SUBST([cmxs])
AC_SUBST([debug_runtime])
AC_SUBST([instrumented_runtime])
AC_SUBST([instrumented_runtime_libs])
AC_SUBST([has_monotonic_clock])
AC_SUBST([otherlibraries])
AC_SUBST([cc_has_debug_prefix_map])
AC_SUBST([as_has_debug_prefix_map])
AC_SUBST([with_debugger]) # TODO: rename this variable
AC_SUBST([with_camltex])
AC_SUBST([ocamldoc])
AC_SUBST([documentation_tool])
AC_SUBST([documentation_tool_cmd])
AC_SUBST([ocamltest])
AC_SUBST([ASPP])
AC_SUBST([endianness])
AC_SUBST([AS])
AC_SUBST([asm_cfi_supported])
AC_SUBST([sharedlib_cflags])
AC_SUBST([rpath])
AC_SUBST([mksharedlib])
AC_SUBST([mkmaindll])
AC_SUBST([mksharedlibrpath])
AC_SUBST([install_bytecode_programs])
AC_SUBST([install_source_artifacts])
AC_SUBST([install_ocamlnat])
AC_SUBST([profinfo])
AC_SUBST([profinfo_width])
AC_SUBST([frame_pointers])
AC_SUBST([flambda])
AC_SUBST([flambda_invariants])
AC_SUBST([cmm_invariants])
AC_SUBST([windows_unicode])
AC_SUBST([flat_float_array])
AC_SUBST([function_sections])
AC_SUBST([afl])
AC_SUBST([force_safe_string])
AC_SUBST([default_safe_string])
AC_SUBST([flexdll_chain])
AC_SUBST([flexlink_flags])
AC_SUBST([PACKLD])
AC_SUBST([stdlib_manpages])
AC_SUBST([compute_deps])
AC_SUBST([naked_pointers])
AC_SUBST([naked_pointers_checker])

## Generated files

AC_CONFIG_FILES([Makefile.build_config])
AC_CONFIG_FILES([Makefile.config])
AC_CONFIG_FILES([stdlib/sys.ml])
AC_CONFIG_FILES([manual/src/version.tex])
AC_CONFIG_FILES([manual/src/html_processing/src/common.ml])
AC_CONFIG_FILES([tools/eventlog_metadata])
AC_CONFIG_HEADERS([runtime/caml/m.h])
AC_CONFIG_HEADERS([runtime/caml/s.h])
AC_CONFIG_HEADERS([runtime/caml/version.h])

# Definitions related to the version of OCaml
AC_DEFINE([OCAML_VERSION_MAJOR], [OCAML__VERSION_MAJOR])
AC_DEFINE([OCAML_VERSION_MINOR], [OCAML__VERSION_MINOR])
AC_DEFINE([OCAML_VERSION_PATCHLEVEL], [OCAML__VERSION_PATCHLEVEL])
m4_if([OCAML__VERSION_EXTRA],[], [],
  [AC_DEFINE([OCAML_VERSION_ADDITIONAL], ["][OCAML__VERSION_EXTRA]["])
  AC_DEFINE([OCAML_VERSION_EXTRA], ["][OCAML__VERSION_EXTRA]["])])
AC_DEFINE([OCAML_VERSION], [OCAML__VERSION_NUMBER])
AC_DEFINE([OCAML_VERSION_STRING], ["][OCAML__VERSION]["])

# Checks for system types

AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AS_CASE([$host],
  [*-pc-windows],
    [CC=cl
    ccomptype=msvc
    S=asm
    SO=dll
    outputexe=-Fe
    syslib='$(1).lib'],
  [i386-*-solaris*],
    [AC_MSG_ERROR([Building for 32 bits target is not supported. \
If your host is 64 bits, you can try with './configure CC="gcc -m64"' \
(or "cc -m64" if you don't have GCC).])],
  [ccomptype=cc
  S=s
  SO=so
  outputexe='-o $(EMPTY)'
  syslib='-l$(1)'])

# Environment variables that are taken into account

AC_ARG_VAR([AS], [which assembler to use])
AC_ARG_VAR([ASPP], [which assembler (with preprocessor) to use])
AC_ARG_VAR([PARTIALLD], [how to build partial (relocatable) object files])

# Command-line arguments to configure

AC_ARG_ENABLE([debug-runtime],
  [AS_HELP_STRING([--disable-debug-runtime],
    [do not build runtime with debugging support])])

AC_ARG_ENABLE([debugger],
  [AS_HELP_STRING([--enable-debugger],
    [build the debugger @<:@default=auto@:>@])],
  [],
  [enable_debugger=auto])

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
  [AC_MSG_ERROR([The vmthreads library is no longer available. \
It was deleted in OCaml 4.09.])],
  [])

AC_ARG_ENABLE([systhreads],
  [AS_HELP_STRING([--disable-systhreads],
    [disable the Win32/POSIX threads library])])

AC_ARG_ENABLE([graph-lib], [],
  [AC_MSG_ERROR([The graphics library is no longer distributed with OCaml \
since version 4.09. It is now distributed as a separate "graphics" package: \
https://github.com/ocaml/graphics])],
  [])

AC_ARG_ENABLE([str-lib],
  [AS_HELP_STRING([--disable-str-lib],
    [do not build the str library])])

AC_ARG_ENABLE([unix-lib],
  [AS_HELP_STRING([--disable-unix-lib],
    [do not build the unix library])])

AC_ARG_ENABLE([bigarray-lib],
  [AS_HELP_STRING([--disable-bigarray-lib],
    [do not build the legacy separate bigarray library])])

AC_ARG_ENABLE([ocamldoc],
  [AS_HELP_STRING([--disable-ocamldoc],
    [do not build the ocamldoc documentation system])],
  [],
  [ocamldoc=auto])

AC_ARG_WITH([odoc],
  [AS_HELP_STRING([--with-odoc],
    [build documentation with odoc])])


AC_ARG_ENABLE([ocamltest],
  [AS_HELP_STRING([--disable-ocamltest],
    [do not build the ocamltest driver])])

AC_ARG_ENABLE([native-toplevel],
  [AS_HELP_STRING([--enable-native-toplevel],
    [build the native toplevel])])

AC_ARG_ENABLE([frame-pointers],
  [AS_HELP_STRING([--enable-frame-pointers],
    [use frame pointers in runtime and generated code])])

AC_ARG_ENABLE([naked-pointers],
  [AS_HELP_STRING([--disable-naked-pointers],
    [do not allow naked pointers])])

AC_ARG_ENABLE([naked-pointers-checker],
  [AS_HELP_STRING([--enable-naked-pointers-checker],
    [enable the naked pointers checker])])

AC_ARG_ENABLE([spacetime], [],
  [AC_MSG_ERROR([spacetime profiling was deleted in OCaml 4.12.])],
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
    [location of binary programs on target system])])

AC_ARG_ENABLE([reserved-header-bits],
  [AS_HELP_STRING([--enable-reserved-header-bits=BITS],
  [reserve BITS (between 0 and 31) bits in block headers for profiling info])],
  [AS_CASE([$enable_reserved_header_bits],
    [0],
      [with_profinfo=false
      profinfo_width=0],
    [[[1-9]]|[[1-2]][[0-9]]|3[[0-1]]],
      [with_profinfo=true
      profinfo_width="$enable_reserved_header_bits"],
    [AC_MSG_ERROR([invalid argument to --enable-reserved-header-bits])])])

AC_ARG_ENABLE([stdlib-manpages],
  [AS_HELP_STRING([--disable-stdlib-manpages],
    [do not build or install the library man pages])])

AC_ARG_ENABLE([warn-error],
  [AS_HELP_STRING([--enable-warn-error],
    [treat C compiler warnings as errors])])

AC_ARG_VAR([WINDOWS_UNICODE_MODE],
  [how to handle Unicode under Windows: ansi, compatible])

# There are two configure-time string safety options,
# --(enable|disable)-force-safe-string and
# DEFAULT_STRING=safe|unsafe that
# interact with a compile-time (un)safe-string option.
#
# If --enable-force-safe-string is set at configure time, then the compiler
# will always enforce that string and bytes are distinct: the
# compile-time -unsafe-string option is disabled. This lets us
# assume pervasive string immutability, for code optimizations and
# in the C layer.
#
# If --disable-force-safe-string is set at configure-time, the compiler
# will use the compile-time (un)safe-string option to decide whether
# string and bytes are compatible on a per-file basis. The
# configuration variable DEFAULT_STRING=safe|unsafe decides which
# setting will be chosen by default, if no compile-time option is
# explicitly passed.
#
# The configure-time behavior of OCaml 4.05 and older was equivalent
# to --disable-force-safe-string DEFAULT_STRING=unsafe. With OCaml 4.06
# and older was equivalent to --disable-force-safe-string DEFAULT_STRING=safe.
# With OCaml 4.10 and later use --enable-force-safe-string DEFAULT_STRING=safe.
# We expect the --disable-force-safe-string and DEFAULT_STRING=unsafe options
# to be removed in the future.

AC_ARG_ENABLE([force-safe-string],
  [AS_HELP_STRING([--disable-force-safe-string],
    [do not force strings to be safe])])

AC_ARG_VAR([DEFAULT_STRING],
  [whether strings should be safe (default) or unsafe])

AC_ARG_ENABLE([flat-float-array],
  [AS_HELP_STRING([--disable-flat-float-array],
    [do not use flat float arrays])])

AC_ARG_ENABLE([function-sections],
  [AS_HELP_STRING([--disable-function-sections],
    [do not emit each function in a separate section])],
  [],
  [enable_function_sections=auto])

AC_ARG_WITH([afl],
  [AS_HELP_STRING([--with-afl],
    [use the AFL fuzzer])])

AC_ARG_WITH([flexdll],
  [AS_HELP_STRING([--with-flexdll],
    [bootstrap FlexDLL from the given sources])],
  [AS_IF([test x"$withval" = 'xyes'],[with_flexdll=flexdll])])

AS_IF([test x"$enable_unix_lib" = "xno"],
  [AS_IF([test x"$enable_debugger" = "xyes"],
    [AC_MSG_ERROR([replay debugger requires the unix library])],
    [enable_debugger="no"])
   AS_IF([test x"$enable_bigarray_lib" = "xyes"],
    [AC_MSG_ERROR([legacy bigarray library requires the unix library])])])

AS_IF([test x"$enable_unix_lib" = "xno" -o x"$enable_str_lib" = "xno"],
  [AS_IF([test x"$enable_ocamldoc" = "xyes"],
    [AC_MSG_ERROR([ocamldoc requires the unix and str libraries])],
    [enable_ocamldoc="no"
     with_camltex=""])],
  [with_camltex="true"])

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
AS_CASE([$ocaml_cv_cc_vendor],
  [xlc-*],
    [CPP="$CC -E -qnoppline"], # suppress incompatible XLC line directives
  [sunc-*],
    [CPP="$CC -E -Qn"], # suppress generation of Sun PRO ident string
  [msvc-*],
    [CPP="$CC -nologo -EP"])

# Libraries to build depending on the host

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [unix_or_win32="win32"
    unixlib="win32unix"
    ],
  [unix_or_win32="unix"
  unixlib="unix"])
AS_CASE([$host],
  [*-*-cygwin*|*-*-mingw32|*-pc-windows],
    [exeext=".exe"],
  [exeext=''])

otherlibraries="dynlink"
AS_IF([test x"$enable_unix_lib" != "xno"],
  [enable_unix_lib=yes
  AS_IF([test x"$enable_bigarray_lib" != "xno"],
    [otherlibraries="$otherlibraries $unixlib bigarray"],
    [otherlibraries="$otherlibraries $unixlib"])])
AS_IF([test x"$enable_str_lib" != "xno"],
  [otherlibraries="$otherlibraries str"])

# Checks for system services

## Test whether #! scripts are supported
## TODO: have two values, one for host and one for target
AC_SYS_INTERPRETER

long_shebang=false
AS_IF(
  [test "x$interpval" = "xyes"],
    [AS_CASE([$host],
      [*-cygwin|*-*-mingw32|*-pc-windows],
        [shebangscripts=false],
      [shebangscripts=true
       prev_exec_prefix="$exec_prefix"
       AS_IF([test "x$exec_prefix" = "xNONE"],[exec_prefix="$prefix"])
       eval "expanded_bindir=\"$bindir\""
       exec_prefix="$prev_exec_prefix"
       # Assume maximum shebang is 128 chars; less #!, /ocamlrun, an optional
       # 1 char suffix and the \0 leaving 115 characters
       AS_IF([test "${#expanded_bindir}" -gt 115],[long_shebang=true])
      ]
    )],
  [shebangscripts=false]
)

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

AS_CASE([$ocaml_cv_cc_vendor],
  [xlc-*],
    [outputobj='-o $(EMPTY)'
    warn_error_flag=''
    cc_warnings='-qflag=i:i'], # all warnings enabled
  [sunc-*],
    [outputobj='-o $(EMPTY)'; cc_warnings=""],
  [msvc-*],
    [outputobj='-Fo'
    warn_error_flag='-WX'
    cc_warnings=''],
  [outputobj='-o $(EMPTY)'
  warn_error_flag='-Werror'
  cc_warnings='-Wall -Wdeclaration-after-statement'])

AS_CASE([$enable_warn_error,OCAML__DEVELOPMENT_VERSION],
  [yes,*|,true],
    [cc_warnings="$cc_warnings $warn_error_flag"])

# We select high optimization levels, provided we can turn off:
# - strict type-based aliasing analysis (too risky for the OCaml runtime)
# - strict no-overflow conditions on signed integer arithmetic
#   (the OCaml runtime assumes Java-style behavior of signed integer arith.)
# Concerning optimization level, -O3 is somewhat risky, so take -O2.
# Concerning language version, gnu99 is ISO C99 plus GNU extensions
# that are often used in standard headers.  Older GCC versions
# defaults to gnu89, which is not C99.  Clang defaults to gnu99 or
# gnu11, which is fine.

# Note: the vendor macro can not recognize MinGW because it calls the
# C preprocessor directly so no compiler specific macro like __MING32__
# is defined. We thus catch MinGW first by looking at host and examine
# the vendor only as a fall-back. We could put tis part of the logic
# in the macro itself, too
AS_CASE([$host],
  [*-*-mingw32],
    [AS_CASE([$ocaml_cv_cc_vendor],
      [gcc-[[01234]]-*],
        [AC_MSG_ERROR(m4_normalize([This version of Mingw GCC is too old.
          Please use GCC version 5 or above.]))],
      [gcc-*],
        [internal_cflags="-Wno-unused $cc_warnings \
-fexcess-precision=standard"
        # TODO: see whether the code can be fixed to avoid -Wno-unused
        common_cflags="-O2 -fno-strict-aliasing -fwrapv -mms-bitfields"
        internal_cppflags='-D__USE_MINGW_ANSI_STDIO=0 -DUNICODE -D_UNICODE'
        internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE="
        internal_cppflags="${internal_cppflags}\$(WINDOWS_UNICODE)"],
      [AC_MSG_ERROR([Unsupported C compiler for a Mingw build])])],
  [AS_CASE([$ocaml_cv_cc_vendor],
    [clang-*],
      [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
      internal_cflags="$cc_warnings -fno-common"],
    [gcc-[[012]]-*],
      # Some versions known to miscompile OCaml, e,g, 2.7.2.1, some 2.96.
      # Plus: C99 support unknown.
      [AC_MSG_ERROR(m4_normalize([This version of GCC is too old.
        Please use GCC version 4.2 or above.]))],
    [gcc-3-*|gcc-4-[[01]]],
      # No -fwrapv option before GCC 3.4.
      # Known problems with -fwrapv fixed in 4.2 only.
      [AC_MSG_WARN(m4_normalize([This version of GCC is rather old.
        Reducing optimization level."]));
      AC_MSG_WARN([Consider using GCC version 4.2 or above.]);
      common_cflags="-std=gnu99 -O";
      internal_cflags="$cc_warnings"],
    [gcc-4-[[234]]],
      # No -fexcess-precision option before GCC 4.5
      [common_cflags="-std=gnu99 -O2 -fno-strict-aliasing -fwrapv \
-fno-builtin-memcmp";
      internal_cflags="$cc_warnings"],
    [gcc-4-*],
      [common_cflags="-std=gnu99 -O2 -fno-strict-aliasing -fwrapv \
-fno-builtin-memcmp";
      internal_cflags="$cc_warnings -fexcess-precision=standard"],
    [gcc-*],
      [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
      internal_cflags="$cc_warnings -fno-common \
-fexcess-precision=standard"],
    [msvc-*],
      [common_cflags="-nologo -O2 -Gy- -MD $cc_warnings"
      common_cppflags="-D_CRT_SECURE_NO_DEPRECATE"
      internal_cppflags='-DUNICODE -D_UNICODE'
      OCAML_CL_HAS_VOLATILE_METADATA
      AS_IF([test "x$cl_has_volatile_metadata" = "xtrue"],
            [internal_cflags='-d2VolatileMetadata-'])
      internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE="
      internal_cppflags="${internal_cppflags}\$(WINDOWS_UNICODE)"],
    [xlc-*],
      [common_cflags="-O5 -qtune=balanced -qnoipa -qinline";
      internal_cflags="$cc_warnings"],
    [sunc-*], # Optimization should be >= O4 to inline functions
              # and prevent unresolved externals
      [common_cflags="-O4 -xc99=all -D_XPG6 $CFLAGS";
      internal_cflags="$cc_warnings"],
    [common_cflags="-O"])])

internal_cppflags="-DCAML_NAME_SPACE $internal_cppflags"

# Enable SSE2 on x86 mingw to avoid using 80-bit registers.
AS_CASE([$host],
  [i686-*-mingw32],
    [internal_cflags="$internal_cflags -mfpmath=sse -msse2"])

# Use 64-bit file offset if possible
# See also AC_SYS_LARGEFILE
# Problem: flags are added to CC rather than CPPFLAGS
AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [],
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
    [*-pc-windows|*-w64-mingw32],
    [AC_MSG_ERROR([Cannot build native Win32 with --disable-shared])])],
  [supports_shared_libraries=true])

# Define flexlink chain and flags correctly for the different Windows ports
AS_CASE([$host],
  [i686-*-cygwin],
    [flexdll_chain='cygwin'
    flexlink_flags="-chain $flexdll_chain -merge-manifest -stack 16777216"],
  [x86_64-*-cygwin],
    [flexdll_chain='cygwin64'
    flexlink_flags="-chain $flexdll_chain -merge-manifest -stack 16777216"],
  [*-*-cygwin*],
    [AC_MSG_ERROR([unknown cygwin variant])],
  [i686-w64-mingw32],
    [flexdll_chain='mingw'
    flexlink_flags="-chain $flexdll_chain -stack 16777216"],
  [x86_64-w64-mingw32],
    [flexdll_chain='mingw64'
    flexlink_flags="-chain $flexdll_chain -stack 33554432"],
  [i686-pc-windows],
    [flexdll_chain='msvc'
    flexlink_flags="-merge-manifest -stack 16777216"],
  [x86_64-pc-windows],
    [flexdll_chain='msvc64'
    flexlink_flags="-x64 -merge-manifest -stack 33554432"])

AS_IF([test x"$supports_shared_libraries" != 'xfalse'], [
  AC_MSG_CHECKING([for flexdll sources])
  AS_IF([test x"$with_flexdll" = "xno"],
    [flexdir=''
    AC_MSG_RESULT([disabled])],
    [flexmsg=''
    AS_CASE([$target],
      [*-*-cygwin*|*-w64-mingw32|*-pc-windows],
      [AS_IF([test x"$with_flexdll" = 'x' -o x"$with_flexdll" = 'xflexdll'],
        [AS_IF([test -f 'flexdll/flexdll.h'],
          [flexdir=flexdll
          iflexdir='$(ROOTDIR)/flexdll'
          with_flexdll="$iflexdir"],
          [AS_IF([test x"$with_flexdll" != 'x'],
            [AC_MSG_RESULT([requested but not available])
            AC_MSG_ERROR([exiting])])])],
        [rm -rf flexdll-sources
        AS_IF([test -f "$with_flexdll/flexdll.h"],
          [mkdir -p flexdll-sources
          cp -r "$with_flexdll"/* flexdll-sources/
          flexdir='flexdll-sources'
          iflexdir='$(ROOTDIR)/flexdll-sources'
          flexmsg=" (from $with_flexdll)"],
          [AC_MSG_RESULT([requested but not available])
          AC_MSG_ERROR([exiting])])])
      AS_IF([test x"$flexdir" = 'x'],
        [AC_MSG_RESULT([no])],
        [AC_MSG_RESULT([$iflexdir$flexmsg])
        bootstrapping_flexdll=true
        # The submodule should be searched *before* any other -I paths
        internal_cppflags="-I $iflexdir $internal_cppflags"])],
      [AS_IF([test x"$with_flexdll" != 'x'],
        [AC_MSG_RESULT([requested but not supported])
        AC_MSG_ERROR([exiting])])])])

  AC_CHECK_PROG([flexlink],[flexlink],[flexlink])

  AS_IF([test -n "$flexlink" -a -z "$flexdir"],[
    OCAML_TEST_FLEXLINK([$flexlink], [$flexdll_chain],
                        [$internal_cppflags], [$host])

    AS_CASE([$host],
      [*-w64-mingw32|*-pc-windows],
      [flexlink_where="$(cmd /c "$flexlink" -where 2>/dev/null)"
      AS_IF([test -z "$flexlink_where"],
        [AC_MSG_ERROR([$flexlink is not executable from a native Win32 process])
      ])])
  ])

  OCAML_TEST_FLEXDLL_H([$flexdir])

  AS_IF([test -n "$flexlink" -a x"$have_flexdll_h" = 'xno'],
    [OCAML_TEST_FLEXLINK_WHERE([$flexlink])
    AS_IF([test "x$have_flexdll_h" = 'xyes'],
      [internal_cppflags="$internal_cppflags -I \"$flexlink_where\""])
  ])
])

AS_CASE([$have_flexdll_h,$supports_shared_libraries,$host],
 [no,true,*-*-cygwin*],
   [supports_shared_libraries=false
   AC_MSG_WARN([flexdll.h not found: shared library support disabled.])],
 [no,*,*-w64-mingw32|no,*,*-pc-windows],
   [AC_MSG_ERROR([flexdll.h is required for native Win32])])

AS_CASE([$flexdir,$supports_shared_libraries,$flexlink,$host],
  [,true,,*-*-cygwin*],
    [supports_shared_libraries=false
    AC_MSG_WARN([flexlink not found: shared library support disabled.])],
  [,*,,*-w64-mingw32|,*,,*-pc-windows],
    [AC_MSG_ERROR([flexlink is required for native Win32])])

AS_CASE([$cc_basename,$host],
  [*,*-*-darwin*],
    [mkexe="$mkexe -Wl,-no_compact_unwind";
    AC_DEFINE([HAS_ARCH_CODE32], [1])],
  [*,*-*-haiku*], [mathlib=""],
  [*,*-*-cygwin*],
    [common_cppflags="$common_cppflags -U_WIN32"
    AS_IF([$supports_shared_libraries],
      [mkexe='$(FLEXLINK) -exe $(if $(OC_LDFLAGS),-link "$(OC_LDFLAGS)")'
      mkexedebugflag="-link -g"],
      [mkexe="$mkexe -Wl,--stack,16777216"
      oc_ldflags="-Wl,--stack,16777216"]
    )
    ostype="Cygwin"],
  [*,*-*-mingw32],
    [AS_CASE([$host],
      [i686-*-*], [oc_dll_ldflags="-static-libgcc"])
    mkexedebugflag="-link -g"
    ostype="Win32"
    toolchain="mingw"
    mkexe='$(FLEXLINK) -exe $(if $(OC_LDFLAGS),-link "$(OC_LDFLAGS)")'
    oc_ldflags='-municode'
    SO="dll"],
  [*,*-pc-windows],
    [toolchain=msvc
    ostype="Win32"
    mkexe='$(FLEXLINK) -exe $(if $(OC_LDFLAGS),-link "$(OC_LDFLAGS)")'
    oc_ldflags='/ENTRY:wmainCRTStartup'
    mkexedebugflag=''],
  [*,x86_64-*-linux*],
    AC_DEFINE([HAS_ARCH_CODE32], [1]),
  [xlc*,powerpc-ibm-aix*],
    [mkexe="$mkexe "
     oc_ldflags="-brtl -bexpfull"
    AC_DEFINE([HAS_ARCH_CODE32], [1])],
  [gcc*,powerpc-*-linux*],
    [oc_ldflags="-mbss-plt"],
)


## Program to use to install files
AC_PROG_INSTALL

# Checks for libraries

## Mathematical library
AC_CHECK_LIB([m],[cos])

AS_IF([test "x$ac_cv_lib_m_cos" = xyes ], [mathlib="-lm"], [mathlib=""])

# Checks for header files

AC_CHECK_HEADER([math.h])
AC_CHECK_HEADERS([unistd.h],[AC_DEFINE([HAS_UNISTD])])
AC_CHECK_HEADER([stdint.h],[AC_DEFINE([HAS_STDINT_H])])
AC_CHECK_HEADER([dirent.h], [AC_DEFINE([HAS_DIRENT])], [],
  [#include <sys/types.h>])

AC_CHECK_HEADER([sys/select.h], [AC_DEFINE([HAS_SYS_SELECT_H])], [],
  [#include <sys/types.h>])

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

AS_IF([! $arch64],
  [AS_CASE([$target_cpu],
    [i686], [],
    [AS_IF([test "$ac_cv_alignof_double" -gt 4],
      [AC_DEFINE([ARCH_ALIGN_DOUBLE], [1])])
     AS_IF([test "x$ac_cv_sizeof_long" = "x8" &&
            test "$ac_cv_alignof_long" -gt 4],
      [AC_DEFINE([ARCH_ALIGN_INT64], [1])],
      [AS_IF([test "x$ac_cv_sizeof_long_long" = "x8" &&
              test "$ac_cv_alignof_long_long" -gt 4],
      [AC_DEFINE([ARCH_ALIGN_INT64], [1])])])
    ])])

# Shared library support

sharedlib_cflags=''
mksharedlib='shared-libs-not-available'
rpath=''
mksharedlibrpath=''
natdynlinkopts=""

AS_IF([test x"$enable_shared" != "xno"],
  [AS_CASE([$host],
    [*-apple-darwin*],
      [mksharedlib="$CC -shared \
                   -flat_namespace -undefined suppress -Wl,-no_compact_unwind \
                   \$(LDFLAGS)"
      supports_shared_libraries=true],
    [*-*-mingw32],
      [mksharedlib='$(FLEXLINK)'
      mkmaindll='$(FLEXLINK) -maindll'
      AS_IF([test -n "$oc_dll_ldflags"],[
        mksharedlib="$mksharedlib -link \"$oc_dll_ldflags\""
        mkmaindll="$mkmaindll -link \"$oc_dll_ldflags\""])],
    [*-pc-windows],
      [mksharedlib='$(FLEXLINK)'
      mkmaindll='$(FLEXLINK) -maindll'],
    [*-*-cygwin*],
      [mksharedlib='$(FLEXLINK)'
      mkmaindll='$(FLEXLINK) -maindll'],
    [powerpc-ibm-aix*],
      [AS_CASE([$ocaml_cv_cc_vendor],
               [xlc*],
               [mksharedlib="$CC -qmkshrobj -G \$(LDFLAGS)"
                supports_shared_libraries=true])],
    [*-*-solaris*],
      [sharedlib_cflags="-fPIC"
      mksharedlib="$CC -shared"
      rpath="-Wl,-rpath,"
      mksharedlibrpath="-Wl,-rpath,"
      supports_shared_libraries=true],
    [[*-*-linux*|*-*-freebsd[3-9]*|*-*-freebsd[1-9][0-9]*\
    |*-*-openbsd*|*-*-netbsd*|*-*-dragonfly*|*-*-gnu*|*-*-haiku*]],
      [sharedlib_cflags="-fPIC"
       AS_CASE([$cc_basename,$host],
           [*gcc*,powerpc-*-linux*],
           [mksharedlib="$CC -shared -mbss-plt \$(LDFLAGS)"],
           [[*,i[3456]86-*]],
           # Disable DT_TEXTREL warnings on Linux and BSD i386
           # See https://github.com/ocaml/ocaml/issues/9800
           [mksharedlib="$CC -shared \$(LDFLAGS) -Wl,-z,notext"],
           [mksharedlib="$CC -shared \$(LDFLAGS)"])
      oc_ldflags="$oc_ldflags -Wl,-E"
      rpath="-Wl,-rpath,"
      mksharedlibrpath="-Wl,-rpath,"
      natdynlinkopts="-Wl,-E"
      supports_shared_libraries=true])])

AS_IF([test -z "$mkmaindll"], [mkmaindll=$mksharedlib])

# Configure native dynlink

natdynlink=false

AS_IF([test x"$supports_shared_libraries" = 'xtrue'],
  [AS_CASE(["$host"],
    [*-*-cygwin*], [natdynlink=true],
    [*-*-mingw32], [natdynlink=true],
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
    [riscv*-*-linux*], [natdynlink=true])])

AS_CASE([$enable_native_toplevel,$natdynlink],
  [yes,false],
    [AC_MSG_ERROR(m4_normalize([
      The native toplevel requires native dynlink support]))],
  [yes,*],
    [install_ocamlnat=true],
    [install_ocamlnat=false])

# Try to work around the Skylake/Kaby Lake processor bug.
AS_CASE(["$cc_basename,$host"],
  [*gcc*,x86_64-*|*gcc*,i686-*],
    [OCAML_CC_HAS_FNO_TREE_VRP
    AS_IF([$cc_has_fno_tree_vrp],
      [internal_cflags="$internal_cflags -fno-tree-vrp"])])

OCAML_CC_SUPPORTS_ALIGNED

## Check whether __attribute__((optimize("tree-vectorize")))) is supported
OCAML_CC_SUPPORTS_TREE_VECTORIZE

# Configure the native-code compiler

arch=none
model=default
system=unknown

AS_CASE([$host],
  [[i[3456]86-*-linux*]],
    [arch=i386; system=linux_elf],
  [[i[3456]86-*-*bsd*]],
    [arch=i386; system=bsd_elf],
  [[i[3456]86-*-haiku*]],
    [arch=i386; system=beos],
  [[i[3456]86-*-cygwin]],
    [arch=i386; system=cygwin],
  [[i[3456]86-*-gnu*]],
    [arch=i386; system=gnu],
  [[i[3456]86-*-mingw32]],
    [arch=i386; system=mingw],
  [i686-pc-windows],
    [arch=i386; system=win32],
  [x86_64-pc-windows],
    [arch=amd64; system=win64],
  [[powerpc64le*-*-linux*]],
    [arch=power; model=ppc64le; system=elf],
  [[powerpc*-*-linux*]],
    [arch=power; AS_IF([$arch64],[model=ppc64],[model=ppc]); system=elf],
  [[s390x*-*-linux*]],
    [arch=s390x; model=z10; system=elf],
  # expected to match "gnueabihf" as well as "musleabihf"
  [armv6*-*-linux-*eabihf],
    [arch=arm; model=armv6; system=linux_eabihf],
  [armv7*-*-linux-*eabihf],
    [arch=arm; model=armv7; system=linux_eabihf],
  [armv8*-*-linux-*eabihf],
    [arch=arm; model=armv8; system=linux_eabihf],
  [armv8*-*-linux-*eabi],
    [arch=arm; model=armv8; system=linux_eabi],
  [armv7*-*-linux-*eabi],
    [arch=arm; model=armv7; system=linux_eabi],
  [armv6t2*-*-linux-*eabi],
    [arch=arm; model=armv6t2; system=linux_eabi],
  [armv6*-*-linux-*eabi],
    [arch=arm; model=armv6; system=linux_eabi],
  [armv6*-*-freebsd*],
    [arch=arm; model=armv6; system=freebsd],
  [earmv6*-*-netbsd*],
    [arch=arm; model=armv6; system=netbsd],
  [earmv7*-*-netbsd*],
    [arch=arm; model=armv7; system=netbsd],
  [armv5te*-*-linux-*eabi],
    [arch=arm; model=armv5te; system=linux_eabi],
  [armv5*-*-linux-*eabi],
    [arch=arm; model=armv5; system=linux_eabi],
  [arm*-*-linux-*eabihf],
    [arch=arm; system=linux_eabihf],
  [arm*-*-linux-*eabi],
    [arch=arm; system=linux_eabi],
  [arm*-*-openbsd*],
    [arch=arm; system=bsd],
  [zaurus*-*-openbsd*],
    [arch=arm; system=bsd],
  [x86_64-*-linux*],
    [arch=amd64; system=linux],
  [x86_64-*-gnu*],
    [arch=amd64; system=gnu],
  [x86_64-*-dragonfly*],
    [arch=amd64; system=dragonfly],
  [x86_64-*-solaris*],
    [arch=amd64; system=solaris],
  [x86_64-*-freebsd*],
    [arch=amd64; system=freebsd],
  [x86_64-*-netbsd*],
    [arch=amd64; system=netbsd],
  [x86_64-*-openbsd*],
    [arch=amd64; system=openbsd],
  [x86_64-*-haiku*],
    [arch=amd64; system=beos],
  [arm64-*-darwin*],
    [arch=arm64; system=macosx],
  [aarch64-*-darwin*],
    [arch=arm64; system=macosx],
  [x86_64-*-darwin*],
    [arch=amd64; system=macosx],
  [x86_64-*-mingw32],
    [arch=amd64; system=mingw64],
  [aarch64-*-linux*],
    [arch=arm64; system=linux],
  [aarch64-*-freebsd*],
    [arch=arm64; system=freebsd],
  [aarch64-*-openbsd*],
    [arch=arm64; system=openbsd],
  [x86_64-*-cygwin*],
    [arch=amd64; system=cygwin],
  [riscv64-*-linux*],
    [arch=riscv; model=riscv64; system=linux]
)

AS_IF([test x"$enable_native_compiler" = "xno"],
  [native_compiler=false
  AC_MSG_NOTICE([the native compiler is disabled])],
  [native_compiler=true])

AS_IF([! $native_compiler], [natdynlink=false])

AS_IF([$natdynlink], [cmxs="cmxs"], [cmxs="cmx"])

AC_DEFINE_UNQUOTED([OCAML_OS_TYPE], ["$ostype"])

AC_CHECK_TOOL([DIRECT_LD],[ld])
AS_IF([test -z "$PARTIALLD"],
  [AS_CASE(["$arch,$cc_basename,$system,$model"],
    [amd64,*gcc*,macosx,*], [PACKLD_FLAGS=' -arch x86_64'],
    [power,*gcc*,elf,ppc], [PACKLD_FLAGS=' -m elf32ppclinux'],
    [power,*gcc*,elf,ppc64], [PACKLD_FLAGS=' -m elf64ppc'],
    [power,*gcc*,elf,ppc64le], [PACKLD_FLAGS=' -m elf64lppc'],
    [PACKLD_FLAGS=''])
  # The string for PACKLD must be capable of being concatenated with the
  # output filename. Don't assume that all C compilers understand GNU -ofoo
  # form, so ensure that the definition includes a space at the end (which is
  # achieved using the $(EMPTY) expansion trick).
   AS_IF([test x"$cc_basename" = "xcl"],
    # For the Microsoft C compiler there must be no space at the end of the
    # string.
    [PACKLD="link -lib -nologo $machine -out:"],
    [PACKLD="$DIRECT_LD -r$PACKLD_FLAGS -o \$(EMPTY)"])],
  [PACKLD="$PARTIALLD -o \$(EMPTY)"])

# Disable PIE at link time when ocamlopt does not produce position-independent
# code and the system produces PIE executables by default and demands PIC
# object files to do so.
# This issue does not affect amd64 (x86_64) and s390x (Z systems),
# since ocamlopt produces PIC object files by default.
# Currently the problem is known for Alpine Linux on platforms other
# than amd64 and s390x (issue #7562), and probably affects all Linux
# distributions that use the musl standard library and dynamic loader.
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
       [common_cflags="-no-pie $common_cflags"],
    [])])

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

AS_CASE([$as_target,$ocaml_cv_cc_vendor],
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
    default_aspp="$default_as"],
  [])

AS_IF([test "$with_pic"],
  [fpic=true
  AC_DEFINE([CAML_WITH_FPIC])
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
  [AC_DEFINE([HAS_C99_FLOAT_OPS])
  # Check whether round works (known bug in mingw-w64)
  OCAML_C99_CHECK_ROUND
  # Check whether fma works (regressed in mingw-w64 8.0.0; present, but broken,
  # in VS2013-2017 and present but unimplemented in Cygwin64)
  OCAML_C99_CHECK_FMA],
  [AS_IF([test x"$enable_imprecise_c99_float_ops" != "xyes" ],
    [AS_CASE([$enable_imprecise_c99_float_ops,$ocaml_cv_cc_vendor],
      [no,*], [hard_error=true],
      [,msvc-*], [AS_IF([test "${ocaml_cv_cc_vendor#msvc-}" -lt 1800 ],
        [hard_error=false],
        [hard_error=true])],
      [hard_error=true])
     AS_IF([test x"$hard_error" = 'xtrue'],
       [AC_MSG_ERROR(m4_normalize([
         C99 float ops unavailable, enable replacements
         with --enable-imprecise-c99-float-ops]))],
       [AC_MSG_WARN(m4_normalize([
         C99 float ops unavailable, replacements enabled
         (ancient Visual Studio)]))])])])

## getrusage
AC_CHECK_FUNC([getrusage], [AC_DEFINE([HAS_GETRUSAGE])])

## times
AC_CHECK_FUNC([times], [AC_DEFINE([HAS_TIMES])])

## secure_getenv and __secure_getenv

saved_CPPFLAGS="$CPPFLAGS"
CPPFLAGS="-D_GNU_SOURCE $CPPFLAGS"

AC_CHECK_FUNC([secure_getenv],
  [AC_DEFINE([HAS_SECURE_GETENV])],
  [AC_CHECK_FUNC([__secure_getenv], [AC_DEFINE([HAS___SECURE_GETENV])])])

CPPFLAGS="$saved_CPPFLAGS"

## issetugid

AC_CHECK_FUNC([issetugid], [AC_DEFINE([HAS_ISSETUGID])])

## Checking for monotonic clock source
## On Windows MSVC, QueryPerformanceCounter and QueryPerformanceFrequency
## are always available.
## On Unix platforms, we check for the appropriate POSIX feature-test macros.
## On MacOS clock_gettime's CLOCK_MONOTONIC flag is not actually monotonic.
## mach_timebase_info and mach_absolute_time are used instead.

AS_CASE([$host],
  [*-*-windows],
    [has_monotonic_clock=true],
  [*-apple-darwin*], [
    AC_CHECK_FUNCS([mach_timebase_info mach_absolute_time],
      [
        has_monotonic_clock=true
        AC_DEFINE([HAS_MACH_ABSOLUTE_TIME])
      ],
      [has_monotonic_clock=false])],
  [AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
    #include <unistd.h>
    #include <time.h>
    int main(void)
    {
      #if !(defined(_POSIX_TIMERS) && defined(_POSIX_MONOTONIC_CLOCK)   \
         && _POSIX_MONOTONIC_CLOCK != (-1))
        #error "no monotonic clock source"
      #endif
        return 0;
     }
    ]])],
    [
      has_monotonic_clock=true
      AC_DEFINE([HAS_POSIX_MONOTONIC_CLOCK])
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
        [yes,false], [
          AC_MSG_ERROR([Instrumented runtime support requested \
but no proper monotonic clock source was found.])
        ],
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
          [
           AC_MSG_ERROR([Instrumented runtime support requested \
but clock_gettime is missing.])
          ],
        [yes,*,false],
          [
           AC_MSG_ERROR([Instrumented runtime support requested \
but no proper monotonic clock source was found.])
          ]
      )]
    )]
)

## Sockets

## TODO: check whether the different libraries are really useful

sockets=true

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [cclibs="$cclibs -lws2_32"
    AC_SEARCH_LIBS([socket], [ws2_32])
    AC_CHECK_FUNC([socketpair], [AC_DEFINE([HAS_SOCKETPAIR])])],
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

AS_IF([$sockets], [AC_DEFINE([HAS_SOCKETS])])

## socklen_t

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [AC_CHECK_TYPE([socklen_t], [AC_DEFINE([HAS_SOCKLEN_T])], [],
      [#include <ws2tcpip.h>])],
  [AC_CHECK_TYPE([socklen_t], [AC_DEFINE([HAS_SOCKLEN_T])], [],
    [#include <sys/socket.h>])])

AC_CHECK_FUNC([inet_aton], [AC_DEFINE([HAS_INET_ATON])])

## Unix domain sockets support on Windows

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [AC_CHECK_HEADERS([afunix.h], [AC_DEFINE([HAS_AFUNIX_H])], [],
      [#include <winsock2.h>])])

## IPv6 support

ipv6=true

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
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
  [AC_CHECK_FUNC([getaddrinfo], [], [ipv6=false])])

AS_IF([$ipv6],
  [AC_CHECK_FUNC([getnameinfo], [], [ipv6=false])])

AS_IF([$ipv6],
  [AC_CHECK_FUNC([inet_pton], [], [ipv6=false])])

AS_IF([$ipv6],
  [AC_CHECK_FUNC([inet_ntop], [AC_DEFINE([HAS_IPV6])])])

AC_CHECK_FUNC([rewinddir], [AC_DEFINE([HAS_REWINDDIR])])

AC_CHECK_FUNC([lockf], [AC_DEFINE([HAS_LOCKF])])

AC_CHECK_FUNC([mkfifo], [AC_DEFINE([HAS_MKFIFO])])

AC_CHECK_FUNC([getcwd], [AC_DEFINE([HAS_GETCWD])])

AC_CHECK_DECL([system], [AC_DEFINE([HAS_SYSTEM])], [], [[#include <stdlib.h>]])

## utime
## Note: this was defined in config/s-nt.h but the autoconf macros do not
# seem to detect it properly on Windows so we hardcode the definition
# of HAS_UTIME on Windows but this will probably need to be clarified
AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [AC_DEFINE([HAS_UTIME])],
  [AC_CHECK_HEADER([sys/types.h],
    [AC_CHECK_HEADER([utime.h],
      [AC_CHECK_FUNC([utime], [AC_DEFINE([HAS_UTIME])])])])])

AC_CHECK_FUNC([utimes], [AC_DEFINE([HAS_UTIMES])])

AC_CHECK_FUNC([fchmod],
  [AC_CHECK_FUNC([fchown], [AC_DEFINE([HAS_FCHMOD])])])

AC_CHECK_FUNC([truncate],
  [AC_CHECK_FUNC([ftruncate], [AC_DEFINE([HAS_TRUNCATE])])])

## select
AC_CHECK_FUNC([select],
  [AC_CHECK_TYPE([fd_set],
    [AC_DEFINE([HAS_SELECT])
    select=true], [select=false], [
#include <sys/types.h>
#include <sys/select.h>
  ])])

AC_CHECK_FUNC([nanosleep], [AC_DEFINE([HAS_NANOSLEEP])])

AC_CHECK_FUNC([symlink],
  [AC_CHECK_FUNC([readlink],
    [AC_CHECK_FUNC([lstat], [AC_DEFINE([HAS_SYMLINK])])])])

AC_CHECK_FUNC([realpath], [AC_DEFINE([HAS_REALPATH])])

# wait
AC_CHECK_FUNC(
  [waitpid],
  [
    wait=true
    AC_DEFINE([HAS_WAITPID])
  ],
  [wait=false])

AC_CHECK_FUNC(
  [wait4],
  [
    has_wait=true
    AC_DEFINE([HAS_WAIT4])
  ])

## getgroups
AC_CHECK_FUNC([getgroups], [AC_DEFINE([HAS_GETGROUPS])])

## setgroups
AC_CHECK_FUNC([setgroups], [AC_DEFINE([HAS_SETGROUPS])])

## initgroups
AC_CHECK_FUNC([initgroups], [AC_DEFINE([HAS_INITGROUPS])])

## termios

AC_CHECK_HEADER([termios.h],
  [AC_CHECK_FUNC([tcgetattr],
    [AC_CHECK_FUNC([tcsetattr],
      [AC_CHECK_FUNC([tcsendbreak],
        [AC_CHECK_FUNC([tcflush],
          [AC_CHECK_FUNC([tcflow], [AC_DEFINE([HAS_TERMIOS])])])])])])])

## setitimer

AC_CHECK_FUNC([setitimer],
  [
    setitimer=true
    AC_DEFINE([HAS_SETITIMER])
  ],
  [setitimer=false])

## gethostname
# Note: detection fails on Windows so hardcoding the result
# (should be debugged later)
AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [AC_DEFINE([HAS_GETHOSTNAME])],
  [AC_CHECK_FUNC([gethostname], [AC_DEFINE([HAS_GETHOSTNAME])])])

## uname

AC_CHECK_HEADER([sys/utsname.h],
  [AC_CHECK_FUNC([uname], [AC_DEFINE([HAS_UNAME])])])

## gettimeofday

AC_CHECK_FUNC([gettimeofday],
  [
    gettimeofday=true
    AC_DEFINE([HAS_GETTIMEOFDAY])
  ],
  [gettimeofday=false])

## mktime

AC_CHECK_FUNC([mktime], [AC_DEFINE([HAS_MKTIME])])

## setsid

AS_CASE([$host],
  [*-cygwin|*-*-mingw32|*-pc-windows], [],
  [AC_CHECK_FUNC([setsid], [AC_DEFINE([HAS_SETSID])])])

## putenv

AC_CHECK_FUNC([putenv], [AC_DEFINE([HAS_PUTENV])])

## setenv and unsetenv

AC_CHECK_FUNC([setenv],
  [AC_CHECK_FUNC([unsetenv], [AC_DEFINE([HAS_SETENV_UNSETENV])])])

## newlocale() and <locale.h>
# Note: the detection fails on msvc so we hardcode the result
# (should be debugged later)
AS_CASE([$host],
  [*-pc-windows], [AC_DEFINE([HAS_LOCALE_H])],
  [AC_CHECK_HEADER([locale.h],
    [AC_CHECK_FUNC([newlocale],
      [AC_CHECK_FUNC([freelocale],
        [AC_CHECK_FUNC([uselocale], [AC_DEFINE([HAS_LOCALE_H])])])])])])

AC_CHECK_HEADER([xlocale.h],
  [AC_CHECK_FUNC([newlocale],
    [AC_CHECK_FUNC([freelocale],
      [AC_CHECK_FUNC([uselocale], [AC_DEFINE([HAS_XLOCALE_H])])])])])

## strtod_l
# Note: not detected on MSVC so hardcoding the result
# (should be debugged later)
AS_CASE([$host],
  [*-pc-windows], [AC_DEFINE([HAS_STRTOD_L])],
  [AC_CHECK_FUNC([strtod_l], [AC_DEFINE([HAS_STRTOD_L])])])

## shared library support
AS_IF([$supports_shared_libraries],
  [AS_CASE([$host],
    [*-*-mingw32|*-pc-windows|*-*-cygwin*],
      [DLLIBS=""],
    [AC_CHECK_FUNC([dlopen],
      [supports_shared_libraries=true DLLIBS=""],
      [AC_CHECK_LIB([dl], [dlopen],
        [supports_shared_libraries=true DLLIBS="-ldl $DLLIBS"],
        [supports_shared_libraries=false])])])],
  [supports_shared_libraries=false])

AS_IF([$supports_shared_libraries],
  [AC_MSG_NOTICE([Dynamic loading of shared libraries is supported.])
  AC_DEFINE([SUPPORT_DYNAMIC_LINKING])],
  [AC_MSG_NOTICE([Dynamic loading of shared libraries is not supported.])])

## mmap

AC_CHECK_HEADER([sys/mman.h],
  [AC_CHECK_FUNC([mmap],
    [AC_CHECK_FUNC([munmap], [AC_DEFINE([HAS_MMAP])])])])

## pwrite

AC_CHECK_FUNC([pwrite], [AC_DEFINE([HAS_PWRITE])])

## -fdebug-prefix-map support by the C compiler
AS_CASE([$ocaml_cv_cc_vendor,$host],
  [*,*-*-mingw32], [cc_has_debug_prefix_map=false],
  [*,*-pc-windows], [cc_has_debug_prefix_map=false],
  [xlc*,powerpc-ibm-aix*], [cc_has_debug_prefix_map=false],
  [sunc*,sparc-sun-*], [cc_has_debug_prefix_map=false],
  [OCAML_CC_HAS_DEBUG_PREFIX_MAP])

## Does stat support nanosecond precision

AC_CHECK_MEMBER([struct stat.st_atim.tv_nsec],
  [stat_has_ns_precision=true
  AC_DEFINE([HAS_NANOSECOND_STAT], [1])],
  [],
  [
    AC_INCLUDES_DEFAULT
    #include <sys/stat.h>
  ])


AS_IF([! $stat_has_ns_precision],
  [AC_CHECK_MEMBER([struct stat.st_atimespec.tv_nsec],
    [stat_has_ns_precision=true
    AC_DEFINE([HAS_NANOSECOND_STAT], [2])],
    [],
    [
      AC_INCLUDES_DEFAULT
      #include <sys/stat.h>
    ])])

AS_IF([! $stat_has_ns_precision],
  [AC_CHECK_MEMBER([struct stat.st_atimensec],
    [stat_has_ns_precision=true
    AC_DEFINE([HAS_NANOSECOND_STAT], [3])],
    [],
    [
      AC_INCLUDES_DEFAULT
      #include <sys/stat.h>
    ])])

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

AC_CHECK_FUNC([mkstemp], [AC_DEFINE([HAS_MKSTEMP])])

## nice

AC_CHECK_FUNC([nice], [AC_DEFINE([HAS_NICE])])

## dup3

AC_CHECK_FUNC([dup3], [AC_DEFINE([HAS_DUP3])])

## pipe2

AC_CHECK_FUNC([pipe2], [AC_DEFINE([HAS_PIPE2])])

## accept4

AC_CHECK_FUNC([accept4], [AC_DEFINE([HAS_ACCEPT4])])

## getauxval

AC_CHECK_FUNC([getauxval], [AC_DEFINE([HAS_GETAUXVAL])])

## shmat
AC_CHECK_HEADER([sys/shm.h],
  [
    AC_DEFINE([HAS_SYS_SHM_H])
    AC_CHECK_FUNC([shmat], [AC_DEFINE([HAS_SHMAT])])
  ])

## execvpe

AC_CHECK_FUNC([execvpe], [AC_DEFINE([HAS_EXECVPE])])

## posix_spawn

AC_CHECK_HEADER([spawn.h],
  [AC_CHECK_FUNC([posix_spawn],
    [AC_CHECK_FUNC([posix_spawnp], [AC_DEFINE([HAS_POSIX_SPAWN])])])])

## ffs or _BitScanForward

AC_CHECK_FUNC([ffs], [AC_DEFINE([HAS_FFS])])
AC_CHECK_FUNC([_BitScanForward], [AC_DEFINE([HAS_BITSCANFORWARD])])

## Determine whether the debugger should/can be built

AS_CASE([$enable_debugger],
  [no],
    [with_debugger=""
    AC_MSG_NOTICE([replay debugger disabled])],
  [AS_IF([$sockets],
    [with_debugger="ocamldebugger"
    AC_MSG_NOTICE([replay debugger supported])],
    [with_debugger=""
    AC_MSG_NOTICE([replay debugger not supported])])
  ])

## Should the runtime with debugging support be built
AS_CASE([$enable_debug_runtime],
  [no], [debug_runtime=false],
  [debug_runtime=true])

## Determine if system stack overflows can be detected

AC_MSG_CHECKING([whether stack overflows can be detected])

AS_CASE([$arch,$system],
  [i386,linux_elf|amd64,linux|amd64,macosx \
    |amd64,openbsd|i386,bsd_elf|arm64,linux|arm64,macosx],
    [AC_DEFINE([HAS_STACK_OVERFLOW_DETECTION])
    AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])])

## Determine if the POSIX threads library is supported

AS_CASE([$enable_systhreads,$enable_unix_lib],
  [yes,no],
    [systhread_support=false
    AC_MSG_ERROR([the Win32/POSIX threads library requires the unix library])],
  [no,*|*,no],
    [systhread_support=false
    AC_MSG_NOTICE([the Win32/POSIX threads library is disabled])],
  [AS_CASE([$host],
    [*-*-mingw32|*-pc-windows],
      [systhread_support=true
      otherlibraries="$otherlibraries systhreads"
      AC_MSG_NOTICE([the Win32 threads library is supported])],
    [AX_PTHREAD(
      [systhread_support=true
      otherlibraries="$otherlibraries systhreads"
      common_cflags="$common_cflags $PTHREAD_CFLAGS"
      AC_MSG_NOTICE([the POSIX threads library is supported])
      saved_CFLAGS="$CFLAGS"
      saved_LIBS="$LIBS"
      CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
      LIBS="$LIBS $PTHREAD_LIBS"
      AC_CHECK_FUNC([sigwait], [AC_DEFINE([HAS_SIGWAIT])])
      LIBS="$saved_LIBS"
      CFLAGS="$saved_CFLAGS"],
      [AS_IF([test x"$enable_systhreads" = "xyes"],
        [AC_MSG_ERROR([the POSIX thread library is not available])],
        [systhread_support=false
        AC_MSG_NOTICE([the POSIX threads library is not supported])])])])])

## Does the assembler support debug prefix map and CFI directives
as_has_debug_prefix_map=false
asm_cfi_supported=false
AS_IF([$native_compiler],
  [AS_CASE([$host],
    [*-*-mingw32|*-pc-windows], [],
    [OCAML_AS_HAS_DEBUG_PREFIX_MAP
    OCAML_AS_HAS_CFI_DIRECTIVES])])

## Frame pointers

AS_IF([test x"$enable_frame_pointers" = "xyes"],
  [AS_CASE(["$host,$cc_basename"],
    [x86_64-*-linux*,gcc*|x86_64-*-linux*,clang*],
      [common_cflags="$common_cflags -g  -fno-omit-frame-pointer"
      frame_pointers=true
      AC_DEFINE([WITH_FRAME_POINTERS])
      AC_MSG_NOTICE([using frame pointers])],
    [AC_MSG_ERROR([frame pointers not supported on this platform])]
  )],
  [AC_MSG_NOTICE([not using frame pointers])
  frame_pointers=false])

## No naked pointers

AS_IF([test x"$enable_naked_pointers" = "xno" ],
  [naked_pointers=false
   AC_DEFINE([NO_NAKED_POINTERS])],
  [naked_pointers=true])

AS_IF([test x"$enable_naked_pointers_checker" = "xyes" ],
  [AS_IF([test x"$enable_naked_pointers" = "xno" ],
         [AC_MSG_ERROR(m4_normalize([
               --enable-naked-pointers-checker and --disable-naked-pointers
               are incompatible]))])
   AS_CASE(["$arch","$system"],
    [amd64,linux|amd64,macosx \
    |amd64,openbsd|amd64,win64 \
    |amd64,freebsd|amd64,solaris \
    |arm64,linux|arm64,macosx],
      [naked_pointers_checker=true
      AC_DEFINE([NAKED_POINTERS_CHECKER])],
    [*],
      [AC_MSG_ERROR([naked pointers checker not supported on this platform])]
  )],
  [naked_pointers_checker=false])

## Check for mmap support for huge pages and contiguous heap
OCAML_MMAP_SUPPORTS_HUGE_PAGES

AC_DEFINE_UNQUOTED([PROFINFO_WIDTH], [$profinfo_width])
AS_IF([$profinfo], [AC_DEFINE([WITH_PROFINFO])])

AS_IF([test x"$enable_installing_bytecode_programs" = "xno"],
  [install_bytecode_programs=false],
  [install_bytecode_programs=true])

AS_IF([test x"$enable_installing_source_artifacts" = "xno"],
  [install_source_artifacts=false],
  [install_source_artifacts=true])

AS_IF([test x"$enable_ocamldoc" = "xno"],
  [ocamldoc=""],
  [ocamldoc=ocamldoc])

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



AS_CASE([$enable_ocamltest,OCAML__DEVELOPMENT_VERSION],
  [yes,*|,true],[ocamltest='ocamltest'],
  [ocamltest=''])

AS_IF([test x"$enable_flambda" = "xyes"],
  [flambda=true
  AS_IF([test x"$enable_flambda_invariants" = "xyes"],
    [flambda_invariants=true],
    [flambda_invariants=false])],
  [flambda=false
  flambda_invariants=false])

AS_IF([test x"$enable_cmm_invariants" = "xyes"],
  [cmm_invariants=true],
  [cmm_invariants=false])

AS_IF([test x"$enable_flat_float_array" = "xno"],
  [flat_float_array=false],
  [AC_DEFINE([FLAT_FLOAT_ARRAY])
  flat_float_array=true])

AS_IF([test x"$enable_function_sections" = "xno"],
  [function_sections=false],
  [AS_CASE([$arch],
    [amd64|i386|arm64], # not supported on arm32, see issue #9124.
     [AS_CASE([$target],
        [*-cygwin*|*-mingw*|*-windows|*-apple-darwin*],
          [function_sections=false;
           AC_MSG_NOTICE([No support for function sections on $target.])],
        [*],
          [AS_CASE([$ocaml_cv_cc_vendor],
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
              internal_cflags="$internal_cflags -ffunction-sections";
              AC_DEFINE([FUNCTION_SECTIONS])],
            [*],
              [function_sections=false;
              AC_MSG_NOTICE([Function sections are not supported by
              $ocaml_cv_cc_vendor.])])])],
    [function_sections=false]);
  AS_IF([test x"$function_sections" = "xfalse"],
    [AS_IF([test x"$enable_function_sections" = "xyes"],
      [AC_MSG_ERROR([Function sections are not supported.])],
      [AC_MSG_NOTICE([Disabling function sections.])])],
    [])])

AS_IF([test x"$with_afl" = "xyes"],
  [afl=true],
  [afl=false])

AS_IF([test x"$enable_force_safe_string" = "xno"],
  [force_safe_string=false],
  [AC_DEFINE([CAML_SAFE_STRING])
   force_safe_string=true])

AS_IF([test x"$DEFAULT_STRING" = "xunsafe"],
  [default_safe_string=false],
  [default_safe_string=true])

oc_cflags="$common_cflags $internal_cflags"
oc_cppflags="$common_cppflags $internal_cppflags"
ocamlc_cflags="$common_cflags $sharedlib_cflags \$(CFLAGS)"
ocamlc_cppflags="$common_cppflags \$(CPPFLAGS)"
cclibs="$cclibs $mathlib"

AS_CASE([$host],
  [*-*-mingw32],
    [bytecclibs="-lws2_32 -lversion"
    nativecclibs="-lws2_32 -lversion"],
  [*-pc-windows],
    [bytecclibs="advapi32.lib ws2_32.lib version.lib"
    nativecclibs="advapi32.lib ws2_32.lib version.lib"],
  [bytecclibs="$cclibs $DLLIBS $PTHREAD_LIBS"
  nativecclibs="$cclibs $DLLIBS"])

AS_IF([test x"$libdir" = x'${exec_prefix}/lib'],
  [libdir="$libdir"/ocaml])

AS_IF([test x"$mandir" = x'${datarootdir}/man'],
  [mandir='${prefix}/man'])

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [AS_CASE([$WINDOWS_UNICODE_MODE],
      [ansi],
        [windows_unicode=0],
      [compatible|""],
        [windows_unicode=1],
      [AC_MSG_ERROR([unexpected windows unicode mode])])],
  [windows_unicode=0])

# Define default prefix correctly for the different Windows ports
AS_IF([test x"$prefix" = "xNONE"],
  [AS_CASE([$host],
    [i686-w64-mingw32], [prefix='C:/ocamlmgw'],
    [x86_64-w64-mingw32], [prefix='C:/ocamlmgw64'],
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
  [*-*-mingw32],
    [AC_DEFINE([HAS_BROKEN_PRINTF])
    AC_DEFINE([HAS_STRERROR])
    AC_DEFINE([HAS_IPV6])
    AC_DEFINE([HAS_NICE])],
  [*-pc-windows],
    [AC_DEFINE([HAS_BROKEN_PRINTF])
    AC_DEFINE([HAS_STRERROR])
    AC_DEFINE([HAS_IPV6])
    AC_DEFINE([HAS_NICE])],
  [*-*-solaris*],
    # This is required as otherwise floats are printed
    # as "Infinity" and "Inf" instead of the expected "inf"
    [AC_DEFINE([HAS_BROKEN_PRINTF])])

AS_IF([test x"$enable_stdlib_manpages" != "xno"],
  [stdlib_manpages=true],[stdlib_manpages=false])

# Do not permanently cache the result of flexdll.h
unset ac_cv_header_flexdll_h

AC_OUTPUT
