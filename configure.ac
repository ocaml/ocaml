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

AC_INIT([OCaml],
        m4_esyscmd([head -n1 VERSION | tr -d '\r\n']),
        [caml-list@inria.fr],
        [ocaml],
        [http://www.ocaml.org])

AC_MSG_NOTICE([Configuring OCaml version AC_PACKAGE_VERSION])

# Configuration variables

## Command-line arguments passed to configure
CONFIGURE_ARGS="$*"

# Command-line tools section of the Unix manual
programs_man_section=1

# Library section of the Unix manual
libraries_man_section=3

# Command to build executalbes
mkexe="\$(CC) \$(OC_CFLAGS) \$(OC_CPPFLAGS) \$(OC_LDFLAGS)"

# Flags for building executable files with debugging symbols
mkexedebugflag="-g"
common_cflags=""
common_cppflags=""
internal_cflags=""
internal_cppflags=""
ocamlc_cflags=""
ocamlc_cppflags=""
ocamlopt_cflags=""
ocamlopt_cppflags=""
oc_ldflags=""
with_sharedlibs=true
ostype="Unix"
iflexdir=""
SO="so"
toolchain="cc"
profinfo=false
profinfo_width=0
extralibs=

# Information about the package

## Source directory
AC_CONFIG_SRCDIR([runtime/interp.c])

## Directory containing auxiliary scripts used dugring build
AC_CONFIG_AUX_DIR([build-aux])

## Output variables

AC_SUBST([CONFIGURE_ARGS])
AC_SUBST([VERSION], [AC_PACKAGE_VERSION])
AC_SUBST([CC])
# Note: This is present for the flexdll bootstrap where it exposed as the old
# TOOLPREF variable. It would be better if flexdll where updated to require
# WINDRES instead.
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
AC_SUBST([graphlib])
AC_SUBST([outputexe])
AC_SUBST([outputobj])
AC_SUBST([syslib])
AC_SUBST([extralibs])
AC_SUBST([programs_man_section])
AC_SUBST([libraries_man_section])
AC_SUBST([fpic])
AC_SUBST([mkexe])
AC_SUBST([mkexedebugflag])
AC_SUBST([ccomptype])
AC_SUBST([toolchain])
AC_SUBST([oc_cflags])
AC_SUBST([oc_cppflags])
AC_SUBST([oc_ldflags])
AC_SUBST([bytecclibs])
AC_SUBST([dllibs])
AC_SUBST([nativecclibs])
AC_SUBST([ocamlc_cflags])
AC_SUBST([ocamlc_cppflags])
AC_SUBST([ocamlopt_cflags])
AC_SUBST([ocamlopt_cppflags])
AC_SUBST([iflexdir])
AC_SUBST([hashbangscripts])
AC_SUBST([AR])
AC_SUBST([RANLIB])
AC_SUBST([RANLIBCMD])
AC_SUBST([mklib])
AC_SUBST([supports_shared_libraries])
AC_SUBST([natdynlink])
AC_SUBST([natdynlinkopts])
AC_SUBST([cmxs])
AC_SUBST([debug_runtime])
AC_SUBST([instrumented_runtime])
AC_SUBST([otherlibraries])
AC_SUBST([cc_has_debug_prefix_map])
AC_SUBST([as_has_debug_prefix_map])
AC_SUBST([with_debugger]) # TODO: rename this variable
AC_SUBST([with_camltex])
AC_SUBST([ocamldoc])
AC_SUBST([pthread_link])
AC_SUBST([x_includes])
AC_SUBST([x_libraries])
AC_SUBST([libbfd_include])
AC_SUBST([libbfd_link])
AC_SUBST([asppprofflags])
AC_SUBST([ASPP])
AC_SUBST([AS])
AC_SUBST([asm_cfi_supported])
AC_SUBST([sharedlib_cflags])
AC_SUBST([rpath])
AC_SUBST([mksharedlib])
AC_SUBST([mkmaindll])
AC_SUBST([mksharedlibrpath])
AC_SUBST([install_bytecode_programs])
AC_SUBST([install_source_artifacts])
AC_SUBST([profiling])
AC_SUBST([profinfo])
AC_SUBST([profinfo_width])
AC_SUBST([cc_profile])
AC_SUBST([frame_pointers])
AC_SUBST([spacetime])
AC_SUBST([call_counts])
AC_SUBST([libunwind_available])
AC_SUBST([libunwind_include_flags])
AC_SUBST([libunwind_link_flags])
AC_SUBST([flambda])
AC_SUBST([flambda_invariants])
AC_SUBST([max_testsuite_dir_retries])
AC_SUBST([windows_unicode])
AC_SUBST([flat_float_array])
AC_SUBST([afl])
AC_SUBST([force_safe_string])
AC_SUBST([default_safe_string])
AC_SUBST([flexdll_chain])
AC_SUBST([flexlink_flags])
AC_SUBST([PACKLD])

## Generated files

AC_CONFIG_FILES([Makefile.common])
AC_CONFIG_FILES([Makefile.config])
AC_CONFIG_HEADERS([runtime/caml/m.h])
AC_CONFIG_HEADERS([runtime/caml/s.h])

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
    mkexedebugflag=''
    syslib='$(1).lib'],
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

AC_ARG_WITH([dllibs],
  [AS_HELP_STRING([--with-dllibs=LIBS],
    [use LIBS (in addition to dl) to load dynamic libs @<:@default=""@:>@])],
  [otherdllibs=$withval],
  [otherdllibs=""])

AC_ARG_ENABLE([instrumented-runtime],
  [AS_HELP_STRING([--enable-instrumented-runtime],
    [build the instrumented runtime @<:@default=auto@:>@])],
  [],
  [enable_instrumented_runtime=auto])

AC_ARG_ENABLE([vmthreads],
  [AS_HELP_STRING([--disable-vmthreads],
    [disable the bytecode threads library])])

AC_ARG_ENABLE([systhreads],
  [AS_HELP_STRING([--disable-systhreads],
    [disable the Win32/POSIX threads library])])

AC_ARG_WITH([libunwind],
  [AS_HELP_STRING([--with-libunwind],
    [location of the libunwind package])])

AC_ARG_WITH([libunwind-include-dir],
  [AS_HELP_STRING([--with-libunwind-include-dir],
    [location of header files for libunwind])])

AC_ARG_ENABLE([graph-lib],
  [AS_HELP_STRING([--disable-graph-lib],
    [do not build the graphics library])])

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

AC_ARG_ENABLE([frame-pointers],
  [AS_HELP_STRING([--enable-frame-pointers],
    [use frame pointers in runtime and generated code])])

AC_ARG_ENABLE([naked-pointers],
  [AS_HELP_STRING([--disable-naked-pointers],
    [do not allow naked pointers])])

AC_ARG_ENABLE([spacetime],
  [AS_HELP_STRING([--enable-spacetime],
    [build the spacetime profiler])])

AC_ARG_ENABLE([call-counts],
  [AS_HELP_STRING([--disable-call-counts],
    [disable the call counts in spacetime])])

AC_ARG_ENABLE([cfi],
  [AS_HELP_STRING([--disable-cfi],
    [disable the CFI directives in assembly files])])

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

AC_ARG_WITH([target-bindir],
  [AS_HELP_STRING([--with-target-bindir],
    [location of binary programs on target system])])

AC_ARG_WITH([reserved-header-bits],
  [AS_HELP_STRING([--with-reserved-header-bits=BITS],
    [reserve BITS bits in block headers for profiling info])])

AC_ARG_VAR([WINDOWS_UNICODE_MODE],
  [how to handle Unicode under Windows: ansi, compatible])

# There are two configure-time string safety options,
# --(enable|disable)-force-safe-string and
# --with-default-string=safe|unsafe that
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
# configure-time options --with-default-string=safe|unsafe decides which
# setting will be chosen by default, if no compile-time option is
# explicitly passed.
#
# The configure-time behavior of OCaml 4.05 and older was equivalent
# to --disable-force-safe-string --with-default-string=unsafe. OCaml 4.06
# and later uses --disable-force-safe-string --with-default-string=safe. We
# expect --enable-force-safe-string to become the default in the future.

AC_ARG_ENABLE([force-safe-string],
  [AS_HELP_STRING([--enable-force-safe-string],
    [force strings to be safe])])

AC_ARG_WITH([default-string],
  [AS_HELP_STRING([--with-default-string=mode],
    [whether strings should be safe or unsafe by default
      @<:@default=safe@:>@])])

AC_ARG_ENABLE([flat-float-array],
  [AS_HELP_STRING([--disable-flat-float-array],
    [do not use flat float arrays])])

AC_ARG_WITH([afl],
  [AS_HELP_STRING([--with-afl],
    [use the AFL fuzzer])])

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
old_host_os=$host_os
AS_IF([test x"$host_os" = "xwindows"],[host_os=mingw])
LT_INIT
host_os=$old_host_os

# Extracting information from libtool's configuration
AS_IF([test -n "$RANLIB" ],
  [RANLIBCMD="$RANLIB"],
  [RANLIB="$AR rs"; RANLIBCMD=""]
)

AS_CASE([$host],
  # In config/Makefile.mingw*, we had:
  # TARGET=i686-w64-mingw32 and x86_64-w64-mingw32
  # TOOLPREF=$(TARGET)-
  # ARCMD=$(TOOLPREF)ar
  # RANLIB=$(TOOLPREF)ranlib
  # RANLIBCMD=$(TOOLPREF)ranlib
  # However autoconf and libtool seem to use ar and ranlib
  # So we let them do, at the moment
  [*-pc-windows],
    [
      libext=lib
      AR=""; RANLIB=echo; RANLIBCMD=""
      AS_IF([test "$host_cpu" = "x86_64" ],
        [machine="-machine:AMD64 "],
        [machine=""])
      mklib="link -lib -nologo $machine /out:\$(1) \$(2)"
    ],
  [
    mklib="rm -f \$(1) && ${AR} rc \$(1) \$(2) && ${RANLIB} \$(1)"
  ])

# C preprocessor: we do not rely on AC_PROG_CPP because it returns
# 'fcc -E' while we really need cpp
AC_CHECK_TOOL([DIRECT_CPP],[cpp])
AS_CASE([$host],
  [*-pc-windows], [CPP="$CC -nologo -EP"],
  [CPP=$DIRECT_CPP])

# Libraries to build depending on the host

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [unix_or_win32="win32"
    unixlib="win32unix"
    graphlib="win32graph"
    cc_profile=''],
  [unix_or_win32="unix"
  unixlib="unix"
  graphlib="graph"
  cc_profile='-pg'])
AS_CASE([$host],
  [*-*-cygwin*|*-*-mingw32|*-pc-windows],
    [exeext=".exe"],
  [exeext=''])

otherlibraries="dynlink"
AS_IF([test x"$enable_unix_lib" != "xno"],
  [AS_IF([test x"$enable_bigarray_lib" != "xno"],
    [otherlibraries="$otherlibraries $unixlib bigarray"],
    [otherlibraries="$otherlibraries $unixlib"])])
AS_IF([test x"$enable_str_lib" != "xno"],
  [otherlibraries="$otherlibraries str"])

# Checks for system services

## Test whether #! scripts are supported
## TODO: have two values, one for host and one for target
AC_SYS_INTERPRETER

AS_IF(
  [test "x$interpval" = "xyes"],
    [AS_CASE([$host],
      [*-cygwin|*-*-mingw32|*-pc-windows],
        [hashbangscripts=false],
      [hashbangscripts=true]
    )],
  [hashbangscripts=false]
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

## Find vendor of the C compiler
OCAML_CC_VENDOR

## Determine which flags to use for the C compiler

AS_CASE([$ocaml_cv_cc_vendor],
  [msvc-*],
    [outputobj=-Fo; CPP="cl -nologo -EP"; gcc_warnings=""],
  [outputobj='-o $(EMPTY)'; AS_CASE([AC_PACKAGE_VERSION],
    [*+dev*],
      [gcc_warnings="-Wall -Werror"],
    [gcc_warnings="-Wall"]
  )]
)

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
    [internal_cflags="-Wno-unused $gcc_warnings"
    # TODO: see whether the code can be fixed to avoid -Wno-unused
    common_cflags="-O -mms-bitfields"
    internal_cppflags='-DUNICODE -D_UNICODE'
    internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE="
    internal_cppflags="${internal_cppflags}\$(WINDOWS_UNICODE)"],
  [AS_CASE([$ocaml_cv_cc_vendor],
    [clang-*],
      [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
      internal_cflags="$gcc_warnings"],
    [gcc-[012]-*],
      # Some versions known to miscompile OCaml, e,g, 2.7.2.1, some 2.96.
      # Plus: C99 support unknown.
      [AC_MSG_ERROR([This version of GCC is too old.
        Please use GCC version 4.2 or above.])],
    [gcc-3-*|gcc-4-[01]],
      # No -fwrapv option before GCC 3.4.
      # Known problems with -fwrapv fixed in 4.2 only.
      [AC_MSG_WARN([This version of GCC is rather old.
        Reducing optimization level."]);
      AC_MSG_WARN([Consider using GCC version 4.2 or above.]);
      common_cflags="-std=gnu99 -O";
      internal_cflags="$gcc_warnings"],
    [gcc-4-*],
      [common_cflags="-std=gnu99 -O2 -fno-strict-aliasing -fwrapv \
-fno-builtin-memcmp";
      internal_cflags="$gcc_warnings"],
    [gcc-*],
      [common_cflags="-O2 -fno-strict-aliasing -fwrapv";
      internal_cflags="$gcc_warnings"],
    [msvc-*],
      [common_cflags="-nologo -O2 -Gy- -MD"
      common_cppflags="-D_CRT_SECURE_NO_DEPRECATE"
      internal_cppflags='-DUNICODE -D_UNICODE'
      internal_cppflags="$internal_cppflags -DWINDOWS_UNICODE="
      internal_cppflags="${internal_cppflags}\$(WINDOWS_UNICODE)"],
    [common_cflags="-O"])])

internal_cppflags="-DCAML_NAME_SPACE $internal_cppflags"

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

AS_IF([test x"$enable_shared" = "xno"],[with_sharedlibs=false])

AS_CASE([$CC,$host],
  [*,*-*-darwin*],
    [mkexe="$mkexe -Wl,-no_compact_unwind";
    AC_DEFINE([HAS_ARCH_CODE32], [1])],
  [*,*-*-haiku*], [mathlib=""],
  [*,*-*-cygwin*],
    [AS_CASE([$target],
      [i686-*], [flavor=cygwin],
      [x86_64-*], [flavor=cygwin64],
      [AC_MSG_ERROR([unknown cygwin variant])])
    common_cppflags="$common_cppflags -U_WIN32"
    AS_IF([$with_sharedlibs],
      [flexlink="flexlink -chain $flavor -merge-manifest -stack 16777216"
      flexdir=`$flexlink -where | tr -d '\015'`
      AS_IF([test -z "$flexdir"],
        [AC_MSG_WARN(
          [flexlink not found: native shared libraries won't be available.]
        )
        with_sharedlibs=false],
        [iflexdir="-I\"$flexdir\""
        mkexe="$flexlink -exe"
        mkexedebugflag="-link -g"]
      )]
    )
    AS_IF([! $with_sharedlibs],
      [mkexe="$mkexe -Wl,--stack,16777216"
      oc_ldflags="-Wl,--stack,16777216"]
    )
    ostype="Cygwin"],
  [*,*-*-mingw32],
    [AS_IF([$with_sharedlibs],
      [AS_CASE([$host],
        [i686-*-*], [flexdll_chain="mingw"],
        [x86_64-*-*], [flexdll_chain="mingw64"])
      flexlink="flexlink -chain $flexdll_chain -merge-manifest -stack 16777216"
      flexdir=`$flexlink -where | tr -d '\015'`
      AS_IF([test -z "$flexdir"], [flexdir='$(ROOTDIR)/flexdll'])
      iflexdir="-I\"$flexdir\""
      mkexedebugflag="-link -g"])
    ostype="Win32"
    toolchain="mingw"
    mkexe='$(MKEXE_ANSI) $(if $(OC_LDFLAGS),-link "$(OC_LDFLAGS)")'
    oc_ldflags='-municode'
    SO="dll"],
  [*,*-pc-windows],
    [toolchain=msvc
    ostype="Win32"
    mkexe='$(MKEXE_ANSI) $(if $(OC_LDFLAGS),-link "$(OC_LDFLAGS)")'
    oc_ldflags='/ENTRY:wmainCRTStartup'
    AS_CASE([$host],
      [i686-pc-windows], [flexdll_chain=msvc],
      [x86_64-pc-windows], [flexdll_chain=msvc64])
    AS_IF([$with_sharedlibs],
      [flexlink="flexlink -chain $flexdll_chain -merge-manifest -stack 16777216"
      flexdir=`$flexlink -where | tr -d '\015'`
      AS_IF([test -z "$flexdir"], [flexdir='$(ROOTDIR)/flexdll'])
      iflexdir="-I\"$flexdir\""
      mkexedebugflag="-link -g"])],
  [*,x86_64-*-linux*],
    AC_DEFINE([HAS_ARCH_CODE32], [1])
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
AC_CHECK_HEADER([sys/shm.h],[AC_DEFINE([HAS_SYS_SHM_H])])
AC_CHECK_HEADER([dirent.h], [AC_DEFINE([HAS_DIRENT])], [],
  [#include <sys/types.h>])

AC_CHECK_HEADER([sys/select.h], [AC_DEFINE([HAS_SYS_SELECT_H])], [],
  [#include <sys/types.h>])

# Checks for types

## off_t
AC_TYPE_OFF_T
AC_DEFINE([HAS_OFF_T])

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
  [AC_DEFINE([ARCH_BIG_ENDIAN], [1])],
  [],
  [AC_MSG_ERROR([could not determine endianness.])],
  [AC_MSG_ERROR([unable to handle universal endianness])]
)

AC_CHECK_ALIGNOF([double])
AC_CHECK_ALIGNOF([long])

AS_IF([! $arch64],
  [AS_IF([test x"$ac_cv_alignof_double" = "x8" ],
    [AS_IF([test "$target_cpu" != "i686" ],
      [AC_DEFINE([ARCH_ALIGN_DOUBLE], [1])])])
  AS_IF([test x"$ac_cv_alignof_long" = "x8" ],
    [AC_DEFINE([ARCH_ALIGN_INT64], [1])]
  )
  ]
)

# Shared library support

shared_libraries_supported=false
sharedlib_cflags=''
mksharedlib='shared-libs-not-available'
rpath=''
mksharedlibrpath=''
natdynlinkopts=""

AS_IF([test x"$enable_shared" != "xno"],
  [AS_CASE([$host],
    [*-apple-darwin*],
      [mksharedlib="$CC -shared -flat_namespace -undefined suppress \
                   -Wl,-no_compact_unwind"
      shared_libraries_supported=true],
    [*-*-mingw32],
      [mksharedlib='$(FLEXLINK)'
      mkmaindll='$(FLEXLINK) -maindll'
      shared_libraries_supported=$with_sharedlibs],
    [*-pc-windows],
      [mksharedlib='$(FLEXLINK)'
      mkmaindll='$(FLEXLINK) -maindll'
      shared_libraries_supported=$with_sharedlibs],
    [*-*-cygwin*],
      [mksharedlib="$flexlink"
      mkmaindll="$flexlink -maindll"
      shared_libraries_supported=true],
    [[*-*-linux*|*-*-freebsd[3-9]*|*-*-freebsd[1-9][0-9]*\
    |*-*-openbsd*|*-*-netbsd*|*-*-dragonfly*|*-*-gnu*|*-*-haiku*]],
      [sharedlib_cflags="-fPIC"
      mksharedlib="$CC -shared"
      oc_ldflags="$oc_ldflags -Wl,-E"
      rpath="-Wl,-rpath,"
      mksharedlibrpath="-Wl,-rpath,"
      natdynlinkopts="-Wl,-E"
      shared_libraries_supported=true])])

AS_IF([test -z "$mkmaindll"], [mkmaindll=$mksharedlib])

# Configure native dynlink

natdynlink=false

AS_IF([test x"$enable_shared" != "xno"],
  [AS_CASE(["$host"],
    [*-*-cygwin*], [natdynlink=true],
    [*-*-mingw32], [natdynlink=true],
    [*-pc-windows], [natdynlink=true],
    [[i[3456]86-*-linux*]], [natdynlink=true],
    [[i[3456]86-*-gnu*]], [natdynlink=true],
    [[x86_64-*-linux*]], [natdynlink=true],
    [[i[3456]86-*-darwin*]], [AS_IF([$arch64], [natdynlink=true])],
    [x86_64-*-darwin*], [natdynlink=true],
    [s390x*-*-linux*], [natdynlink=true],
    [powerpc*-*-linux*], [natdynlink=true],
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
    [aarch64-*-linux*], [natdynlink=true])])

# Try to work around the Skylake/Kaby Lake processor bug.
AS_CASE(["$CC,$host"],
  [*gcc*,x86_64-*|*gcc*,i686-*],
    [OCAML_CC_HAS_FNO_TREE_VRP
    AS_IF([$cc_has_fno_tree_vrp],
      [internal_cflags="$internal_cflags -fno-tree-vrp"])])

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
  [[i[3456]86-*-darwin*]],
    [AS_IF([$arch64], [arch=amd64], [arch=i386]); system=macosx],
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
  [[powerpc-*-netbsd*]],
    [arch=power; model=ppc; system=netbsd],
  [[powerpc-*-openbsd*]],
    [arch=power; model=ppc; system=bsd_elf],
  [[s390x*-*-linux*]],
    [arch=s390x; model=z10; system=elf],
  [armv6*-*-linux-gnueabihf],
    [arch=arm; model=armv6; system=linux_eabihf],
  [armv7*-*-linux-gnueabihf],
    [arch=arm; model=armv7; system=linux_eabihf],
  [armv8*-*-linux-gnueabihf],
    [arch=arm; model=armv8; system=linux_eabihf],
  [armv8*-*-linux-gnueabi],
    [arch=arm; model=armv8; system=linux_eabi],
  [armv7*-*-linux-gnueabi],
    [arch=arm; model=armv7; system=linux_eabi],
  [armv6t2*-*-linux-gnueabi],
    [arch=arm; model=armv6t2; system=linux_eabi],
  [armv6*-*-linux-gnueabi],
    [arch=arm; model=armv6; system=linux_eabi],
  [armv6*-*-freebsd*],
    [arch=arm; model=armv6; system=freebsd],
  [earmv6*-*-netbsd*],
    [arch=arm; model=armv6; system=netbsd],
  [earmv7*-*-netbsd*],
    [arch=arm; model=armv7; system=netbsd],
  [armv5te*-*-linux-gnueabi],
    [arch=arm; model=armv5te; system=linux_eabi],
  [armv5*-*-linux-gnueabi],
    [arch=arm; model=armv5; system=linux_eabi],
  [arm*-*-linux-gnueabi],
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
  [x86_64-*-freebsd*],
    [arch=amd64; system=freebsd],
  [x86_64-*-netbsd*],
    [arch=amd64; system=netbsd],
  [x86_64-*-openbsd*],
    [arch=amd64; system=openbsd],
  [x86_64-*-darwin*],
    [arch=amd64; system=macosx],
  [x86_64-*-mingw32],
    [arch=amd64; system=mingw64],
  [aarch64-*-linux*],
    [arch=arm64; system=linux],
  [x86_64-*-cygwin*],
    [arch=amd64; system=cygwin]
)

AS_IF([test x"$enable_native_compiler" = "xno"],
  [arch=none; model=default; system=unknown; native_compiler=false
  AC_MSG_NOTICE([the native compiler is disabled])],
  [native_compiler=true])

AS_IF([! $native_compiler], [natdynlink=false])

AS_IF([$natdynlink], [cmxs="cmxs"], [cmxs="cmx"])

AC_DEFINE_UNQUOTED([OCAML_OS_TYPE], ["$ostype"])

AC_CHECK_TOOL([DIRECT_LD],[ld])
AS_IF([test -z "$PARTIALLD"],
  # The string for PACKLD must be capable of being concatenated with the
  # output filename. Don't assume that all C compilers understand GNU -ofoo
  # form, so ensure that the definition includes a space at the end (which is
  # achieved using the $(EMPTY) expansion trick).
  [AS_CASE(["$arch,$CC,$system,$model"],
    [amd64,gcc*,macosx,*], [PACKLD='ld -r -arch x86_64 -o $(EMPTY)'],
    [amd64,gcc*,solaris,*], [PACKLD='ld -r -m elf_x86_64 -o $(EMPTY)'],
    [power,gcc*,elf,ppc], [PACKLD='ld -r -m elf32ppclinux -o $(EMPTY)'],
    [power,gcc*,elf,ppc64], [PACKLD='ld -r -m elf64ppc -o $(EMPTY)'],
    [power,gcc*,elf,ppc64le], [PACKLD='ld -r -m elf64lppc -o $(EMPTY)'],
    # For the Microsoft C compiler there must be no space at the end of the
    # string.
    [*,cl,*,*], [PACKLD="link -lib -nologo $machine -out:"],
    [PACKLD="$DIRECT_LD -r -o \$(EMPTY)"])],
  [PACKLD="$PARTIALLD -o \$(EMPTY)"])

AS_IF([test $arch != "none" && $arch64 ],
  [otherlibraries="$otherlibraries raw_spacetime_lib"])

# Profiling

AS_CASE(["$arch,$system"],
  [i386,linux_elf], [profiling=true],
  [i386,gnu], [profiling=true],
  [i386,bsd_elf], [profiling=true],
  [amd64,macosx], [profiling=true],
  [i386,macosx], [profiling=true],
  [amd64,linux], [profiling=true],
  [amd64,openbsd], [profiling=true],
  [amd64,freebsd], [profiling=true],
  [amd64,netbsd], [profiling=true],
  [arm,netbsd], [profiling=true],
  [amd64,gnu], [profiling=true],
  [arm,linux*], [profiling=true],
  [power,elf], [profiling=true],
  [power,bsd*], [profiling=true],
  [power,netbsd], [profiling=true],
  [profiling=false])

# Assembler

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [asppprofflags=''],
  [asppprofflags='-DPROFILING'])

AS_IF([test -n "$host_alias"], [toolpref="${host}-"], [toolpref=""])

# We first compute default values for as and aspp
# If values have been given by the user then they take precedence over
# those just computed
# One may want to check whether the user provided values first
# and only compute values if none has been provided

AS_CASE(["$arch,$system"],
  [i386,win32],
    [default_as="ml -nologo -coff -Cp -c -Fo"],
  [amd64,win64],
    [default_as="ml64 -nologo -Cp -c -Fo"],
  [amd64,macosx],
    [AS_CASE([$ccfamily],
      [clang-*],
        [default_as='clang -arch x86_64 -Wno-trigraphs -c'
        default_aspp='clang -arch x86_64 -Wno-trigraphs -c'],
      [default_as="${toolpref}as -arch x86_64"
      default_aspp="${toolpref}gcc -arch x86_64 -c"])],
  [amd64,solaris],
    [default_as="${toolpref}as --64"
    default_aspp="${toolpref}gcc -m64 -c"],
  [i386,solaris],
    [default_as="${toolpref}as"
    default_aspp="${toolpref}gcc -c"],
  [power,elf],
    [AS_CASE([$model],
      [ppc64le],
        [default_as="${toolpref}as -a64 -mpower8"
        default_aspp="${toolpref}gcc -m64 -mcpu=powerpc64le -c"],
      [ppc64],
        [default_as="${toolpref}as -a64 -mppc64"
        default_aspp="${toolpref}gcc -m64 -c"],
      [ppc],
        [default_as="${toolpref}as -mppc"
        default_aspp="${toolpref}gcc -m32 -c"])],
  [s390x,elf],
    [default_as="${toolpref}as -m 64 -march=$model"
    default_aspp="${toolpref}gcc -c -Wa,-march=$model"],
  [arm,freebsd],
    [default_as="${toolpref}cc -c"
    default_aspp="${toolpref}cc -c"],
  [*,dragonfly],
    [default_as="${toolpref}as"
    default_aspp="${toolpref}cc -c"],
  [*,freebsd],
    [default_as="${toolpref}as"
    default_aspp="${toolpref}cc -c"],
  [amd64,*|arm,*|arm64,*|i386,*|power,bsd*|power,netbsd],
    [default_as="${toolpref}as"
    AS_CASE([$ccfamily],
      [clang-*], [default_aspp="${toolpref}clang -c -Wno-trigraphs"],
      [default_aspp="${toolpref}gcc -c"])])

AS_IF([test -z "$AS"], [AS="$default_as"])

AS_IF([test -z "$ASPP"], [ASPP="$default_aspp"])

# Checks for library functions

## Check the semantics of signal handlers
OCAML_SIGNAL_HANDLERS_SEMANTICS

## Check for C99 float ops

# Note: this was disabled on Windows but the autoconf-generated script
# does find the function it is looking for.
# however the fma test does not pass so we disable the feature
# for the moment, to be backward-compatible

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [],
  [has_c99_float_ops=true
  AC_CHECK_FUNC([expm1], [], [has_c99_float_ops=false])
  AS_IF([$has_c99_float_ops],
    [AC_CHECK_FUNC([log1p], [], [has_c99_float_ops=false])])
  AS_IF([$has_c99_float_ops],
    [AC_CHECK_FUNC([hypot], [], [has_c99_float_ops=false])])
  AS_IF([$has_c99_float_ops],
    [AC_CHECK_FUNC([fma], [
      AS_CASE([$target],[x86_64-*-cygwin],[],[AC_DEFINE([HAS_WORKING_FMA])])],
      [has_c99_float_ops=false])])
  AS_IF([$has_c99_float_ops],
    [AC_CHECK_FUNC([copysign], [AC_DEFINE([HAS_C99_FLOAT_OPS])])])])

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

CPP_FLAGS="$saved_CPPFLAGS"

## issetugid

AC_CHECK_FUNC([issetugid], [AC_DEFINE([HAS_ISSETUGID])])

## clock_gettime, for the instrumented runtime

## Note: on MinGW, configure finds a clock_gettime and thus the build
# system tries to build the instrumented runtime, which causes
# warnings. For the moment we simply disable it on MinGW
# but this would need to be further investigated
AS_CASE([$host],
  [*-*-mingw32], [instrumented_runtime=false],
  [AS_IF([test "x$enable_instrumented_runtime" = "xno" ],
    [instrumented_runtime=false; instrumented_runtime_libs=""],
    [
      AC_SEARCH_LIBS([clock_gettime], [rt],
        [has_clock_gettime=true],
        [has_clock_gettime=false])
      AS_CASE([$enable_instrumented_runtime,$has_clock_gettime],
        [*,true],
          [instrumented_runtime=true
          AS_IF([test "x$ac_cv_search_clock_gettime" = "xnone required"],
            [instrumented_runtime_libs=""],
            [instrumented_runtime_libs=$ac_cv_search_clock_gettime])],
        [auto,*],
          [instrumented_runtime=false],
        [yes,*],
          [AC_MSG_ERROR([the instrumented runtime can not be built])])])])

## Sockets

## TODO: check whether the different libraries are really useful

sockets=false

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [cclibs="$cclibs -lws2_32"
    sockets=true],
  [
    AC_CHECK_FUNC([socket])
    AC_CHECK_FUNC([socketpair])
    AC_CHECK_FUNC([bind])
    AC_CHECK_FUNC([listen])
    AC_CHECK_FUNC([accept])
    AC_CHECK_FUNC([connect])
    sockets=true
  ]
)

AS_IF([$sockets], [AC_DEFINE([HAS_SOCKETS])])

## socklen_t in sys/socket.h

AC_CHECK_TYPE(
  [socklen_t],
  [AC_DEFINE([HAS_SOCKLEN_T])], [],
  [#include <sys/socket.h>])

AC_CHECK_FUNC([inet_aton], [AC_DEFINE([HAS_INET_ATON])])

## IPv6 support

ipv6=true

AC_CHECK_TYPE(
  [struct sockaddr_in6], [], [ipv6=false],
[
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
]
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

AC_CHECK_FUNC([getpriority],
  [AC_CHECK_FUNC([setpriority],
    [AC_DEFINE([HAS_GETPRIORITY])])])

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

AC_CHECK_FUNC([dup2], [AC_DEFINE([HAS_DUP2])])

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

## Asynchronous I/O

AC_MSG_CHECKING([for asynchronous I/O])
AC_RUN_IFELSE(
  [AC_LANG_SOURCE([[
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

int signalled;

void sigio_handler(int arg)
{
  signalled = 1;
}

int main(void)
{
#if defined(SIGIO) && defined(FASYNC) && defined(F_SETFL) && defined(F_SETOWN)
  int p[2];
  int ret;
#define OUT 0
#define IN 1
  if (socketpair(PF_UNIX, SOCK_STREAM, 0, p) == -1) return 1;
  signalled = 0;
  signal(SIGIO, sigio_handler);
  ret = fcntl(p[OUT], F_GETFL, 0);
  fcntl(p[OUT], F_SETFL, ret | FASYNC);
  fcntl(p[OUT], F_SETOWN, getpid());
  switch(fork()) {
  case -1:
    return 1;
  case 0:
    close(p[OUT]);
    write(p[IN], "x", 1);
    sleep(1);
    exit(0);
  default:
    close(p[IN]);
    while(wait(NULL) == -1 && errno == EINTR) /*nothing*/;
  }
  if (signalled) return 0; else return 1;
#else
  return 1;
#endif
}
  ]])],
  [
    AC_DEFINE([HAS_ASYNC_IO])
    AC_MSG_RESULT([yes])
  ],
  [AC_MSG_RESULT([no])],
  [AC_MSG_RESULT([no])]
)

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
AS_IF([$shared_libraries_supported],
  [AS_CASE([$host],
    [*-*-mingw32|*-pc-windows],
      [supports_shared_libraries=$shared_libraries_supported; dllibs=""],
    [AC_CHECK_FUNC([dlopen],
      [supports_shared_libraries=true dllibs=""],
      [AC_CHECK_LIB([dl], [dlopen],
        [supports_shared_libraries=true dllibs="-ldl $otherdllibs"],
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
AS_CASE([$host],
  [*-*-mingw32|*-pc-windows], [cc_has_debug_prefix_map=false],
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

## execvpe

AC_CHECK_FUNC([execvpe], [AC_DEFINE([HAS_EXECVPE])])

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
  [i386,linux_elf|amd64,linux|amd64,macosx|i386,macosx \
    |amd64,openbsd|i386,bsd_elf],
    [AC_DEFINE([HAS_STACK_OVERFLOW_DETECTION])
    AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])])

## Determine if the POSIX threads library is supported

AS_IF([test x"$enable_systhreads" = "xno"],
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
      AS_CASE([$host],
        [*-*-solaris*], [pthread_link="-lpthread -lposix4"],
        [*-*-haiku*], [pthread_link=""],
        [pthread_link="-lpthread"])
      common_cppflags="$common_cppflags -D_REENTRANT"
      AC_MSG_NOTICE([the POSIX threads library is supported])
      saved_CFLAGS="$CFLAGS"
      saved_LIBS="$LIBS"
      CFLAGS="$CFLAGS $PTHREAD_CFLAGS"
      LIBS="$LIBS $pthread_link"
      AC_CHECK_FUNC([sigwait], [AC_DEFINE([HAS_SIGWAIT])])
      LIBS="$saved_LIBS"
      CFLAGS="$saved_CFLAGS"],
      [AS_IF([test x"$enable_systhreads" = "xyes"],
        [AC_MSG_ERROR([the POSIX thread library is not available])],
        [systhread_support=false
        AC_MSG_NOTICE([the POSIX threads library is not supported])])])])])

## Determine if the bytecode thread library is supported

AS_IF([test x"$enable_vmthreads" = "xno"],
  [AC_MSG_NOTICE([the bytecode threads library is disabled])],
  [AS_IF([$select && $setitimer && $gettimeofday && $wait],
    [otherlibraries="$otherlibraries threads"
    AC_MSG_NOTICE([the bytecode threads library is supported])],
    [AS_IF([test x"$enable_vmthreads" = "xyes"],
      [AC_MSG_NOTICE([the bytecode threads library is not available])],
      [AC_MSG_NOTICE([the bytecode threads library is not supported])])])])

## XWindow

AS_IF([test x"$enable_graph_lib" = "xno" ],
  [AC_MSG_NOTICE([the graph library has been disabled])],
  [AS_CASE([$host],
    [*-*-mingw32|*-pc-windows], [otherlibraries="$otherlibraries win32graph"],
    [AC_PATH_X
    AS_IF([test -z "$no_x"],
      [AS_IF([test -z $x_libraries],
        [x_libraries="-lX11"],
        [x_libraries="-L$x_libraries -lX11"])
      AC_MSG_NOTICE([X has been found])
      otherlibraries="$otherlibraries graph"
      AC_MSG_NOTICE([the graph library will be built])],
      [AC_MSG_NOTICE([X has not been found])
      AS_IF([test x"$enable_graph_lib" = "xyes" ],
        [AC_MSG_ERROR([can not build the graph library which was requested])],
        [AC_MSG_NOTICE([the graph library will not be built])])])])])

## libbfd

AC_CHECK_HEADER([bfd.h],
  [libbfd_ling=""
  AC_CHECK_LIB([bfd], [bfd_openr], [libbfd_link="-lbfd"])
  AS_IF([test -z "$libbfd_link"],
    [AC_CHECK_LIB([bfd], [bfd_openr], [libbfd_link="-lbfd -ldl"], [], [-ldl])])
  AS_IF([test -z "$libbfd_link"],
    [AC_CHECK_LIB([bfd], [bfd_openr],
      [libbfd_link="-lbfd -ldl -liberty"], [], [-ldl -liberty])])
  AS_IF([test -z "$libbfd_link"],
    [AC_CHECK_LIB([bfd], [bfd_openr],
      [libbfd_link="-lbfd -ldl -liberty -lz"], [], [-ldl -liberty -lz])])
  AS_IF([test -z "$libbfd_link"],
    [AC_CHECK_LIB([bfd], [bfd_openr],
      [libbfd_link="-lbfd -ldl -liberty -lz -lintl"], [],
      [-ldl -liberty -lz -lintl])])
  AS_IF([test -n "$libbfd_link"],
    [AC_DEFINE([HAS_LIBBFD])],
    [AC_MSG_NOTICE(m4_normalize([
      BFD library not found, 'objinfo' will be unable to display
      info on .cmxs files.
    ]))])
  ])

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
  [AS_CASE(["$host,$CC"],
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
  [AC_DEFINE([NO_NAKED_POINTERS])])

## Check for mmap support for huge pages and contiguous heap
OCAML_MMAP_SUPPORTS_HUGE_PAGES

# Spacetime profiling, including libunwind detection

# The number of bits used for profiling information is configurable here.
# The more bits used for profiling, the smaller will be Max_wosize.
# Note that PROFINFO_WIDTH must still be defined even if not configuring
# for Spacetime (see comment in runtime/caml/mlvalues.h on [Profinfo_hd]).
AC_MSG_CHECKING([whether to build spacetime])
AS_IF([test x"$enable_spacetime" != "xyes" ],
  [spacetime=false
  call_counts=true # as in original script but should probably be false
  libunwind_available=false
  libunwind_include_flags=
  libunwind_link_flags=
  AC_MSG_RESULT([no])],
  [AS_CASE([$arch],
    [amd64], [spacetime_supported=true],
    [spacetime_supported=false])
  AS_IF([$spacetime_supported],
    [AC_MSG_RESULT([yes])
    spacetime=true
    profinfo=true
    profinfo_width=26
    AC_DEFINE([WITH_SPACETIME])
    AS_IF([test x"$enable_call_counts" != "xno"],
      [call_counts=true
      AC_DEFINE([ENABLE_CALL_COUNTS])],
      [call_counts=false])
    AC_MSG_CHECKING([whether to use libunwind])
    AS_IF([test x"$with_libunwind" = "xno"],
      [AC_MSG_RESULT([disabled])],
      [AC_CHECK_HEADER([libunwind.h],
        [AC_DEFINE([HAS_LIBUNWIND])
        AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])
      ])
    ],
    [AS_IF([test x"$enable_spacetime" = "xyes"],
      [AC_MSG_RESULT([requested but not supported])
      AC_MSG_ERROR([exiting])],
      [AC_MSG_RESULT([no])])
    ])
  ])



AC_DEFINE_UNQUOTED([PROFINFO_WIDTH], [$profinfo_width])
AS_IF([$profinfo], [AC_DEFINE([WITH_PROFINFO])])

## PIC

AS_IF([test "$with_pic"],
  [fpic=true
  AC_DEFINE([CAML_WITH_FPIC])
  common_cflags="$common_cflags $sharedlib_cflags"
  aspp="$aspp $sharedlib_cflags"],
  [fpic=false])

AS_IF([test x"$enable_installing_bytecode_programs" = "xno"],
  [install_bytecode_programs=false],
  [install_bytecode_programs=true])

AS_IF([test x"$enable_installing_source_artifacts" = "xno"],
  [install_source_artifacts=false],
  [install_source_artifacts=true])

AS_IF([test x"$enable_ocamldoc" = "xno"],
  [ocamldoc=""],
  [ocamldoc=ocamldoc])

AS_IF([test x"$enable_flambda" = "xyes"],
  [flambda=true
  AS_IF([test x"$enable_flambda_invariants" = "xyes"],
    [flambda_invariants=true],
    [flambda_invariants=false])],
  [flambda=false
  flambda_invariants=false])

AS_IF([test x"$enable_flat_float_array" = "xno"],
  [flat_float_array=false],
  [AC_DEFINE([FLAT_FLOAT_ARRAY])
  flat_float_array=true])

AS_IF([test x"$with_afl" = "xyes"],
  [afl=true],
  [afl=false])

AS_IF([test x"$enable_force_safe_string" = "xyes"],
  [AC_DEFINE([CAML_SAFE_STRING])
  force_safe_string=true],
  [force_safe_string=false])

AS_IF([test x"$with_default_string" = "xunsafe"],
  [default_safe_string=false],
  [default_safe_string=true])

oc_cflags="$common_cflags $internal_cflags"
oc_cppflags="$common_cppflags $internal_cppflags"
ocamlc_cflags="$common_cflags $sharedlib_cflags"
ocamlc_cppflags="$common_cppflags"
ocamlopt_cflags="$common_cflags"
ocamlopt_cppflags="$common_cppflags"
cclibs="$cclibs $mathlib"

AS_CASE([$host],
  [*-*-mingw32],
    [bytecclibs="-lws2_32 -lversion"
    nativecclibs="-lws2_32 -lversion"],
  [*-pc-windows],
    [bytecclibs="advapi32.lib ws2_32.lib version.lib"
    nativecclibs="advapi32.lib ws2_32.lib version.lib"],
  [bytecclibs="$cclibs $dllibs $pthread_link $instrumented_runtime_libs"
  nativecclibs="$cclibs $dllibs"])

AS_IF([test x"$libdir" = x'${exec_prefix}/lib'],
  [libdir="$libdir"/ocaml])

AS_IF([test x"$mandir" = x'${datarootdir}/man'],
  [mandir='${prefix}/man'])

AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [max_testsuite_dir_retries=1
    AS_CASE([$WINDOWS_UNICODE_MODE],
      [ansi],
        [windows_unicode=0],
      [compatible|""],
        [windows_unicode=1],
      [AC_MSG_ERROR([unexpected windows unicode mode])])],
  [max_testsuite_dir_retries=0
  windows_unicode=0])

# Define flexlink chain and flags correctly for the different Windows ports
AS_CASE([$host],
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

# Define default prefix correctly for the different Windows ports
AS_IF([test x"$prefix" = "xNONE"],
  [AS_CASE([$host],
    [i686-w64-mingw32], [prefix='C:/ocamlmgw'],
    [x86_64-w64-mingw32], [prefix='C:/ocamlmgw64'],
    [i686-pc-windows], [prefix='C:/ocamlms'],
    [x86_64-pc-windows], [prefix='C:/ocamlms64'])])

# Define a few macros that were defined in config/m-nt.h
# but whose value is not guessed properly by configure
# (all this should be understood and fixed)
AS_CASE([$host],
  [*-*-mingw32|*-pc-windows],
    [AC_DEFINE([HAS_BROKEN_PRINTF])
    AC_DEFINE([HAS_STRERROR])
    AC_DEFINE([HAS_IPV6])
    AC_DEFINE([HAS_NICE])])

AC_OUTPUT
