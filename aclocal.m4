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

# This file is processed by autoconf.
# It contains macro definitions specific to the OCaml package.
# Every macro defined here should have its name prefixed with OCAML_.

# libtool macros

# Since the project does not use automake, the libtool macro files
# need to be manually included
m4_include([build-aux/libtool.m4])
m4_include([build-aux/ltoptions.m4])
m4_include([build-aux/ltsugar.m4])
m4_include([build-aux/ltversion.m4])
m4_include([build-aux/lt~obsolete.m4])

# Macros from the autoconf macro archive
m4_include([build-aux/ax_func_which_gethostbyname_r.m4])
m4_include([build-aux/ax_pthread.m4])

# OCaml version
m4_include([build-aux/ocaml_version.m4])

# The following macro figures out which C compiler is used.
# It does so by checking for compiler-specific predefined macros.
# A list of such macros can be found at
# https://sourceforge.net/p/predef/wiki/Compilers/
AC_DEFUN([OCAML_CC_VENDOR], [
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PROG_CPP])
  AC_MSG_CHECKING([C compiler vendor])
  AC_PREPROC_IFELSE(
    [AC_LANG_SOURCE([
#if defined(_MSC_VER)
msvc _MSC_VER
#elif defined(__INTEL_COMPILER)
icc __INTEL_COMPILER
#elif defined(__clang_major__) && defined(__clang_minor__)
clang __clang_major__ __clang_minor__
#elif defined(__GNUC__) && defined(__GNUC_MINOR__)
gcc __GNUC__ __GNUC_MINOR__
#elif defined(__xlc__) && defined(__xlC__)
xlc __xlC__ __xlC_ver__
#elif defined(__SUNPRO_C)
sunc __SUNPRO_C __SUNPRO_C
#else
unknown
#endif]
    )],
    [AC_CACHE_VAL([ocaml_cv_cc_vendor],
      [ocaml_cv_cc_vendor=`grep ['^[a-z]'] conftest.i | tr -s ' ' '-' \
                                                      | tr -d '\r'`])],
    [AC_MSG_FAILURE([unexpected preprocessor failure])])
  AC_MSG_RESULT([$ocaml_cv_cc_vendor])
])

AC_DEFUN([OCAML_SIGNAL_HANDLERS_SEMANTICS], [
  AC_MSG_NOTICE([checking semantics of signal handlers])
  AC_CHECK_FUNC([sigaction], [has_sigaction=true], [has_sigaction=false])
  AC_CHECK_FUNC([sigprocmask], [has_sigprocmask=true], [has_sigprocmask=false])
  AS_IF([$has_sigaction && $has_sigprocmask],
    [AC_DEFINE([POSIX_SIGNALS])
      AC_MSG_NOTICE([POSIX signal handling found.])],
    [AC_MSG_NOTICE([assuming signals have the System V semantics.])
    ]
  )
])

AC_DEFUN([OCAML_CC_HAS_FNO_TREE_VRP], [
  AC_MSG_CHECKING([whether the C compiler supports -fno-tree-vrp])
  saved_CFLAGS="$CFLAGS"
  CFLAGS="-Werror -fno-tree-vrp $CFLAGS"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([int main() { return 0; }])],
    [cc_has_fno_tree_vrp=true
    AC_MSG_RESULT([yes])],
    [cc_has_fno_tree_vrp=false
    AC_MSG_RESULT([no])])
  CFLAGS="$saved_CFLAGS"
])

AC_DEFUN([OCAML_CC_SUPPORTS_ALIGNED], [
  AC_MSG_CHECKING([whether the C compiler supports __attribute__((aligned(n)))])
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([typedef struct {__attribute__((aligned(8))) int t;} t;])],
    [AC_DEFINE([SUPPORTS_ALIGNED_ATTRIBUTE])
    AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])])])

AC_DEFUN([OCAML_CC_SUPPORTS_TREE_VECTORIZE], [
  AC_MSG_CHECKING(
 [whether the C compiler supports __attribute__((optimize("tree-vectorize")))])
  saved_CFLAGS="$CFLAGS"
  CFLAGS="-Werror $CFLAGS"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([
       __attribute__((optimize("tree-vectorize"))) void f(void){}
       int main() { f(); return 0; }
    ])],
    [AC_DEFINE([SUPPORTS_TREE_VECTORIZE])
    AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])])
  CFLAGS="$saved_CFLAGS"
])

AC_DEFUN([OCAML_CC_HAS_DEBUG_PREFIX_MAP], [
  AC_MSG_CHECKING([whether the C compiler supports -fdebug-prefix-map])
  saved_CFLAGS="$CFLAGS"
  CFLAGS="-fdebug-prefix-map=old=new $CFLAGS"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([int main() { return 0; }])],
    [cc_has_debug_prefix_map=true
    AC_MSG_RESULT([yes])],
    [cc_has_debug_prefix_map=false
    AC_MSG_RESULT([no])])
  CFLAGS="$saved_CFLAGS"
])

AC_DEFUN([OCAML_CL_HAS_VOLATILE_METADATA], [
  AC_MSG_CHECKING([whether the C compiler supports -d2VolatileMetadata-])
  saved_CFLAGS="$CFLAGS"
  CFLAGS="-d2VolatileMetadata- $CFLAGS"
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([int main() { return 0; }])],
    [cl_has_volatile_metadata=true
    AC_MSG_RESULT([yes])],
    [cl_has_volatile_metadata=false
    AC_MSG_RESULT([no])])
  CFLAGS="$saved_CFLAGS"
])

# Save C compiler related variables
AC_DEFUN([OCAML_CC_SAVE_VARIABLES], [
  saved_CC="$CC"
  saved_CFLAGS="$CFLAGS"
  saved_CPPFLAGS="$CPPFLAGS"
  saved_LIBS="$LIBS"
  saved_ac_ext="$ac_ext"
  saved_ac_compile="$ac_compile"
  # Move the content of confdefs.h to another file so it does not
  # get included
  mv confdefs.h confdefs.h.bak
  touch confdefs.h
])

# Restore the C compiler related variables
AC_DEFUN([OCAML_CC_RESTORE_VARIABLES], [
  # Restore the content of confdefs.h
  mv confdefs.h.bak confdefs.h
  ac_compile="$saved_ac_compile"
  ac_ext="$saved_ac_ext"
  CPPFLAGS="$saved_CPPFLAGS"
  CFLAGS="$saved_CFLAGS"
  CC="$saved_CC"
  LIBS="$saved_LIBS"
])

AC_DEFUN([OCAML_AS_HAS_DEBUG_PREFIX_MAP], [
  AC_MSG_CHECKING([whether the assembler supports --debug-prefix-map])

  OCAML_CC_SAVE_VARIABLES

  # Modify C-compiler variables to use the assembler
  CC="$AS"
  CFLAGS="--debug-prefix-map old=new -o conftest.$ac_objext"
  CPPFLAGS=""
  ac_ext="S"
  ac_compile='$CC $CFLAGS $CPPFLAGS conftest.$ac_ext >&5'

  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([
camlPervasives__loop_1128:
        .file   1       "pervasives.ml"
        .loc    1       193
    ])],
    [as_has_debug_prefix_map=true
    AC_MSG_RESULT([yes])],
    [ashas_debug_prefix_map=false
    AC_MSG_RESULT([no])])

  OCAML_CC_RESTORE_VARIABLES
])

AC_DEFUN([OCAML_AS_HAS_CFI_DIRECTIVES], [
  AC_MSG_CHECKING([whether the assembler supports CFI directives])

  AS_IF([test x"$enable_cfi" = "xno"],
    [AC_MSG_RESULT([disabled])],
    [OCAML_CC_SAVE_VARIABLES

    # Modify C-compiler variables to use the assembler
    CC="$ASPP"
    CFLAGS="-o conftest.$ac_objext"
    CPPFLAGS=""
    ac_ext="S"
    ac_compile='$CC $CFLAGS $CPPFLAGS conftest.$ac_ext >&5'

    AC_COMPILE_IFELSE(
      [AC_LANG_SOURCE([
camlPervasives__loop_1128:
        .file   1       "pervasives.ml"
        .loc    1       193
        .cfi_startproc
        .cfi_adjust_cfa_offset 8
        .cfi_endproc
      ])],
      [aspp_ok=true],
      [aspp_ok=false])

    AS_IF([test "$AS" = "$ASPP"],
      [as_ok="$aspp_ok"],
      [CC="$AS"
      ac_compile='$CC $CFLAGS $CPPFLAGS conftest.$ac_ext >&5'
      AC_COMPILE_IFELSE(
        [AC_LANG_SOURCE([
camlPervasives__loop_1128:
        .file   1       "pervasives.ml"
        .loc    1       193
        .cfi_startproc
        .cfi_adjust_cfa_offset 8
        .cfi_endproc
        ])],
        [as_ok=true],
        [as_ok=false])])

    OCAML_CC_RESTORE_VARIABLES

    AS_IF([$aspp_ok && $as_ok],
      [asm_cfi_supported=true
      AC_DEFINE([ASM_CFI_SUPPORTED])
      AC_MSG_RESULT([yes])],
      [AS_IF([test x"$enable_cfi" = "xyes"],
        [AC_MSG_RESULT([requested but not available
        AC_MSG_ERROR([exiting])])],
        [asm_cfi_supported=false
        AC_MSG_RESULT([no])])])
  ])])

AC_DEFUN([OCAML_MMAP_SUPPORTS_MAP_STACK], [
  AC_MSG_CHECKING([whether mmap supports MAP_STACK])
  AC_RUN_IFELSE(
    [AC_LANG_SOURCE([[
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char *argv[]){
  void *block;
  block = mmap (NULL, 4096, PROT_READ | PROT_WRITE,
                MAP_ANONYMOUS | MAP_PRIVATE | MAP_STACK,
                -1, 0);
  if (block == MAP_FAILED)
     return 1;
  return 0;
}
    ]])],
    [has_mmap_map_stack=true
    AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])],
    [AC_MSG_RESULT([no assumed])])
])

AC_DEFUN([OCAML_MMAP_SUPPORTS_HUGE_PAGES], [
  AC_MSG_CHECKING([whether mmap supports huge pages])
  AC_RUN_IFELSE(
    [AC_LANG_SOURCE([[
#include <sys/mman.h>
#include <stdio.h>
#include <stdlib.h>

#define huge_page_size (4 * 1024 * 1024)

/* Test for the possible availability of huge pages. Answer yes
   if the OS knows about huge pages, even if they are not available
   on the build machine at configure time, because (on Linux) huge
   pages can be activated and deactivated easily while the system
   is running.
*/

int main (int argc, char *argv[]){
  void *block;
  char *p;
  int i, res;
  block = mmap (NULL, huge_page_size, PROT_READ | PROT_WRITE,
                MAP_PRIVATE | MAP_ANONYMOUS | MAP_HUGETLB,
                -1, 0);
  if (block == MAP_FAILED){
    block = mmap (NULL, huge_page_size, PROT_READ | PROT_WRITE,
                  MAP_PRIVATE | MAP_ANONYMOUS,
                  -1, 0);
  }
  if (block == MAP_FAILED){
    perror ("mmap");
    return 3;
  }
  /*printf ("block = %p\n", block);*/
  p = (char *) block;
  for (i = 0; i < huge_page_size; i += 4096){
    p[i] = (char) i;
  }
  return 0;
}
    ]])],
    [AC_DEFINE([HAS_HUGE_PAGES])
    AC_DEFINE_UNQUOTED([HUGE_PAGE_SIZE], [(4 * 1024 * 1024)])
    AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])],
    [AC_MSG_RESULT([no assumed])])
])

AC_DEFUN([OCAML_CHECK_LIBUNWIND], [
  SAVED_CFLAGS="$CFLAGS"
  SAVED_LDFLAGS="$LDFLAGS"
  CFLAGS="$CFLAGS $libunwind_include_flags"
  LDFLAGS="$LDFLAGS $libunwind_link_flags"
  AC_CHECK_HEADER([libunwind.h],
    [AC_DEFINE([HAS_LIBUNWIND])
    libunwind_available=true],
    [libunwind_available=false])
  LDFLAGS="$SAVED_LDFLAGS"
  CFLAGS="$SAVED_CFLAGS"
])

AC_DEFUN([OCAML_TEST_FLEXLINK], [
  OCAML_CC_SAVE_VARIABLES

  AC_MSG_CHECKING([whether $1 works])

  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([int answer = 42;])],
    [# Create conftest1.$ac_objext as a symlink on Cygwin to ensure that native
    # flexlink can cope. The reverse test is unnecessary (a Cygwin-compiled
    # flexlink can read anything).
    mv conftest.$ac_objext conftest1.$ac_objext
    AS_CASE([$4],[*-pc-cygwin],
      [ln -s conftest1.$ac_objext conftest2.$ac_objext],
      [cp conftest1.$ac_objext conftest2.$ac_objext])

    CC="$1 -chain $2 -exe"
    LIBS="conftest2.$ac_objext"
    CPPFLAGS="$3 $CPPFLAGS"
    CFLAGS=""
    AC_LINK_IFELSE(
      [AC_LANG_SOURCE([int main() { return 0; }])],
      [AC_MSG_RESULT([yes])],
      [AC_MSG_RESULT([no])
      AC_MSG_ERROR([$1 does not work])])],
    [AC_MSG_RESULT([unexpected compile error])
    AC_MSG_ERROR([error calling the C compiler])])

  OCAML_CC_RESTORE_VARIABLES
])

AC_DEFUN([OCAML_TEST_FLEXDLL_H], [
  OCAML_CC_SAVE_VARIABLES

  AS_IF([test -n "$1"],[CPPFLAGS="-I $1 $CPPFLAGS"])
  have_flexdll_h=no
  AC_CHECK_HEADER([flexdll.h],[have_flexdll_h=yes],[have_flexdll_h=no])
  AS_IF([test x"$have_flexdll_h" = 'xno'],
    [AS_IF([test -n "$1"],
      [AC_MSG_ERROR([$1/flexdll.h appears unusable])])])

  OCAML_CC_RESTORE_VARIABLES
])

AC_DEFUN([OCAML_TEST_FLEXLINK_WHERE], [
  OCAML_CC_SAVE_VARIABLES

  AC_MSG_CHECKING([if "$1 -where" includes flexdll.h])
  flexlink_where="$($1 -where | tr -d '\r')"
  CPPFLAGS="$CPPFLAGS -I \"$flexlink_where\""
  cat > conftest.c <<"EOF"
#include <flexdll.h>
int main (void) {return 0;}
EOF
  cat > conftest.Makefile <<EOF
all:
	$CC -o conftest$ac_exeext $CFLAGS $CPPFLAGS $LDFLAGS conftest.c $LIBS
EOF
  AS_IF([make -f conftest.Makefile >/dev/null 2>/dev/null],
    [have_flexdll_h=yes
    AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])])

  OCAML_CC_RESTORE_VARIABLES
])

AC_DEFUN([OCAML_HOST_IS_EXECUTABLE], [
  AC_MSG_CHECKING([whether host executables can be run in the build])
  old_cross_compiling="$cross_compiling"
  cross_compiling='no'
  AC_RUN_IFELSE(
    [AC_LANG_SOURCE([[int main (void) {return 0;}]])],
    [AC_MSG_RESULT([yes])
    host_runnable=true],
    [AC_MSG_RESULT([no])
    host_runnable=false],
    # autoconf displays a warning if this parameter is missing, but
    # cross-compilation mode was disabled above.
    [assert=false])
  cross_compiling="$old_cross_compiling"
])

# This is AC_RUN_IFELSE but taking $host_runnable into account (i.e. if the
# program can be run, then it is run)
AC_DEFUN([OCAML_RUN_IFELSE], [
  old_cross_compiling="$cross_compiling"
  AS_IF([test "x$host_runnable" = 'xtrue'], [cross_compiling='no'])
  AC_RUN_IFELSE([$1],[$2],[$3],[$4])
  cross_compiling="$old_cross_compiling"
])

AC_DEFUN([OCAML_C99_CHECK_ROUND], [
  AC_MSG_CHECKING([whether round works])
  OCAML_RUN_IFELSE(
    [AC_LANG_SOURCE([[
#include <math.h>
int main (void) {
  static volatile double d = 0.49999999999999994449;
  return (fpclassify(round(d)) != FP_ZERO);
}
    ]])],
    [AC_MSG_RESULT([yes])
    AC_DEFINE([HAS_WORKING_ROUND])],
    [AC_MSG_RESULT([no])
    AS_CASE([$enable_imprecise_c99_float_ops,$target],
      [no,*], [hard_error=true],
      [yes,*], [hard_error=false],
      [*,x86_64-w64-mingw32], [hard_error=false],
      [hard_error=true])
    AS_IF([test x"$hard_error" = "xtrue"],
      [AC_MSG_ERROR(m4_normalize([
        round does not work, enable emulation with
        --enable-imprecise-c99-float-ops]))],
      [AC_MSG_WARN(m4_normalize([
        round does not work; emulation enabled]))])],
    [AS_CASE([$target],
      [x86_64-w64-mingw32],[AC_MSG_RESULT([cross-compiling; assume not])],
      [AC_MSG_RESULT([cross-compiling; assume yes])
      AC_DEFINE([HAS_WORKING_ROUND])])])
])

AC_DEFUN([OCAML_C99_CHECK_FMA], [
  AC_MSG_CHECKING([whether fma works])
  OCAML_RUN_IFELSE(
    [AC_LANG_SOURCE([[
#include <math.h>
int main (void) {
  /* Tests 264-266 from testsuite/tests/fma/fma.ml. These tests trigger the
     broken implementations of Cygwin64, mingw-w64 (x86_64) and VS2013-2017.
     The static volatile variables aim to thwart GCC's constant folding. */
  static volatile double x, y, z;
  volatile double t264, t265, t266;
  x = 0x3.bd5b7dde5fddap-496;
  y = 0x3.bd5b7dde5fddap-496;
  z = -0xd.fc352bc352bap-992;
  t264 = fma(x, y, z);
  x = 0x3.bd5b7dde5fddap-504;
  y = 0x3.bd5b7dde5fddap-504;
  z = -0xd.fc352bc352bap-1008;
  t265 = fma(x, y, z);
  x = 0x8p-540;
  y = 0x4p-540;
  z = 0x4p-1076;
  t266 = fma(x, y, z);
  return (!(t264 == 0x1.0989687cp-1044 ||
            t264 == 0x0.000004277ca1fp-1022 || /* Acceptable emulated values */
            t264 == 0x0.00000428p-1022)
       || !(t265 == 0x1.0988p-1060 ||
            t265 == 0x0.0000000004278p-1022 ||  /* Acceptable emulated values */
            t265 == 0x0.000000000428p-1022)
       || !(t266 == 0x8p-1076));
}
    ]])],
    [AC_MSG_RESULT([yes])
    AC_DEFINE([HAS_WORKING_FMA])],
    [AC_MSG_RESULT([no])
    AS_CASE([$enable_imprecise_c99_float_ops,$target],
      [no,*], [hard_error=true],
      [yes,*], [hard_error=false],
      [*,x86_64-w64-mingw32|*,x86_64-*-cygwin*], [hard_error=false],
      [AS_CASE([$ocaml_cv_cc_vendor],
        [msvc-*], [AS_IF([test "${ocaml_cv_cc_vendor#msvc-}" -lt 1920 ],
          [hard_error=false],
          [hard_error=true])],
        [hard_error=true])])
    AS_IF([test x"$hard_error" = "xtrue"],
      [AC_MSG_ERROR(m4_normalize([
        fma does not work, enable emulation with
        --enable-imprecise-c99-float-ops]))],
      [AC_MSG_WARN(m4_normalize([
        fma does not work; emulation enabled]))])],
    [AS_CASE([$target],
      [x86_64-w64-mingw32|x86_64-*-cygwin*],
        [AC_MSG_RESULT([cross-compiling; assume not])],
      [AC_MSG_RESULT([cross-compiling; assume yes])
      AC_DEFINE([HAS_WORKING_FMA])])])
])

# Computes a suitable id to insert in quoted strings to ensure that all OCaml
# quoted strings generated by configure cannot be "escaped". The ID takes the
# form {o*|string|o*}.
AC_DEFUN([OCAML_QUOTED_STRING_ID], [
  # Go through ac_subst_vars and put all the values to be substituted in
  # $all_values.
  QS=''
  all_values=''
  for name in $ac_subst_vars; do
    eval "value=\$${name}"
    all_values="$all_values $value"
  done

  # Keep adding 'o' to $QS until |$QS} doesn't appear in any substitution.
  test_string=''
  while test "x${test_string}" != "x${all_values}"; do
    test_string="$all_values"
    eval "test_string=\"\${test_string##*\|${QS}\}}\""
    if test "x${test_string}" != "x${all_values}"; then
      QS="o${QS}"
    fi
  done
])

AC_DEFUN([OCAML_CC_SUPPORTS_ATOMIC], [
  AC_MSG_CHECKING([whether the C compiler supports _Atomic types])
  saved_LIBS="$LIBS"
  LIBS="$LIBS $1"
  AC_LINK_IFELSE([AC_LANG_SOURCE([[
    #include <stdint.h>
    #include <stdatomic.h>
    int main(void)
    {
      _Atomic int64_t n;
      int m;
      int * _Atomic p = &m;
      atomic_store_explicit(&n, 123, memory_order_release);
      * atomic_exchange(&p, 0) = 45;
      return atomic_load_explicit(&n, memory_order_acquire);
    }
    ]])],
  [cc_supports_atomic=true
   AC_MSG_RESULT([yes])],
  [cc_supports_atomic=false
   AC_MSG_RESULT([no])])
  LIBS="$saved_LIBS"
])
