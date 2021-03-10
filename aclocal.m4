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
      [ocaml_cv_cc_vendor=`grep ['^[a-z]'] conftest.i | tr -s ' ' '-'`])],
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

# Save C compiler related variables
AC_DEFUN([OCAML_CC_SAVE_VARIABLES], [
  saved_CC="$CC"
  saved_CFLAGS="$CFLAGS"
  saved_CPPFLAGS="$CPPFLAGS"
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
