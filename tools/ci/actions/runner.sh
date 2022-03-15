#!/usr/bin/env bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*              Anil Madhavapeddy, OCaml Labs                             *
#*                                                                        *
#*   Copyright 2014 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

set -xe

PREFIX=~/local

MAKE="make $MAKE_ARG"
SHELL=dash
TEST_SEQUENTIALLY=$TEST_SEQUENTIALLY

export PATH=$PREFIX/bin:$PATH

Configure () {
  mkdir -p $PREFIX
  cat<<EOF
------------------------------------------------------------------------
This test builds the OCaml compiler distribution with your pull request
and runs its testsuite.
Failing to build the compiler distribution, or testsuite failures are
critical errors that must be understood and fixed before your pull
request can be merged.
------------------------------------------------------------------------
EOF

  configure_flags="\
    --prefix=$PREFIX \
    --enable-debug-runtime \
    $CONFIG_ARG"

  case $XARCH in
  x64)
    ./configure $configure_flags
    ;;
  i386)
    ./configure --build=x86_64-pc-linux-gnu --host=i386-linux \
      CC='gcc -m32 -march=x86-64' \
      AS='as --32' \
      ASPP='gcc -m32 -march=x86-64 -c' \
      PARTIALLD='ld -r -melf_i386' \
      $configure_flags
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac
}

Build () {
  $MAKE world.opt
  echo Ensuring that all names are prefixed in the runtime
  ./tools/check-symbol-names runtime/*.a otherlibs/*/lib*.a
}

Test () {
  echo Running the testsuite
  $MAKE -C testsuite parallel
  cd ..
}

# By default, TestPrefix will attempt to run the tests
# in the given directory in parallel.
# Setting $TEST_SEQUENTIALLY will avoid this behaviour.
TestPrefix () {
  if [[ -z "${TEST_SEQUENTIALLY}" ]]; then
    TO_RUN=parallel-"$1"
  else
    TO_RUN="one DIR=tests/$1"
  fi
  echo Running single testsuite directory with $TO_RUN

  $MAKE -C testsuite $TO_RUN
  cd ..
}

API_Docs () {
  echo Ensuring that all library documentation compiles
  $MAKE -C api_docgen html pdf texi
}

Install () {
  $MAKE install
}

Checks () {
  if fgrep 'SUPPORTS_SHARED_LIBRARIES=true' Makefile.config &>/dev/null ; then
    echo Check the code examples in the manual
    $MAKE manual-pregen
  fi
  # check_all_arches checks tries to compile all backends in place,
  # we would need to redo (small parts of) world.opt afterwards to
  # use the compiler again
  $MAKE check_all_arches
  # Ensure that .gitignore is up-to-date - this will fail if any untreacked or
  # altered files exist.
  test -z "$(git status --porcelain)"
  # check that the 'clean' target also works
  $MAKE clean
  $MAKE -C manual clean
  $MAKE -C manual distclean
  # check that the `distclean` target definitely cleans the tree
  $MAKE distclean
  # Check the working tree is clean
  test -z "$(git status --porcelain)"
  # Check that there are no ignored files
  test -z "$(git ls-files --others -i --exclude-standard)"
}

CheckManual () {
      cat<<EOF
--------------------------------------------------------------------------
This test checks the global structure of the reference manual
(e.g. missing chapters).
--------------------------------------------------------------------------
EOF
  # we need some of the configuration data provided by configure
  ./configure
  $MAKE check-stdlib check-case-collision -C manual/tests

}

BuildManual () {
  $MAKE -C manual/src/html_processing duniverse
  $MAKE -C manual manual
  $MAKE -C manual web
}

# ReportBuildStatus accepts an exit code as a parameter (defaults to 1) and also
# instructs GitHub Actions to set build-status to 'failed' on non-zero exit or
# 'success' otherwise.
ReportBuildStatus () {
  CODE=${1:-1}
  if ((CODE)); then
    STATUS='failed'
  else
    STATUS='success'
  fi
  echo "::set-output name=build-status::$STATUS"
  exit $CODE
}

BasicCompiler () {
  trap ReportBuildStatus ERR

  ./configure --disable-dependency-generation \
              --disable-debug-runtime \
              --disable-instrumented-runtime

  # Need a runtime
  make -j coldstart
  # And generated files (ocamllex compiles ocamlyacc)
  make -j ocamllex

  ReportBuildStatus 0
}

case $1 in
configure) Configure;;
build) Build;;
test) Test;;
test_prefix) TestPrefix $2;;
api-docs) API_Docs;;
install) Install;;
manual) BuildManual;;
other-checks) Checks;;
basic-compiler) BasicCompiler;;
*) echo "Unknown CI instruction: $1"
   exit 1;;
esac
