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

MAKE_WARN="$MAKE --warn-undefined-variables"

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
    --enable-flambda-invariants \
    --enable-ocamltest \
    --disable-dependency-generation \
    $CONFIG_ARG"

  local failed
  ./configure $configure_flags || failed=$?
  if ((failed)) ; then cat config.log ; exit $failed ; fi
}

Build () {
  local failed
  export TERM=ansi
  if [ "$(uname)" = 'Darwin' ]; then
    script -q build.log $MAKE_WARN || failed=$?
    if ((failed)); then
      script -q build.log $MAKE_WARN make -j1 V=1
      exit $failed
    fi
  else
    script --return --command "$MAKE_WARN" build.log || failed=$?
    if ((failed)); then
      script --return --command "$MAKE_WARN -j1 V=1" build.log
      exit $failed
    fi
  fi
  if grep -Fq ' warning: undefined variable ' build.log; then
    echo Undefined Makefile variables detected:
    grep -F ' warning: undefined variable ' build.log
    failed=1
  fi
  rm build.log
  echo Ensuring that all names are prefixed in the runtime
  if ! ./tools/check-symbol-names runtime/*.a otherlibs/*/lib*.a ; then
    failed=1
  fi
  if ((failed)); then
    exit $failed
  fi
}

Test () {
  if [ "$1" = "sequential" ]; then
    echo Running the testsuite sequentially
    $MAKE -C testsuite all
    cd ..
  elif [ "$1" = "parallel" ]; then
    echo Running the testsuite in parallel
    $MAKE -C testsuite parallel
    cd ..
  else
    echo "Error: unexpected argument '$1' to function Test(). " \
         "It should be 'sequential' or 'parallel'."
    exit 1
  fi
}

# By default, TestPrefix will attempt to run the tests
# in the given directory in parallel.
TestPrefix () {
  TO_RUN=parallel-"$1"
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
  local failed
  ./configure || failed=$?
  if ((failed)) ; then cat config.log ; exit $failed ; fi
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
  echo "build-status=$STATUS" >>"$GITHUB_OUTPUT"
  exit $CODE
}

BasicCompiler () {
  local failed
  trap ReportBuildStatus ERR

  local failed
  ./configure --disable-dependency-generation \
              --disable-debug-runtime \
              --disable-instrumented-runtime \
              --enable-ocamltest \
      || failed=$?
  if ((failed)) ; then cat config.log ; exit $failed ; fi

  # Need a runtime
  make -j coldstart || failed=$?
  if ((failed)) ; then
    make -j1 V=1 coldstart
    exit $failed
  fi
  # And generated files (ocamllex compiles ocamlyacc)
  make -j ocamllex || failed=$?
  if ((failed)) ; then
    make -j1 V=1 ocamllex
    exit $failed
  fi

  ReportBuildStatus 0
}

case $1 in
configure) Configure;;
build) Build;;
test) Test parallel;;
test_sequential) Test sequential;;
test_prefix) TestPrefix $2;;
api-docs) API_Docs;;
install) Install;;
manual) BuildManual;;
other-checks) Checks;;
basic-compiler) BasicCompiler;;
*) echo "Unknown CI instruction: $1"
   exit 1;;
esac
