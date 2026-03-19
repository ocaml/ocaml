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

# The prefix is designed to be usable as an opam local switch
PREFIX=~/local/_opam

MAKE="make $MAKE_ARG"
SHELL=dash

MAKE_WARN="$MAKE --warn-undefined-variables"

export PATH=$PREFIX/bin:$PATH

call-configure () {
  local failed
  ./configure "$@" || failed=$?
  if ((failed)); then
    # Output seems to be a little unpredictable in GitHub Actions: ensure that
    # the fold is definitely on a new line
    echo
    echo "::group::config.log content ($(wc -l config.log) lines)"
    cat config.log
    echo '::endgroup::'
    exit $failed
  fi
}

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

  # $CONFIG_ARG will be intentionally word-split - there is no way to pass
  # arguments requiring spaces from the workflows.
  # $CONFIG_ARG also appears last to allow settings specified here to be
  # overridden by the workflows.
  call-configure --prefix="$PREFIX" \
                 --docdir="$PREFIX/doc/ocaml" \
                 --enable-flambda-invariants \
                 --enable-ocamltest \
                 --enable-native-toplevel \
                 -C $CONFIG_ARG
}

Build () {
  local failed
  export TERM=ansi
  if [ "$(uname)" = 'Darwin' ]; then
    script -q build.log $MAKE_WARN || failed=$?
    if ((failed)); then
      script -q build.log $MAKE_WARN -j1 V=1
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
  $MAKE INSTALL_MODE=list install | grep '^->' | sort | uniq -d > duplicates
  if [ -s duplicates ]; then
    echo "The installation duplicates targets:"
    cat duplicates
    exit 1
  fi
  rm duplicates
  $MAKE DESTDIR="$PWD/install" install
  find $PWD/install -name _opam -type d
  $MAKE INSTALL_MODE=clone install
  ret="$PWD"
  script="$PWD/ocaml-compiler-clone.sh"
  cd "$(find $PWD/install -name _opam -type d)"
  mkdir -p "share/ocaml"
  cp "$ret/config.status" "$ret/config.cache" "share/ocaml"
  cp "$ret/ocaml-compiler-clone.sh" "share/ocaml/clone"
  sh $script ~/local/_opam
  cd "$ret"
  rm -rf install
  rm ocaml-compiler-clone.sh
}

target_libdir_is_relative='^ *TARGET_LIBDIR_IS_RELATIVE *= *false'

Test-In-Prefix () {
  { set +x
    echo 'Checking that compilers invoked with alternate runtimes use their'
    echo "configured location, not the alternate runtime's"
    expected1="$(realpath "$PREFIX/lib/ocaml")"
  } 2>/dev/null
  if [[ ! -d "$PREFIX.new" ]]; then
    # In Re-Test-In-Prefix, $PREFIX is the original compiler built by the
    # workflow and then $PREFIX.new is the "alternate configuration". The first
    # time round, we clone whichever compiler has just been built for this test.
    cp -a "$PREFIX" "$PREFIX.new"
    remove="$PREFIX.new"
    if grep -q "$target_libdir_is_relative" Makefile.build_config; then
      # Compiler configured absolutely - both should return the same answer
      expected2="$expected1"
    else
      # Compiler configured relatively
      expected2="$(realpath "$PREFIX").new/lib/ocaml"
    fi
  else
    # The alternate configuration path should be returned, regardless of whether
    # the runtime invoking it is an absolute or a relative one from another
    # location.
    expected2="$(realpath "$PREFIX").new/lib/ocaml-lib"
    remove=''
  fi
  { set +x
    lib1="$($PREFIX.new/bin/ocamlrun $PREFIX/bin/ocamlc.byte -where)"
    lib2="$($PREFIX/bin/ocamlrun $PREFIX.new/bin/ocamlc.byte -where)"
    echo "$PREFIX/bin/ocamlc.byte OSLD: $($PREFIX/bin/ocamlrun \
      $PREFIX/bin/ocamlobjinfo.byte $PREFIX/bin/ocamlc.byte \
        | sed -ne 's/^caml_standard_library_default: //p')"
    echo -n "$PREFIX.new/bin/ocamlrun standard_library_default: "
    $PREFIX.new/bin/ocamlrun -config | sed -ne 's/standard_library_default: //p'
    echo "$PREFIX.new/bin/ocamlrun $PREFIX/bin/ocamlc.byte -where: $lib1"
    if [[ $lib1 != $expected1 ]]; then
      echo -e '  \e[31mEXPECTED\e[0m:' "$expected1"
    fi
    echo
    echo "$PREFIX.new/bin/ocamlc.byte OSLD: $($PREFIX.new/bin/ocamlrun \
      $PREFIX.new/bin/ocamlobjinfo.byte $PREFIX.new/bin/ocamlc.byte \
        | sed -ne 's/^caml_standard_library_default: //p')"
    echo -n "$PREFIX/bin/ocamlrun standard_library_default: "
    $PREFIX/bin/ocamlrun -config | sed -ne 's/standard_library_default: //p'
    echo "$PREFIX/bin/ocamlrun $PREFIX.new/bin/ocamlc.byte -where: $lib2"
    if [[ $lib2 != $expected2 ]]; then
      echo -e '  \e[31mEXPECTED\e[0m:' "$expected2"
    fi
    [[ $lib1 = $expected1 && $lib2 = $expected2 ]] && echo 'Correct.' || exit 1
  } 2>/dev/null
  [[ -z $remove ]] || rm -rf "$remove"
  $MAKE -C testsuite/in_prefix -f Makefile.test test-in-prefix
}

Re-Test-In-Prefix () {
  mkdir -p bak
  mv Makefile.config Makefile.build_config config.status bak
  git clean -dfX &>/dev/null
  mv bak/Makefile.config bak/Makefile.build_config bak/config.status .
  rmdir bak
  # The libdir is configured to be $PREFIX.new/lib/ocaml-lib in order to
  # "poison" the cross-runtime test (otherwise if $PREFIX/bin/ocamlc.byte is
  # missing OSLD, then $PREFIX.new/bin/ocamlrun would still supply the correct
  # ../lib/ocaml. This way, it supplies ../lib/ocaml-lib and the test correctly
  # fails)
  if grep -q "$target_libdir_is_relative" Makefile.build_config; then
    # Compiler configured absolutely - reconfigure relatively
    echo '::group::Re-building the compiler with a relative libdir'
    $MAKE COMPUTE_DEPS=false reconfigure \
          'ADDITIONAL_CONFIGURE_ARGS=--with-relative-libdir=../lib/ocaml-lib \
--enable-runtime-search --enable-runtime-search-target=fallback \
--prefix='"$PREFIX"'.new'
  else
    # Compiler configured relatively - reconfigure absolutely
    echo '::group::Re-building the compiler with an absolute libdir'
    $MAKE COMPUTE_DEPS=false reconfigure \
          'ADDITIONAL_CONFIGURE_ARGS=--without-relative-libdir \
--disable-runtime-search --disable-runtime-search-target \
--prefix='"$PREFIX"'.new --libdir='"$PREFIX"'.new/lib/ocaml-lib'
  fi
  $MAKE
  $MAKE install
  echo '::endgroup::'
  Test-In-Prefix
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
  # Ensure that .gitignore is up-to-date - this will fail if any untracked or
  # altered files exist.
  test -z "$(git status --porcelain)"
  # check that the 'clean' target also works
  $MAKE clean
  $MAKE -C manual clean
  $MAKE -C manual distclean
  # check that the `distclean` target definitely cleans the tree
  $MAKE distclean
  # Check the working tree is clean - config.cache is intentionally not deleted
  # by any of the clean targets
  rm config.cache
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
  call-configure
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

  call-configure --disable-debug-runtime \
                 --disable-instrumented-runtime \
                 --enable-ocamltest \

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

CreateSwitch () {
  # This can be switched to use the Ubuntu package when Ubuntu 26.04 is deployed
  # (opam 2.1.5 in Ubuntu 24.04 is too old)
  curl -Lo opam \
 'https://github.com/ocaml/opam/releases/download/2.4.1/opam-2.4.1-x86_64-linux'
  chmod +x opam
  ./opam init --cli=2.4 --bare --disable-sandboxing --yes --auto-setup
  # This is intentionally done before the switch is created - if the install
  # target creates _opam then the switch creation will fail.
  $MAKE INSTALL_MODE=opam OPAM_PACKAGE_NAME=ocaml-variants install
  # These commands intentionally run using opam's "default" CLI
  ./opam switch create ~/local --empty
  ./opam switch --switch ~/local set-invariant --no-action ocaml-option-flambda
  ./opam pin add --switch ~/local --no-action --kind=path ocaml-variants .
  ./opam install --switch ~/local --yes --assume-built ocaml-variants
  ./opam exec --switch ~/local -- ocamlopt -v
}

case $1 in
configure) Configure;;
build) Build;;
test) Test parallel;;
test_sequential) Test sequential;;
test_prefix) TestPrefix $2;;
api-docs) API_Docs;;
install) Install;;
test-in-prefix) Test-In-Prefix;;
re-test-in-prefix) Re-Test-In-Prefix;;
manual) BuildManual;;
other-checks) Checks;;
basic-compiler) BasicCompiler;;
opam) CreateSwitch;;
*) echo "Unknown CI instruction: $1"
   exit 1;;
esac
