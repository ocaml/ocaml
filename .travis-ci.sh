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

PREFIX=~/local

BuildAndTest () {
  case $XARCH in
  i386)
  echo<<EOF
------------------------------------------------------------------------
This test builds the OCaml compiler distribution with your pull request,
runs its testsuite, and then tries to install some important OCaml softare
(currently camlp4) on top of it.

Failing to build the compiler distribution, or testsuite failures are
critical errors that must be understood and fixed before your pull
request can be merged. The later installation attempts try to run
bleeding-edge software, and failures can sometimes be out of your
control.
------------------------------------------------------------------------
EOF
    mkdir -p $PREFIX
    ./configure --prefix $PREFIX -with-debug-runtime \
      -with-instrumented-runtime $CONFIG_ARG
    export PATH=$PREFIX/bin:$PATH
    make world.opt
    make ocamlnat
    make install
    (cd testsuite && make all)
    (cd testsuite && make USE_RUNTIME="d" all)
    mkdir external-packages
    cd external-packages
    git clone git://github.com/ocaml/ocamlbuild
    mkdir ocamlbuild-install
    (cd ocamlbuild &&
        make -f configure.make Makefile.config src/ocamlbuild_config.ml \
          OCAMLBUILD_PREFIX=$PREFIX \
          OCAMLBUILD_BINDIR=$PREFIX/bin \
          OCAMLBUILD_LIBDIR=$PREFIX/lib \
          OCAML_NATIVE=true \
          OCAML_NATIVE_TOOLS=true &&
        make all &&
        make install)
    git clone git://github.com/ocaml/camlp4
    (cd camlp4 &&
     ./configure --bindir=$PREFIX/bin --libdir=$PREFIX/lib/ocaml \
       --pkgdir=$PREFIX/lib/ocaml && \
      make && make install)
    # git clone git://github.com/ocaml/opam
    # (cd opam && ./configure --prefix $PREFIX &&\
    #   make lib-ext && make && make install)
    # git config --global user.email "some@name.com"
    # git config --global user.name "Some Name"
    # opam init -y -a git://github.com/ocaml/opam-repository
    # opam install -y oasis
    # opam pin add -y utop git://github.com/diml/utop
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac
}

CheckChangesModified () {
  echo<<EOF
------------------------------------------------------------------------
This test checks that the Changes file has been modified by the pull
request. Most contributions should come with a message in the Changes
file, as described in our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#changelog

Some very minor changes (typo fixes for example) may not need
a Changes entry, in which case it is acceptable for this test to fail.
------------------------------------------------------------------------
EOF
  # check that Changes has been modified
  git diff $TRAVIS_COMMIT_RANGE --name-only --exit-code Changes > /dev/null \
  && exit 1 || echo pass
}

CheckTestsuiteModified () {
  echo<<EOF
------------------------------------------------------------------------
This test checks that the OCaml testsuite has been modified by the
pull request. Any new feature should come with tests, bugs should come
with regression tests, and generally any change in behavior that can
be exercized by a test should come with a test or modify and existing
test. See our contributor documentation:

  https://github.com/ocaml/ocaml/blob/trunk/CONTRIBUTING.md#test-you-must

Modifications that result in no change in observable behavior
(documentation contributions for example) can hardly be tested, in
which case it is acceptable for this test to fail.

Note: the heuristic used by this test is extremely fragile; passing it
does *not* imply that your change is appropriately tested.
------------------------------------------------------------------------
EOF
  # check that at least a file in testsuite/ has been modified
  git diff $TRAVIS_COMMIT_RANGE --name-only --exit-code testsuite > /dev/null \
  && exit 1 || echo pass
}

case $CI_KIND in
build) BuildAndTest;;
changes) CheckChangesModified;;
tests) CheckTestsuiteModified;;
*) echo unknown CI kind
   exit 1
   ;;
esac
