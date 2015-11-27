#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#              Anil Madhavapeddy, OCaml Labs                            #
#                                                                       #
#   Copyright 2014 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

PREFIX=~/local

BuildAndTest () {
  case $XARCH in
  i386)
    mkdir -p $PREFIX
    ./configure --prefix $PREFIX
    export PATH=$PREFIX/bin:$PATH
    make world.opt
    make install
    (cd testsuite && make all)
    mkdir external-packages
    cd external-packages
    git clone git://github.com/ocaml/camlp4
    (cd camlp4 &&
     ./configure --bindir=$PREFIX/bin --libdir=$PREFIX/lib/ocaml \
       --pkgdir=$PREFIX/lib/ocaml && \
      make && make install)
    git clone git://github.com/ocaml/opam
    (cd opam && ./configure --prefix $PREFIX &&\
      make lib-ext && make && make install)
    git config --global user.email "some@name.com"
    git config --global user.name "Some Name"
    opam init -y -a git://github.com/ocaml/opam-repository
    opam install -y oasis
    # opam pin add -y utop git://github.com/diml/utop
    ;;
  *)
    echo unknown arch
    exit 1
    ;;
  esac
}

CheckChangesModified () {
  # check that Changes has been modified by the proposed change
  git diff $TRAVIS_COMMIT_RANGE --name-only --exit-code Changes > /dev/null \
  && exit 1 || echo pass
}

CheckTestsuiteModified () {
  # check that testsuite/ has been modified by the proposed change
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
