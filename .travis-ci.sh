#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#              Anil Madhavapeddy, University of Cambridge               #
#                                                                       #
#   Copyright 2014 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# Temporarily place to install OCaml into
PREFIX=`pwd`/ocaml-inst

# Configure Git with sensible defaults
git config --global user.email "some@name.com"
git config --global user.name "Some Name"

case $MODE in
# Do a build, run the test suite and confirm that OPAM works
gcc)
  ./configure -prefix ${PREFIX} ${CONFIGURE_FLAGS}
  make -j2 world.opt
  make install
  export PATH=$PATH:${PREFIX}/bin
  cd testsuite && make all
  mkdir external-packages
  cd external-packages
  git clone git://github.com/ocaml/camlp4
  cd camlp4 && ./configure --bindir=${PREFIX} && make && make install
  cd .. 
  git clone -b 1.2 git://github.com/ocaml/opam
  cd opam && ./configure --prefix=${PREFIX} && make lib-ext && make && make install
  opam init -y -a git://github.com/ocaml/opam-repository
  opam install -y oasis
  ;;
# Perform a clang static analysis and publish the results
clang-scan)
  eval `ssh-agent -s`
  ssh-add .deploy_key
  git clone git@github.com:${TRAVIS_REPO_SLUG} .gh-pages
  git -C .gh-pages checkout --orphan gh-pages
  git -C .gh-pages reset
  git -C .gh-pages clean -dxf
  scan-build-3.6 -o .gh-pages ./configure -prefix ${PREFIX}
  scan-build-3.6 -o .gh-pages make -j2 world.opt
  git -C .gh-pages add .
  git -C .gh-pages commit -m "Update Pages"
  git -C .gh-pages push origin gh-pages -f
  ;;
*)
  echo unknown mode
  exit 1
  ;;
esac
