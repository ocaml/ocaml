#!/usr/bin/env bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                           Samuel Hym, Tarides                          *
#*                                                                        *
#*   Copyright 2024 Tarides                                               *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

set -e

OCAMLDIR=ocaml
DUNEDIR=dune
MULTICORETESTSDIR=multicoretests

export PREFIX="$HOME/local"
export PATH="$PREFIX/bin:$PATH"

fatal() {
  printf %s "$1"
  exit 1
}

build_ocaml() {
  # We let build.yml test for warnings
  cd "$OCAMLDIR"
  if ! ./configure --disable-warn-error --disable-stdlib-manpages \
      --disable-ocamltest --disable-ocamldoc --prefix="$PREFIX" ; then
    cat config.log
    exit 1
  fi

  make -j
  make install
}

build_dune() {
  cd "$DUNEDIR"
  make release
  make install PREFIX="$PREFIX"
}

show_config() {
  set -x
  ocamlc -config
  dune --version
}

build_testsuite() {
  cd "$MULTICORETESTSDIR"
  dune build
}

run_testsuite() {
  export QCHECK_MSG_INTERVAL=60
  cd "$MULTICORETESTSDIR"
  dune build @ci -j1 --no-buffer --display=quiet --cache=disabled \
    --error-reporting=twice
}

case "$1" in
  ocaml)
    build_ocaml
    ;;
  dune)
    build_dune
    ;;
  show_config)
    show_config
    ;;
  build)
    build_testsuite
    ;;
  testsuite)
    run_testsuite
    ;;
  *)
    fatal "Unknown command '$1'"
    ;;
esac
