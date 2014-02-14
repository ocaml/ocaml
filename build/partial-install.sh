#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

######################################
######### Copied from build/install.sh
######################################

set -e

cd `dirname $0`/..

# Save the following environment variables before sourcing config.sh since it
# will overwrite them and the user might have set them to emulate $(DESTDIR)
# which is unfortunately not supported.
SAVED_BINDIR="${BINDIR}"
SAVED_LIBDIR="${LIBDIR}"
SAVED_MANDIR="${MANDIR}"

. config/config.sh

BINDIR="${SAVED_BINDIR:-${BINDIR}}"
LIBDIR="${SAVED_LIBDIR:-${LIBDIR}}"
MANDIR="${SAVED_MANDIR:-${MANDIR}}"

not_installed=$PWD/_build/not_installed

rm -f "$not_installed"
mkdir -p "$PWD/_build"
touch "$not_installed"

wontinstall() {
  echo "$1" >> "$not_installed"
  echo "  don't install $1"
}

installbin() {
  if [ -f "$1" ]; then
    echo "  install binary $2"
    cp -f "$1" "$2"
    [ -x "$2" ] || chmod +x "$2"
  else
    wontinstall "$1"
  fi
}

installbestbin() {
  if [ -f "$1" ]; then
    echo "  install binary $3 (with `basename $1`)"
    cp -f "$1" "$3"
  else
    if [ -f "$2" ]; then
      echo "  install binary $3 (with `basename $2`)"
      cp -f "$2" "$3"
    else
      echo "None of $1, $2 exists"
      exit 3
    fi
  fi
  [ -x "$3" ] || chmod +x "$3"
}

installlib() {
  if [ -f "$1" ]; then
    dest="$2/`basename $1`"
    echo "  install library $dest"
    cp -f "$1" "$2"
    if [ "$RANLIB" != "" ]; then
      "$RANLIB" "$dest"
    fi
  else
    wontinstall "$1"
  fi
}

installdir() {
  args=""
  while [ $# -gt 1 ]; do
    if [ -f "$1" ]; then
      args="$args $1"
    else
      wontinstall "$1"
    fi
    shift
  done
  last="$1"
  for file in $args; do
    echo "  install $last/`basename $file`"
    cp -f "$file" "$last"
  done
}

installlibdir() {
  args=""
  while [ $# -gt 1 ]; do
    args="$args $1"
    shift
  done
  last="$1"
  for file in $args; do
    installlib "$file" "$last"
  done
}

mkdir -p $BINDIR
mkdir -p $LIBDIR
mkdir -p $LIBDIR/camlp4
mkdir -p $LIBDIR/ocamlbuild
mkdir -p $MANDIR/man1
mkdir -p $MANDIR/man3

cd _build

if [ -n "${WITH_CAMLP4}" ]; then
  echo "Installing camlp4..."
  installbin camlp4/camlp4prof.byte$EXE $BINDIR/camlp4prof$EXE
  installbin camlp4/mkcamlp4.byte$EXE $BINDIR/mkcamlp4$EXE
  installbin camlp4/camlp4.byte$EXE $BINDIR/camlp4$EXE
  installbin camlp4/camlp4boot.byte$EXE $BINDIR/camlp4boot$EXE
  installbin camlp4/camlp4o.byte$EXE $BINDIR/camlp4o$EXE
  installbin camlp4/camlp4of.byte$EXE $BINDIR/camlp4of$EXE
  installbin camlp4/camlp4oof.byte$EXE $BINDIR/camlp4oof$EXE
  installbin camlp4/camlp4orf.byte$EXE $BINDIR/camlp4orf$EXE
  installbin camlp4/camlp4r.byte$EXE $BINDIR/camlp4r$EXE
  installbin camlp4/camlp4rf.byte$EXE $BINDIR/camlp4rf$EXE
  installbin camlp4/camlp4o.native$EXE $BINDIR/camlp4o.opt$EXE
  installbin camlp4/camlp4of.native$EXE $BINDIR/camlp4of.opt$EXE
  installbin camlp4/camlp4oof.native$EXE $BINDIR/camlp4oof.opt$EXE
  installbin camlp4/camlp4orf.native$EXE $BINDIR/camlp4orf.opt$EXE
  installbin camlp4/camlp4r.native$EXE $BINDIR/camlp4r.opt$EXE
  installbin camlp4/camlp4rf.native$EXE $BINDIR/camlp4rf.opt$EXE

  cd camlp4
  CAMLP4DIR=$LIBDIR/camlp4
  for dir in Camlp4Parsers Camlp4Printers Camlp4Filters Camlp4Top; do
    echo "Installing $dir..."
    mkdir -p $CAMLP4DIR/$dir
    installdir     \
      $dir/*.cm*   \
      $dir/*.$O    \
      $CAMLP4DIR/$dir
  done
  installdir \
    camlp4lib.cma camlp4lib.cmxa Camlp4.cmi \
    camlp4fulllib.cma camlp4fulllib.cmxa \
    camlp4o.cma camlp4of.cma camlp4oof.cma \
    camlp4orf.cma camlp4r.cma camlp4rf.cma \
    Camlp4Bin.cm[iox] Camlp4Bin.$O Camlp4Top.cm[io] \
    Camlp4_config.cmi camlp4prof.cm[iox] camlp4prof.$O Camlp4_import.cmi \
    $CAMLP4DIR
  installlibdir camlp4lib.$A camlp4fulllib.$A $CAMLP4DIR
  cd ..
fi

if [ -n "${WITH_OCAMLBUILD}" ]; then
  echo "Installing ocamlbuild..."
  cd ocamlbuild
  installbin ocamlbuild.byte$EXE $BINDIR/ocamlbuild.byte$EXE
  installbin ocamlbuild.native$EXE $BINDIR/ocamlbuild.native$EXE
  installbestbin ocamlbuild.native$EXE ocamlbuild.byte$EXE \
    $BINDIR/ocamlbuild$EXE

  installlibdir \
    ocamlbuildlib.$A \
    $LIBDIR/ocamlbuild

  installdir \
    ocamlbuildlib.cmxa \
    ocamlbuildlib.cma \
    ocamlbuild_plugin.cmi \
    ocamlbuild_plugin.cmo \
    ocamlbuild_plugin.cmx \
    ocamlbuild_pack.cmi \
    ocamlbuild_unix_plugin.cmi \
    ocamlbuild_unix_plugin.cmo \
    ocamlbuild_unix_plugin.cmx \
    ocamlbuild_unix_plugin.$O \
    ocamlbuild_executor.cmi \
    ocamlbuild_executor.cmo \
    ocamlbuild_executor.cmx \
    ocamlbuild_executor.$O \
    ocamlbuild.cmo \
    ocamlbuild.cmx \
    ocamlbuild.$O \
    $LIBDIR/ocamlbuild
  cd ..

  installdir \
    ../ocamlbuild/man/ocamlbuild.1 \
    $MANDIR/man1
fi
