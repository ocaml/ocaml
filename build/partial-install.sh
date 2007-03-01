#!/bin/sh

######################################
######### Copied from build/install.sh
######################################

set -e

cd `dirname $0`/..

. config/config.sh

not_installed=$PWD/_build/not_installed

rm -f "$not_installed"

wontinstall() {
  echo "$1" >> "$not_installed"
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
    ranlib "$dest"
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
mkdir -p $STUBLIBDIR
mkdir -p $MANDIR/man1
mkdir -p $MANDIR/man3
mkdir -p $MANDIR/man$MANEXT

cd _build

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
    $dir/*.p.$O  \
    $CAMLP4DIR/$dir
done
installdir \
  camlp4lib.{cma,cmxa} Camlp4.cmi \
  {camlp4o,camlp4of,camlp4oof,camlp4orf,camlp4r,camlp4rf}.cma \
  Camlp4Bin.{cm[iox],$O,p.$O} Camlp4Top.cm[io] \
  $CAMLP4DIR
installlibdir camlp4lib.{$A,p.$A} $CAMLP4DIR
cd ..

echo "Installing ocamlbuild..."

installbin ocamlbuild/ocamlbuild.byte$EXE $BINDIR/ocamlbuild.byte$EXE
installbin ocamlbuild/ocamlbuild.native$EXE $BINDIR/ocamlbuild.native$EXE
installbestbin ocamlbuild/ocamlbuild.native$EXE ocamlbuild/ocamlbuild.byte$EXE $BINDIR/ocamlbuild$EXE

installlibdir \
  ocamlbuild/ocamlbuildlib.{$A,p.$A} \
  $LIBDIR/ocamlbuild

installdir \
  ocamlbuild/ocamlbuildlib{,.p}.cmxa \
  ocamlbuild/ocamlbuildlib.cma \
  ocamlbuild/ocamlbuild_plugin.cmi \
  ocamlbuild/ocamlbuild_pack.cmi \
  ocamlbuild/ocamlbuild.cmo \
  ocamlbuild/ocamlbuild{,.p}.{cmx,$O} \
  $LIBDIR/ocamlbuild

installdir \
  ../ocamlbuild/man/ocamlbuild.1 \
  $MANDIR/man1
