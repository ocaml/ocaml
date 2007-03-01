#!/bin/sh
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
mkdir -p $LIBDIR/caml
mkdir -p $LIBDIR/camlp4
mkdir -p $LIBDIR/vmthreads
mkdir -p $LIBDIR/threads
mkdir -p $LIBDIR/labltk
mkdir -p $LIBDIR/ocamlbuild
mkdir -p $LIBDIR/ocamldoc
mkdir -p $STUBLIBDIR
mkdir -p $MANDIR/man1
mkdir -p $MANDIR/man3
mkdir -p $MANDIR/man$MANEXT

echo "Installing core libraries..."
installlibdir byterun/libcamlrun.{$A,p.$A} \
              asmrun/libasmrun.{$A,p.$A} \
              asmrun/libasmrunp.{$A,p.$A} \
              $LIBDIR

PUBLIC_INCLUDES="\
  alloc.h callback.h config.h custom.h fail.h intext.h \
  memory.h misc.h mlvalues.h printexc.h signals.h compatibility.h"

cd byterun
for i in $PUBLIC_INCLUDES; do
  echo "  install caml/$i"
  sed -f ../tools/cleanup-header $i > $LIBDIR/caml/$i
done
cd ..

installdir otherlibs/{win32,}unix/unixsupport.h $LIBDIR/caml

installdir byterun/ocamlrun.dll yacc/ocamlyacc byterun/ocamlrun $BINDIR

installdir byterun/ld.conf $LIBDIR

installbin win32caml/ocamlwin.exe $PREFIX/OCamlWin.exe

cd _build

echo "Installing the toplevel and compilers..."
installbin ocaml$EXE $BINDIR/ocaml$EXE
installbin ocamlc$EXE $BINDIR/ocamlc$EXE
installbin ocamlopt$EXE $BINDIR/ocamlopt$EXE
installbin ocamlc.opt$EXE $BINDIR/ocamlc.opt$EXE
installbin ocamlopt.opt$EXE $BINDIR/ocamlopt.opt$EXE

set=set # coloration workaround

echo "Installing the standard library..."
installdir \
  stdlib/stdlib.cma \
  stdlib/stdlib{,.p}.cmxa \
  stdlib/camlheader \
  stdlib/camlheader_ur \
  stdlib/std_exit.cm[io] \
  stdlib/arg.{cmi,mli} \
  stdlib/array.{cmi,mli} \
  stdlib/arrayLabels.{cmi,mli} \
  stdlib/buffer.{cmi,mli} \
  stdlib/callback.{cmi,mli} \
  stdlib/camlinternalMod.{cmi,mli} \
  stdlib/camlinternalOO.{cmi,mli} \
  stdlib/char.{cmi,mli} \
  stdlib/complex.{cmi,mli} \
  stdlib/digest.{cmi,mli} \
  stdlib/filename.{cmi,mli} \
  stdlib/format.{cmi,mli} \
  stdlib/gc.{cmi,mli} \
  stdlib/genlex.{cmi,mli} \
  stdlib/hashtbl.{cmi,mli} \
  stdlib/int32.{cmi,mli} \
  stdlib/int64.{cmi,mli} \
  stdlib/lazy.{cmi,mli} \
  stdlib/lexing.{cmi,mli} \
  stdlib/list.{cmi,mli} \
  stdlib/listLabels.{cmi,mli} \
  stdlib/map.{cmi,mli} \
  stdlib/marshal.{cmi,mli} \
  stdlib/moreLabels.{cmi,mli} \
  stdlib/nativeint.{cmi,mli} \
  stdlib/obj.{cmi,mli} \
  stdlib/oo.{cmi,mli} \
  stdlib/parsing.{cmi,mli} \
  stdlib/pervasives.{cmi,mli} \
  stdlib/printexc.{cmi,mli} \
  stdlib/printf.{cmi,mli} \
  stdlib/queue.{cmi,mli} \
  stdlib/random.{cmi,mli} \
  stdlib/scanf.{cmi,mli} \
  stdlib/sort.{cmi,mli} \
  stdlib/stack.{cmi,mli} \
  stdlib/stdLabels.{cmi,mli} \
  stdlib/stream.{cmi,mli} \
  stdlib/string.{cmi,mli} \
  stdlib/stringLabels.{cmi,mli} \
  stdlib/sys.{cmi,mli} \
  stdlib/weak.{cmi,mli} \
  stdlib/$set.{cmi,mli} \
  stdlib/arg{,.p}.{cmx,$O} \
  stdlib/array{,.p}.{cmx,$O} \
  stdlib/arrayLabels{,.p}.{cmx,$O} \
  stdlib/buffer{,.p}.{cmx,$O} \
  stdlib/callback{,.p}.{cmx,$O} \
  stdlib/camlinternalMod{,.p}.{cmx,$O} \
  stdlib/camlinternalOO{,.p}.{cmx,$O} \
  stdlib/char{,.p}.{cmx,$O} \
  stdlib/complex{,.p}.{cmx,$O} \
  stdlib/digest{,.p}.{cmx,$O} \
  stdlib/filename{,.p}.{cmx,$O} \
  stdlib/format{,.p}.{cmx,$O} \
  stdlib/gc{,.p}.{cmx,$O} \
  stdlib/genlex{,.p}.{cmx,$O} \
  stdlib/hashtbl{,.p}.{cmx,$O} \
  stdlib/int32{,.p}.{cmx,$O} \
  stdlib/int64{,.p}.{cmx,$O} \
  stdlib/lazy{,.p}.{cmx,$O} \
  stdlib/lexing{,.p}.{cmx,$O} \
  stdlib/list{,.p}.{cmx,$O} \
  stdlib/listLabels{,.p}.{cmx,$O} \
  stdlib/map{,.p}.{cmx,$O} \
  stdlib/marshal{,.p}.{cmx,$O} \
  stdlib/moreLabels{,.p}.{cmx,$O} \
  stdlib/nativeint{,.p}.{cmx,$O} \
  stdlib/obj{,.p}.{cmx,$O} \
  stdlib/oo{,.p}.{cmx,$O} \
  stdlib/parsing{,.p}.{cmx,$O} \
  stdlib/pervasives{,.p}.{cmx,$O} \
  stdlib/printexc{,.p}.{cmx,$O} \
  stdlib/printf{,.p}.{cmx,$O} \
  stdlib/queue{,.p}.{cmx,$O} \
  stdlib/random{,.p}.{cmx,$O} \
  stdlib/scanf{,.p}.{cmx,$O} \
  stdlib/sort{,.p}.{cmx,$O} \
  stdlib/stack{,.p}.{cmx,$O} \
  stdlib/stdLabels{,.p}.{cmx,$O} \
  stdlib/std_exit{,.p}.{cmx,$O} \
  stdlib/stream{,.p}.{cmx,$O} \
  stdlib/string{,.p}.{cmx,$O} \
  stdlib/stringLabels{,.p}.{cmx,$O} \
  stdlib/sys{,.p}.{cmx,$O} \
  stdlib/weak{,.p}.{cmx,$O} \
  stdlib/$set{,.p}.{cmx,$O} \
  $LIBDIR

installlibdir \
  stdlib/stdlib.{$A,p.$A} \
  $LIBDIR

echo "Installing ocamllex, ocamldebug..."
installbin lex/ocamllex$EXE $BINDIR/ocamllex$EXE
installbin debugger/ocamldebug$EXE $BINDIR/ocamldebug$EXE
installbin lex/ocamllex.opt$EXE $BINDIR/ocamllex.opt$EXE
installbin tools/ocamldep.native$EXE $BINDIR/ocamldep.opt$EXE

echo "Installing some tools..."
installbin tools/ocamlcp.byte$EXE $BINDIR/ocamlcp$EXE
installbin tools/ocamldep.byte$EXE $BINDIR/ocamldep$EXE
installbin tools/ocamlmklib.byte$EXE $BINDIR/ocamlmklib$EXE
installbin tools/ocamlmktop.byte$EXE $BINDIR/ocamlmktop$EXE
installbin tools/ocamlprof.byte$EXE $BINDIR/ocamlprof$EXE
installbin toplevel/expunge.byte$EXE $LIBDIR/expunge$EXE
installbin tools/addlabels.byte $LIBDIR/addlabels
installbin tools/scrapelabels.byte $LIBDIR/scrapelabels
installbin otherlibs/dynlink/extract_crc.byte $LIBDIR/extract_crc
installbin otherlibs/labltk/lib/labltk$EXE $BINDIR/labltk$EXE
installbin otherlibs/labltk/compiler/tkcompiler$EXE $BINDIR/tkcompiler$EXE
installbin otherlibs/labltk/browser/ocamlbrowser$EXE $BINDIR/ocamlbrowser$EXE
installbin otherlibs/labltk/compiler/pp$EXE $LIBDIR/labltk/pp$EXE
installbin otherlibs/labltk/lib/labltktop$EXE $LIBDIR/labltk/labltktop$EXE

echo "Installing libraries..."
installdir \
  otherlibs/bigarray/bigarray.cma \
  otherlibs/dbm/dbm.cma \
  otherlibs/dynlink/dynlink.cma \
  otherlibs/{,win32}graph/graphics.cma \
  otherlibs/num/nums.cma \
  otherlibs/str/str.cma \
  otherlibs/{,win32}unix/unix.cma \
  otherlibs/bigarray/bigarray{,.p}.cmxa \
  otherlibs/dbm/dbm{,.p}.cmxa \
  otherlibs/{,win32}graph/graphics{,.p}.cmxa \
  otherlibs/num/nums{,.p}.cmxa \
  otherlibs/str/str{,.p}.cmxa \
  otherlibs/{,win32}unix/unix{,.p}.cmxa \
  toplevel/toplevellib.cma \
  otherlibs/systhreads/thread.mli \
  otherlibs/systhreads/mutex.mli \
  otherlibs/systhreads/condition.mli \
  otherlibs/systhreads/event.mli \
  otherlibs/systhreads/threadUnix.mli \
  $LIBDIR

installdir \
  otherlibs/labltk/support/fileevent.{mli,cmi,cmx} \
  otherlibs/labltk/support/protocol.{mli,cmi,cmx} \
  otherlibs/labltk/support/textvariable.{mli,cmi,cmx} \
  otherlibs/labltk/support/timer.{mli,cmi,cmx} \
  otherlibs/labltk/support/rawwidget.{mli,cmi,cmx} \
  otherlibs/labltk/support/widget.{mli,cmi,cmx} \
  otherlibs/labltk/support/tkthread.{mli,cmi,cmo} \
  otherlibs/labltk/support/tkthread.$O \
  otherlibs/labltk/labltk/*.mli \
  otherlibs/labltk/labltk/*.cmi \
  otherlibs/labltk/labltk/*.cmx \
  otherlibs/labltk/camltk/*.mli \
  otherlibs/labltk/camltk/*.cmi \
  otherlibs/labltk/camltk/*.cmx \
  otherlibs/labltk/frx/frxlib.cma \
  otherlibs/labltk/frx/frxlib.cmxa \
  otherlibs/labltk/frx/*.mli \
  otherlibs/labltk/frx/*.cmi \
  otherlibs/labltk/frx/*.cmx \
  otherlibs/labltk/jpf/jpflib.cma \
  otherlibs/labltk/jpf/jpflib.cmxa \
  otherlibs/labltk/jpf/*.mli \
  otherlibs/labltk/jpf/*.cmi \
  otherlibs/labltk/jpf/*.cmx \
  otherlibs/labltk/lib/labltk.cma \
  otherlibs/labltk/lib/labltk.cmxa \
  otherlibs/labltk/tkanim/*.mli \
  otherlibs/labltk/tkanim/*.cmi \
  otherlibs/labltk/tkanim/tkanim.cma \
  otherlibs/labltk/tkanim/tkanim.cmxa \
  $LIBDIR/labltk

installdir \
  otherlibs/systhreads/threads.cma \
  otherlibs/systhreads/threads{,.p}.cmxa \
  otherlibs/systhreads/thread.cmi \
  otherlibs/systhreads/mutex.cmi \
  otherlibs/systhreads/condition.cmi \
  otherlibs/systhreads/event.cmi \
  otherlibs/systhreads/threadUnix.cmi \
  $LIBDIR/threads

installdir \
  otherlibs/bigarray/dllbigarray$EXT_DLL \
  otherlibs/dbm/dllmldbm$EXT_DLL \
  otherlibs/{,win32}graph/dllgraphics$EXT_DLL \
  otherlibs/num/dllnums$EXT_DLL \
  otherlibs/str/dllstr$EXT_DLL \
  otherlibs/systhreads/dllthreads$EXT_DLL \
  otherlibs/{,win32}unix/dllunix$EXT_DLL \
  otherlibs/threads/dllvmthreads$EXT_DLL \
  otherlibs/labltk/support/dlllabltk$EXT_DLL \
  otherlibs/labltk/tkanim/dlltkanim$EXT_DLL \
  $STUBLIBDIR

installlibdir \
  otherlibs/threads/libvmthreads.$A \
  $LIBDIR/vmthreads

installdir \
  otherlibs/threads/thread.{cmi,mli} \
  otherlibs/threads/mutex.{cmi,mli} \
  otherlibs/threads/condition.{cmi,mli} \
  otherlibs/threads/event.{cmi,mli} \
  otherlibs/threads/threadUnix.{cmi,mli} \
  otherlibs/threads/threads.cma \
  otherlibs/threads/stdlib.cma \
  otherlibs/threads/unix.cma \
  $LIBDIR/vmthreads

installlibdir \
  otherlibs/labltk/support/liblabltk.$A \
  otherlibs/labltk/lib/labltk.$A \
  otherlibs/labltk/jpf/jpflib.$A \
  otherlibs/labltk/tkanim/libtkanim.$A \
  otherlibs/labltk/tkanim/tkanim.$A \
  otherlibs/labltk/frx/frxlib.$A \
  $LIBDIR/labltk

installlibdir \
  otherlibs/bigarray/libbigarray.{$A,p.$A} \
  otherlibs/dbm/libmldbm.{$A,p.$A} \
  otherlibs/{,win32}graph/libgraphics.{$A,p.$A} \
  otherlibs/num/libnums.{$A,p.$A} \
  otherlibs/str/libstr.{$A,p.$A} \
  otherlibs/systhreads/libthreads.{$A,p.$A} \
  otherlibs/systhreads/libthreadsnat.{$A,p.$A} \
  otherlibs/{,win32}unix/libunix.{$A,p.$A} \
  $LIBDIR

echo "Installing object files and interfaces..."
installdir \
  tools/profiling.cm{o,i} \
  toplevel/topstart.cmo \
  toplevel/toploop.cmi \
  toplevel/topdirs.cmi \
  toplevel/topmain.cmi \
  typing/outcometree.cmi \
  otherlibs/{,win32}graph/graphicsX11.cmi \
  otherlibs/dynlink/dynlink.cmi \
  otherlibs/num/arith_status.cmi \
  otherlibs/num/big_int.cmi \
  otherlibs/num/nat.cmi \
  otherlibs/num/num.cmi \
  otherlibs/num/ratio.cmi \
  otherlibs/bigarray/bigarray.cmi \
  otherlibs/dbm/dbm.cmi \
  otherlibs/{,win32}graph/graphics.cmi \
  otherlibs/str/str.cmi \
  otherlibs/{,win32}unix/unix.cmi \
  otherlibs/{,win32}unix/unixLabels.cmi \
  otherlibs/num/arith_flags{,.p}.{cmx,$O} \
  otherlibs/num/int_misc{,.p}.{cmx,$O} \
  otherlibs/num/arith_status{,.p}.{cmx,$O} \
  otherlibs/num/big_int{,.p}.{cmx,$O} \
  otherlibs/num/nat{,.p}.{cmx,$O} \
  otherlibs/num/num{,.p}.{cmx,$O} \
  otherlibs/num/ratio{,.p}.{cmx,$O} \
  otherlibs/bigarray/bigarray{,.p}.{cmx,$O} \
  otherlibs/dbm/dbm{,.p}.{cmx,$O} \
  otherlibs/{,win32}graph/graphics{,.p}.{cmx,$O} \
  otherlibs/str/str{,.p}.{cmx,$O} \
  otherlibs/{,win32}unix/unix{,.p}.{cmx,$O} \
  otherlibs/{,win32}unix/unixLabels{,.p}.{cmx,$O} \
  $LIBDIR

installlibdir \
  otherlibs/bigarray/bigarray.{$A,p.$A} \
  otherlibs/dbm/dbm.{$A,p.$A} \
  otherlibs/{,win32}graph/graphics.{$A,p.$A} \
  otherlibs/num/nums.{$A,p.$A} \
  otherlibs/str/str.{$A,p.$A} \
  otherlibs/{,win32}unix/unix.{$A,p.$A} \
  stdlib/stdlib.{$A,p.$A} \
  $LIBDIR

installlibdir \
  otherlibs/systhreads/threads.{$A,p.$A} \
  $LIBDIR/threads

echo "Installing manuals..."
(cd ../man && make install)

echo "Installing ocamldoc..."
installbin ocamldoc/ocamldoc$EXE $BINDIR/ocamldoc$EXE
installbin ocamldoc/ocamldoc.opt$EXE $BINDIR/ocamldoc.opt$EXE

installdir \
  ../ocamldoc/ocamldoc.hva \
  ocamldoc/*.cmi \
  ocamldoc/odoc_info.{mli,cm[ia],cmxa,$A} \
  $LIBDIR/ocamldoc

installdir \
  ocamldoc/stdlib_man/* \
  $MANDIR/man3

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
