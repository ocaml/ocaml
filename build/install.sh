#!/bin/sh

#########################################################################
#                                                                       #
#                            Objective Caml                             #
#                                                                       #
#       Nicolas Pouillard, projet Gallium, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 2007 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

set -e

cd `dirname $0`/..

. config/config.sh

not_installed=$PWD/_build/not_installed

rm -f "$not_installed"
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
mkdir -p $LIBDIR/caml
mkdir -p $LIBDIR/camlp4
mkdir -p $LIBDIR/vmthreads
mkdir -p $LIBDIR/threads
mkdir -p $LIBDIR/labltk
mkdir -p $LIBDIR/ocamlbuild
mkdir -p $LIBDIR/ocamldoc
mkdir -p $LIBDIR/ocamldoc/custom
mkdir -p $STUBLIBDIR
mkdir -p $MANDIR/man1
mkdir -p $MANDIR/man3
mkdir -p $MANDIR/man$MANEXT

echo "Installing core libraries..."
installlibdir byterun/libcamlrun.$A asmrun/libasmrun.$A asmrun/libasmrunp.$A \
              $LIBDIR
installdir byterun/libcamlrun_shared$EXT_DLL $LIBDIR

PUBLIC_INCLUDES="\
  alloc.h callback.h config.h custom.h fail.h intext.h \
  memory.h misc.h mlvalues.h printexc.h signals.h compatibility.h"

cd byterun
for i in $PUBLIC_INCLUDES; do
  echo "  install caml/$i"
  sed -f ../tools/cleanup-header $i > $LIBDIR/caml/$i
done
cd ..

WIN32=""
if [ "x$EXE" = "x.exe" ]; then
  installbin win32caml/ocamlwin.exe $PREFIX/OCamlWin.exe
  WIN32=win32
fi

installdir otherlibs/"$WIN32"unix/unixsupport.h \
           otherlibs/bigarray/bigarray.h \
           $LIBDIR/caml

installdir yacc/ocamlyacc$EXE byterun/ocamlrun$EXE $BINDIR

installdir config/Makefile $LIBDIR/Makefile.config
installdir byterun/ld.conf $LIBDIR

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
  stdlib/stdlib.cmxa stdlib/stdlib.p.cmxa \
  stdlib/camlheader \
  stdlib/camlheader_ur \
  stdlib/std_exit.cm[io] stdlib/std_exit.ml \
  stdlib/arg.cmi stdlib/arg.ml stdlib/arg.mli \
  stdlib/array.cmi stdlib/array.ml stdlib/array.mli \
  stdlib/arrayLabels.cmi stdlib/arrayLabels.ml stdlib/arrayLabels.mli \
  stdlib/buffer.cmi stdlib/buffer.ml stdlib/buffer.mli \
  stdlib/callback.cmi stdlib/callback.ml stdlib/callback.mli \
  stdlib/camlinternalLazy.cmi stdlib/camlinternalLazy.ml stdlib/camlinternalLazy.mli \
  stdlib/camlinternalMod.cmi stdlib/camlinternalMod.ml stdlib/camlinternalMod.mli \
  stdlib/camlinternalOO.cmi stdlib/camlinternalOO.ml stdlib/camlinternalOO.mli \
  stdlib/char.cmi stdlib/char.ml stdlib/char.mli \
  stdlib/complex.cmi stdlib/complex.ml stdlib/complex.mli \
  stdlib/digest.cmi stdlib/digest.ml stdlib/digest.mli \
  stdlib/filename.cmi stdlib/filename.ml stdlib/filename.mli \
  stdlib/format.cmi stdlib/format.ml stdlib/format.mli \
  stdlib/gc.cmi stdlib/gc.ml stdlib/gc.mli \
  stdlib/genlex.cmi stdlib/genlex.ml stdlib/genlex.mli \
  stdlib/hashtbl.cmi stdlib/hashtbl.ml stdlib/hashtbl.mli \
  stdlib/int32.cmi stdlib/int32.ml stdlib/int32.mli \
  stdlib/int64.cmi stdlib/int64.ml stdlib/int64.mli \
  stdlib/lazy.cmi stdlib/lazy.ml stdlib/lazy.mli \
  stdlib/lexing.cmi stdlib/lexing.ml stdlib/lexing.mli \
  stdlib/list.cmi stdlib/list.ml stdlib/list.mli \
  stdlib/listLabels.cmi stdlib/listLabels.ml stdlib/listLabels.mli \
  stdlib/map.cmi stdlib/map.ml stdlib/map.mli \
  stdlib/marshal.cmi stdlib/marshal.ml stdlib/marshal.mli \
  stdlib/moreLabels.cmi stdlib/moreLabels.ml stdlib/moreLabels.mli \
  stdlib/nativeint.cmi stdlib/nativeint.ml stdlib/nativeint.mli \
  stdlib/obj.cmi stdlib/obj.ml stdlib/obj.mli \
  stdlib/oo.cmi stdlib/oo.ml stdlib/oo.mli \
  stdlib/parsing.cmi stdlib/parsing.ml stdlib/parsing.mli \
  stdlib/pervasives.cmi stdlib/pervasives.ml stdlib/pervasives.mli \
  stdlib/printexc.cmi stdlib/printexc.ml stdlib/printexc.mli \
  stdlib/printf.cmi stdlib/printf.ml stdlib/printf.mli \
  stdlib/queue.cmi stdlib/queue.ml stdlib/queue.mli \
  stdlib/random.cmi stdlib/random.ml stdlib/random.mli \
  stdlib/scanf.cmi stdlib/scanf.ml stdlib/scanf.mli \
  stdlib/sort.cmi stdlib/sort.ml stdlib/sort.mli \
  stdlib/stack.cmi stdlib/stack.ml stdlib/stack.mli \
  stdlib/stdLabels.cmi stdlib/stdLabels.ml stdlib/stdLabels.mli \
  stdlib/stream.cmi stdlib/stream.ml stdlib/stream.mli \
  stdlib/string.cmi stdlib/string.ml stdlib/string.mli \
  stdlib/stringLabels.cmi stdlib/stringLabels.ml stdlib/stringLabels.mli \
  stdlib/sys.cmi stdlib/sys.ml stdlib/sys.mli \
  stdlib/weak.cmi stdlib/weak.ml stdlib/weak.mli \
  stdlib/$set.cmi stdlib/$set.ml stdlib/$set.mli \
 stdlib/arg.cmx stdlib/arg.p.cmx \
 stdlib/array.cmx stdlib/array.p.cmx \
 stdlib/arrayLabels.cmx stdlib/arrayLabels.p.cmx \
 stdlib/buffer.cmx stdlib/buffer.p.cmx \
 stdlib/callback.cmx stdlib/callback.p.cmx \
 stdlib/camlinternalLazy.cmx stdlib/camlinternalLazy.p.cmx \
 stdlib/camlinternalMod.cmx stdlib/camlinternalMod.p.cmx \
 stdlib/camlinternalOO.cmx stdlib/camlinternalOO.p.cmx \
 stdlib/char.cmx stdlib/char.p.cmx \
 stdlib/complex.cmx stdlib/complex.p.cmx \
 stdlib/digest.cmx stdlib/digest.p.cmx \
 stdlib/filename.cmx stdlib/filename.p.cmx \
 stdlib/format.cmx stdlib/format.p.cmx \
 stdlib/gc.cmx stdlib/gc.p.cmx \
 stdlib/genlex.cmx stdlib/genlex.p.cmx \
 stdlib/hashtbl.cmx stdlib/hashtbl.p.cmx \
 stdlib/int32.cmx stdlib/int32.p.cmx \
 stdlib/int64.cmx stdlib/int64.p.cmx \
 stdlib/lazy.cmx stdlib/lazy.p.cmx \
 stdlib/lexing.cmx stdlib/lexing.p.cmx \
 stdlib/list.cmx stdlib/list.p.cmx \
 stdlib/listLabels.cmx stdlib/listLabels.p.cmx \
 stdlib/map.cmx stdlib/map.p.cmx \
 stdlib/marshal.cmx stdlib/marshal.p.cmx \
 stdlib/moreLabels.cmx stdlib/moreLabels.p.cmx \
 stdlib/nativeint.cmx stdlib/nativeint.p.cmx \
 stdlib/obj.cmx stdlib/obj.p.cmx \
 stdlib/oo.cmx stdlib/oo.p.cmx \
 stdlib/parsing.cmx stdlib/parsing.p.cmx \
 stdlib/pervasives.cmx stdlib/pervasives.p.cmx \
 stdlib/printexc.cmx stdlib/printexc.p.cmx \
 stdlib/printf.cmx stdlib/printf.p.cmx \
 stdlib/queue.cmx stdlib/queue.p.cmx \
 stdlib/random.cmx stdlib/random.p.cmx \
 stdlib/scanf.cmx stdlib/scanf.p.cmx \
 stdlib/sort.cmx stdlib/sort.p.cmx \
 stdlib/stack.cmx stdlib/stack.p.cmx \
 stdlib/stdLabels.cmx stdlib/stdLabels.p.cmx \
 stdlib/std_exit.cmx stdlib/std_exit.p.cmx stdlib/std_exit.$O stdlib/std_exit.p.$O \
 stdlib/stream.cmx stdlib/stream.p.cmx \
 stdlib/string.cmx stdlib/string.p.cmx \
 stdlib/stringLabels.cmx stdlib/stringLabels.p.cmx \
 stdlib/sys.cmx stdlib/sys.p.cmx \
 stdlib/weak.cmx stdlib/weak.p.cmx \
 stdlib/$set.cmx stdlib/$set.p.cmx \
  $LIBDIR

installlibdir \
  stdlib/stdlib.$A stdlib/stdlib.p.$A \
  $LIBDIR

echo "Installing ocamllex, ocamldebug..."
installbin lex/ocamllex$EXE $BINDIR/ocamllex$EXE
installbin debugger/ocamldebug$EXE $BINDIR/ocamldebug$EXE
installbin lex/ocamllex.opt$EXE $BINDIR/ocamllex.opt$EXE
installbin tools/ocamldep.native$EXE $BINDIR/ocamldep.opt$EXE

echo "Installing some tools..."
installbin tools/objinfo.byte$EXE $BINDIR/ocamlobjinfo$EXE
installbin ../tools/objinfo_helper$EXE $LIBDIR/objinfo_helper$EXE
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
installbin otherlibs/labltk/browser/ocamlbrowser$EXE $BINDIR/ocamlbrowser$EXE
installbin otherlibs/labltk/compiler/pp$EXE $LIBDIR/labltk/pp$EXE
installbin otherlibs/labltk/lib/labltktop$EXE $LIBDIR/labltk/labltktop$EXE

echo "Installing libraries..."
installdir \
  otherlibs/bigarray/bigarray.cma \
  otherlibs/dbm/dbm.cma \
  otherlibs/dynlink/dynlink.cma \
  otherlibs/"$WIN32"graph/graphics.cma \
  otherlibs/num/nums.cma \
  otherlibs/str/str.cma \
  otherlibs/"$WIN32"unix/unix.cma \
  otherlibs/bigarray/bigarray.cmxa \
  otherlibs/dbm/dbm.cmxa \
  otherlibs/dynlink/dynlink.cmxa \
  otherlibs/"$WIN32"graph/graphics.cmxa \
  otherlibs/num/nums.cmxa \
  otherlibs/str/str.cmxa \
  otherlibs/"$WIN32"unix/unix.cmxa \
  toplevel/toplevellib.cma \
  otherlibs/systhreads/thread.mli \
  otherlibs/systhreads/mutex.mli \
  otherlibs/systhreads/condition.mli \
  otherlibs/systhreads/event.mli \
  otherlibs/systhreads/threadUnix.mli \
  $LIBDIR

installdir \
  otherlibs/labltk/support/fileevent.mli \
  otherlibs/labltk/support/fileevent.cmi \
  otherlibs/labltk/support/fileevent.cmx \
  otherlibs/labltk/support/protocol.mli \
  otherlibs/labltk/support/protocol.cmi \
  otherlibs/labltk/support/protocol.cmx \
  otherlibs/labltk/support/textvariable.mli \
  otherlibs/labltk/support/textvariable.cmi \
  otherlibs/labltk/support/textvariable.cmx \
  otherlibs/labltk/support/timer.mli \
  otherlibs/labltk/support/timer.cmi \
  otherlibs/labltk/support/timer.cmx \
  otherlibs/labltk/support/rawwidget.mli \
  otherlibs/labltk/support/rawwidget.cmi \
  otherlibs/labltk/support/rawwidget.cmx \
  otherlibs/labltk/support/widget.mli \
  otherlibs/labltk/support/widget.cmi \
  otherlibs/labltk/support/widget.cmx \
  otherlibs/labltk/support/tkthread.mli \
  otherlibs/labltk/support/tkthread.cmi \
  otherlibs/labltk/support/tkthread.cmo \
  otherlibs/labltk/support/tkthread.$O \
  otherlibs/labltk/support/tkthread.cmx \
  otherlibs/labltk/labltk/[^_]*.mli \
  otherlibs/labltk/labltk/*.cmi \
  otherlibs/labltk/labltk/*.cmx \
  otherlibs/labltk/camltk/[^_]*.mli \
  otherlibs/labltk/camltk/*.cmi \
  otherlibs/labltk/camltk/*.cmx \
  otherlibs/labltk/frx/frxlib.cma \
  otherlibs/labltk/frx/frxlib.cmxa \
  ../otherlibs/labltk/frx/*.mli \
  otherlibs/labltk/frx/*.cmi \
  otherlibs/labltk/jpf/jpflib.cma \
  otherlibs/labltk/jpf/jpflib.cmxa \
  otherlibs/labltk/jpf/*.mli \
  otherlibs/labltk/jpf/*.cmi \
  otherlibs/labltk/jpf/*.cmx \
  otherlibs/labltk/lib/labltk.cma \
  otherlibs/labltk/lib/labltk.cmxa \
  otherlibs/labltk/lib/labltk.cmx \
  otherlibs/labltk/compiler/tkcompiler \
  $LIBDIR/labltk

installdir \
  otherlibs/systhreads/threads.cma \
  otherlibs/systhreads/threads.cmxa \
  otherlibs/systhreads/thread.cmi \
  otherlibs/systhreads/thread.cmx \
  otherlibs/systhreads/mutex.cmi \
  otherlibs/systhreads/mutex.cmx \
  otherlibs/systhreads/condition.cmi \
  otherlibs/systhreads/condition.cmx \
  otherlibs/systhreads/event.cmi \
  otherlibs/systhreads/event.cmx \
  otherlibs/systhreads/threadUnix.cmi \
  otherlibs/systhreads/threadUnix.cmx \
  $LIBDIR/threads

installdir \
  otherlibs/bigarray/dllbigarray$EXT_DLL \
  otherlibs/dbm/dllmldbm$EXT_DLL \
  otherlibs/"$WIN32"graph/dllgraphics$EXT_DLL \
  otherlibs/num/dllnums$EXT_DLL \
  otherlibs/str/dllstr$EXT_DLL \
  otherlibs/systhreads/dllthreads$EXT_DLL \
  otherlibs/"$WIN32"unix/dllunix$EXT_DLL \
  otherlibs/threads/dllvmthreads$EXT_DLL \
  otherlibs/labltk/support/dlllabltk$EXT_DLL \
  $STUBLIBDIR

installlibdir \
  otherlibs/threads/libvmthreads.$A \
  $LIBDIR/vmthreads

installdir \
  otherlibs/threads/thread.cmi \
  otherlibs/threads/thread.mli \
  otherlibs/threads/mutex.cmi \
  otherlibs/threads/mutex.mli \
  otherlibs/threads/condition.cmi \
  otherlibs/threads/condition.mli \
  otherlibs/threads/event.cmi \
  otherlibs/threads/event.mli \
  otherlibs/threads/threadUnix.cmi \
  otherlibs/threads/threadUnix.mli \
  otherlibs/threads/threads.cma \
  otherlibs/threads/stdlib.cma \
  otherlibs/threads/unix.cma \
  $LIBDIR/vmthreads

installlibdir \
  otherlibs/labltk/support/liblabltk.$A \
  otherlibs/labltk/lib/labltk.$A \
  otherlibs/labltk/jpf/jpflib.$A \
  otherlibs/labltk/frx/frxlib.$A \
  $LIBDIR/labltk

installlibdir \
  otherlibs/bigarray/libbigarray.$A \
  otherlibs/dbm/libmldbm.$A \
  otherlibs/"$WIN32"graph/libgraphics.$A \
  otherlibs/num/libnums.$A \
  otherlibs/str/libstr.$A \
  otherlibs/systhreads/libthreads.$A \
  otherlibs/systhreads/libthreadsnat.$A \
  otherlibs/"$WIN32"unix/libunix.$A \
  $LIBDIR

echo "Installing object files and interfaces..."
installdir \
  tools/profiling.cm[oi] \
  toplevel/topstart.cmo \
  toplevel/toploop.cmi \
  toplevel/topdirs.cmi \
  toplevel/topmain.cmi \
  typing/outcometree.cmi \
  typing/outcometree.mli \
  otherlibs/graph/graphicsX11.cmi \
  otherlibs/graph/graphicsX11.mli \
  otherlibs/dynlink/dynlink.cmi \
  otherlibs/dynlink/dynlink.mli \
  otherlibs/num/arith_status.cmi \
  otherlibs/num/arith_status.mli \
  otherlibs/num/big_int.cmi \
  otherlibs/num/big_int.mli \
  otherlibs/num/nat.cmi \
  otherlibs/num/nat.mli \
  otherlibs/num/num.cmi \
  otherlibs/num/num.mli \
  otherlibs/num/ratio.cmi \
  otherlibs/num/ratio.mli \
  otherlibs/bigarray/bigarray.cmi \
  otherlibs/bigarray/bigarray.mli \
  otherlibs/dbm/dbm.cmi \
  otherlibs/dbm/dbm.mli \
  otherlibs/dynlink/dynlink.cmx \
  otherlibs/"$WIN32"graph/graphics.cmi \
  otherlibs/"$WIN32"graph/graphics.mli \
  otherlibs/str/str.cmi \
  otherlibs/str/str.mli \
  otherlibs/"$WIN32"unix/unix.cmi \
  otherlibs/"$WIN32"unix/unix.mli \
  otherlibs/"$WIN32"unix/unixLabels.cmi \
  otherlibs/"$WIN32"unix/unixLabels.mli \
  otherlibs/num/arith_flags.cmx \
  otherlibs/num/int_misc.cmx \
  otherlibs/num/arith_status.cmx \
  otherlibs/num/big_int.cmx \
  otherlibs/num/nat.cmx \
  otherlibs/num/num.cmx \
  otherlibs/num/ratio.cmx \
  otherlibs/bigarray/bigarray.cmx \
  otherlibs/dbm/dbm.cmx \
  otherlibs/"$WIN32"graph/graphics.cmx \
  otherlibs/graph/graphicsX11.cmx \
  otherlibs/str/str.cmx \
  otherlibs/"$WIN32"unix/unix.cmx \
  otherlibs/"$WIN32"unix/unixLabels.cmx \
  $LIBDIR

installlibdir \
  otherlibs/bigarray/bigarray.$A \
  otherlibs/dbm/dbm.$A \
  otherlibs/dynlink/dynlink.$A \
  otherlibs/"$WIN32"graph/graphics.$A \
  otherlibs/num/nums.$A \
  otherlibs/str/str.$A \
  otherlibs/"$WIN32"unix/unix.$A \
  stdlib/stdlib.$A \
  $LIBDIR

installlibdir \
  otherlibs/systhreads/threads.$A \
  $LIBDIR/threads

echo "Installing manuals..."
(cd ../man && make install)

echo "Installing ocamldoc..."
installbin ocamldoc/ocamldoc $BINDIR/ocamldoc$EXE
installbin ocamldoc/ocamldoc.opt $BINDIR/ocamldoc.opt$EXE

installdir \
  ../ocamldoc/ocamldoc.hva \
  ocamldoc/*.cmi \
  ocamldoc/odoc_info.mli ocamldoc/odoc_info.cm[ia] ocamldoc/odoc_info.cmxa \
  ocamldoc/odoc_info.$A \
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

echo "Installing ocamlbuild..."

cd ocamlbuild
installbin ocamlbuild.byte$EXE $BINDIR/ocamlbuild.byte$EXE
installbin ocamlbuild.native$EXE $BINDIR/ocamlbuild.native$EXE
installbestbin ocamlbuild.native$EXE ocamlbuild.byte$EXE $BINDIR/ocamlbuild$EXE

installlibdir \
  ocamlbuildlib.$A \
  $LIBDIR/ocamlbuild

installdir \
  ocamlbuildlib.cmxa \
  ocamlbuildlib.cma \
  ocamlbuild_plugin.cmi \
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
