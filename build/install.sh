#!/bin/sh
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
  installdir byterun/ocamlrun.dll $BINDIR
  WIN32=win32
fi

installdir otherlibs/"$WIN32"unix/unixsupport.h \
           otherlibs/bigarray/bigarray.h \
           $LIBDIR/caml

installdir yacc/ocamlyacc byterun/ocamlrun $BINDIR

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
  stdlib/std_exit.cm[io] \
  stdlib/arg.cmi stdlib/arg.mli \
  stdlib/array.cmi stdlib/array.mli \
  stdlib/arrayLabels.cmi stdlib/arrayLabels.mli \
  stdlib/buffer.cmi stdlib/buffer.mli \
  stdlib/callback.cmi stdlib/callback.mli \
  stdlib/camlinternalMod.cmi stdlib/camlinternalMod.mli \
  stdlib/camlinternalOO.cmi stdlib/camlinternalOO.mli \
  stdlib/char.cmi stdlib/char.mli \
  stdlib/complex.cmi stdlib/complex.mli \
  stdlib/digest.cmi stdlib/digest.mli \
  stdlib/filename.cmi stdlib/filename.mli \
  stdlib/format.cmi stdlib/format.mli \
  stdlib/gc.cmi stdlib/gc.mli \
  stdlib/genlex.cmi stdlib/genlex.mli \
  stdlib/hashtbl.cmi stdlib/hashtbl.mli \
  stdlib/int32.cmi stdlib/int32.mli \
  stdlib/int64.cmi stdlib/int64.mli \
  stdlib/lazy.cmi stdlib/lazy.mli \
  stdlib/lexing.cmi stdlib/lexing.mli \
  stdlib/list.cmi stdlib/list.mli \
  stdlib/listLabels.cmi stdlib/listLabels.mli \
  stdlib/map.cmi stdlib/map.mli \
  stdlib/marshal.cmi stdlib/marshal.mli \
  stdlib/moreLabels.cmi stdlib/moreLabels.mli \
  stdlib/nativeint.cmi stdlib/nativeint.mli \
  stdlib/obj.cmi stdlib/obj.mli \
  stdlib/oo.cmi stdlib/oo.mli \
  stdlib/parsing.cmi stdlib/parsing.mli \
  stdlib/pervasives.cmi stdlib/pervasives.mli \
  stdlib/printexc.cmi stdlib/printexc.mli \
  stdlib/printf.cmi stdlib/printf.mli \
  stdlib/queue.cmi stdlib/queue.mli \
  stdlib/random.cmi stdlib/random.mli \
  stdlib/scanf.cmi stdlib/scanf.mli \
  stdlib/sort.cmi stdlib/sort.mli \
  stdlib/stack.cmi stdlib/stack.mli \
  stdlib/stdLabels.cmi stdlib/stdLabels.mli \
  stdlib/stream.cmi stdlib/stream.mli \
  stdlib/string.cmi stdlib/string.mli \
  stdlib/stringLabels.cmi stdlib/stringLabels.mli \
  stdlib/sys.cmi stdlib/sys.mli \
  stdlib/weak.cmi stdlib/weak.mli \
  stdlib/$set.cmi stdlib/$set.mli \
 stdlib/arg.cmx stdlib/arg.p.cmx stdlib/arg.$O stdlib/arg.p.$O \
 stdlib/array.cmx stdlib/array.p.cmx stdlib/array.$O stdlib/array.p.$O \
 stdlib/arrayLabels.cmx stdlib/arrayLabels.p.cmx stdlib/arrayLabels.$O stdlib/arrayLabels.p.$O \
 stdlib/buffer.cmx stdlib/buffer.p.cmx stdlib/buffer.$O stdlib/buffer.p.$O \
 stdlib/callback.cmx stdlib/callback.p.cmx stdlib/callback.$O stdlib/callback.p.$O \
 stdlib/camlinternalMod.cmx stdlib/camlinternalMod.p.cmx stdlib/camlinternalMod.$O stdlib/camlinternalMod.p.$O \
 stdlib/camlinternalOO.cmx stdlib/camlinternalOO.p.cmx stdlib/camlinternalOO.$O stdlib/camlinternalOO.p.$O \
 stdlib/char.cmx stdlib/char.p.cmx stdlib/char.$O stdlib/char.p.$O \
 stdlib/complex.cmx stdlib/complex.p.cmx stdlib/complex.$O stdlib/complex.p.$O \
 stdlib/digest.cmx stdlib/digest.p.cmx stdlib/digest.$O stdlib/digest.p.$O \
 stdlib/filename.cmx stdlib/filename.p.cmx stdlib/filename.$O stdlib/filename.p.$O \
 stdlib/format.cmx stdlib/format.p.cmx stdlib/format.$O stdlib/format.p.$O \
 stdlib/gc.cmx stdlib/gc.p.cmx stdlib/gc.$O stdlib/gc.p.$O \
 stdlib/genlex.cmx stdlib/genlex.p.cmx stdlib/genlex.$O stdlib/genlex.p.$O \
 stdlib/hashtbl.cmx stdlib/hashtbl.p.cmx stdlib/hashtbl.$O stdlib/hashtbl.p.$O \
 stdlib/int32.cmx stdlib/int32.p.cmx stdlib/int32.$O stdlib/int32.p.$O \
 stdlib/int64.cmx stdlib/int64.p.cmx stdlib/int64.$O stdlib/int64.p.$O \
 stdlib/lazy.cmx stdlib/lazy.p.cmx stdlib/lazy.$O stdlib/lazy.p.$O \
 stdlib/lexing.cmx stdlib/lexing.p.cmx stdlib/lexing.$O stdlib/lexing.p.$O \
 stdlib/list.cmx stdlib/list.p.cmx stdlib/list.$O stdlib/list.p.$O \
 stdlib/listLabels.cmx stdlib/listLabels.p.cmx stdlib/listLabels.$O stdlib/listLabels.p.$O \
 stdlib/map.cmx stdlib/map.p.cmx stdlib/map.$O stdlib/map.p.$O \
 stdlib/marshal.cmx stdlib/marshal.p.cmx stdlib/marshal.$O stdlib/marshal.p.$O \
 stdlib/moreLabels.cmx stdlib/moreLabels.p.cmx stdlib/moreLabels.$O stdlib/moreLabels.p.$O \
 stdlib/nativeint.cmx stdlib/nativeint.p.cmx stdlib/nativeint.$O stdlib/nativeint.p.$O \
 stdlib/obj.cmx stdlib/obj.p.cmx stdlib/obj.$O stdlib/obj.p.$O \
 stdlib/oo.cmx stdlib/oo.p.cmx stdlib/oo.$O stdlib/oo.p.$O \
 stdlib/parsing.cmx stdlib/parsing.p.cmx stdlib/parsing.$O stdlib/parsing.p.$O \
 stdlib/pervasives.cmx stdlib/pervasives.p.cmx stdlib/pervasives.$O stdlib/pervasives.p.$O \
 stdlib/printexc.cmx stdlib/printexc.p.cmx stdlib/printexc.$O stdlib/printexc.p.$O \
 stdlib/printf.cmx stdlib/printf.p.cmx stdlib/printf.$O stdlib/printf.p.$O \
 stdlib/queue.cmx stdlib/queue.p.cmx stdlib/queue.$O stdlib/queue.p.$O \
 stdlib/random.cmx stdlib/random.p.cmx stdlib/random.$O stdlib/random.p.$O \
 stdlib/scanf.cmx stdlib/scanf.p.cmx stdlib/scanf.$O stdlib/scanf.p.$O \
 stdlib/sort.cmx stdlib/sort.p.cmx stdlib/sort.$O stdlib/sort.p.$O \
 stdlib/stack.cmx stdlib/stack.p.cmx stdlib/stack.$O stdlib/stack.p.$O \
 stdlib/stdLabels.cmx stdlib/stdLabels.p.cmx stdlib/stdLabels.$O stdlib/stdLabels.p.$O \
 stdlib/std_exit.cmx stdlib/std_exit.p.cmx stdlib/std_exit.$O stdlib/std_exit.p.$O \
 stdlib/stream.cmx stdlib/stream.p.cmx stdlib/stream.$O stdlib/stream.p.$O \
 stdlib/string.cmx stdlib/string.p.cmx stdlib/string.$O stdlib/string.p.$O \
 stdlib/stringLabels.cmx stdlib/stringLabels.p.cmx stdlib/stringLabels.$O stdlib/stringLabels.p.$O \
 stdlib/sys.cmx stdlib/sys.p.cmx stdlib/sys.$O stdlib/sys.p.$O \
 stdlib/weak.cmx stdlib/weak.p.cmx stdlib/weak.$O stdlib/weak.p.$O \
 stdlib/$set.cmx stdlib/$set.p.cmx stdlib/$set.$O stdlib/$set.p.$O \
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
  otherlibs/"$WIN32"graph/graphics.cma \
  otherlibs/num/nums.cma \
  otherlibs/str/str.cma \
  otherlibs/"$WIN32"unix/unix.cma \
  otherlibs/bigarray/bigarray.cmxa \
  otherlibs/dbm/dbm.cmxa \
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
  otherlibs/systhreads/threads.cmxa \
  otherlibs/systhreads/thread.cmi \
  otherlibs/systhreads/mutex.cmi \
  otherlibs/systhreads/condition.cmi \
  otherlibs/systhreads/event.cmi \
  otherlibs/systhreads/threadUnix.cmi \
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
  otherlibs/labltk/tkanim/dlltkanim$EXT_DLL \
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
  otherlibs/labltk/tkanim/libtkanim.$A \
  otherlibs/labltk/tkanim/tkanim.$A \
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
  otherlibs/graph/graphicsX11.cmi \
  otherlibs/dynlink/dynlink.cmi \
  otherlibs/num/arith_status.cmi \
  otherlibs/num/big_int.cmi \
  otherlibs/num/nat.cmi \
  otherlibs/num/num.cmi \
  otherlibs/num/ratio.cmi \
  otherlibs/bigarray/bigarray.cmi \
  otherlibs/dbm/dbm.cmi \
  otherlibs/"$WIN32"graph/graphics.cmi \
  otherlibs/str/str.cmi \
  otherlibs/"$WIN32"unix/unix.cmi \
  otherlibs/"$WIN32"unix/unixLabels.cmi \
  otherlibs/num/arith_flags.cmx \
  otherlibs/num/arith_flags.$O \
  otherlibs/num/int_misc.cmx \
  otherlibs/num/int_misc.$O \
  otherlibs/num/arith_status.cmx \
  otherlibs/num/arith_status.$O \
  otherlibs/num/big_int.cmx \
  otherlibs/num/big_int.$O \
  otherlibs/num/nat.cmx \
  otherlibs/num/nat.$O \
  otherlibs/num/num.cmx \
  otherlibs/num/num.$O \
  otherlibs/num/ratio.cmx \
  otherlibs/num/ratio.$O \
  otherlibs/bigarray/bigarray.cmx \
  otherlibs/bigarray/bigarray.$O \
  otherlibs/dbm/dbm.cmx \
  otherlibs/dbm/dbm.$O \
  otherlibs/"$WIN32"graph/graphics.cmx \
  otherlibs/"$WIN32"graph/graphics.$O \
  otherlibs/str/str.cmx \
  otherlibs/str/str.$O \
  otherlibs/"$WIN32"unix/unix.cmx \
  otherlibs/"$WIN32"unix/unix.$O \
  otherlibs/"$WIN32"unix/unixLabels.cmx \
  otherlibs/"$WIN32"unix/unixLabels.$O \
  $LIBDIR

installlibdir \
  otherlibs/bigarray/bigarray.$A \
  otherlibs/dbm/dbm.$A \
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
