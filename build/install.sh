#!/bin/sh
# $Id$
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
mkdir -p $LIBDIR/ocamldoc/custom
mkdir -p $STUBLIBDIR
mkdir -p $MANDIR/man1
mkdir -p $MANDIR/man3
mkdir -p $MANDIR/man$MANEXT

echo "Installing core libraries..."
installlibdir byterun/libcamlrun.$A byterun/libcamlrunp.$A \
              asmrun/libasmrun.$A asmrun/libasmrunp.$A \
              asmrun/libasmrunp.$A asmrun/libasmrunpp.$A \
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

installdir otherlibs/win32unix/unixsupport.h otherlibs/unix/unixsupport.h \
           $LIBDIR/caml

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
  stdlib/stdlib.cmxa stdlib/stdlibp.cmxa \
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
 stdlib/arg.cmx stdlib/argp.cmx stdlib/arg.$O stdlib/argp.$O \
 stdlib/array.cmx stdlib/arrayp.cmx stdlib/array.$O stdlib/arrayp.$O \
 stdlib/arrayLabels.cmx stdlib/arrayLabelsp.cmx stdlib/arrayLabels.$O stdlib/arrayLabelsp.$O \
 stdlib/buffer.cmx stdlib/bufferp.cmx stdlib/buffer.$O stdlib/bufferp.$O \
 stdlib/callback.cmx stdlib/callbackp.cmx stdlib/callback.$O stdlib/callbackp.$O \
 stdlib/camlinternalMod.cmx stdlib/camlinternalModp.cmx stdlib/camlinternalMod.$O stdlib/camlinternalModp.$O \
 stdlib/camlinternalOO.cmx stdlib/camlinternalOOp.cmx stdlib/camlinternalOO.$O stdlib/camlinternalOOp.$O \
 stdlib/char.cmx stdlib/charp.cmx stdlib/char.$O stdlib/charp.$O \
 stdlib/complex.cmx stdlib/complexp.cmx stdlib/complex.$O stdlib/complexp.$O \
 stdlib/digest.cmx stdlib/digestp.cmx stdlib/digest.$O stdlib/digestp.$O \
 stdlib/filename.cmx stdlib/filenamep.cmx stdlib/filename.$O stdlib/filenamep.$O \
 stdlib/format.cmx stdlib/formatp.cmx stdlib/format.$O stdlib/formatp.$O \
 stdlib/gc.cmx stdlib/gcp.cmx stdlib/gc.$O stdlib/gcp.$O \
 stdlib/genlex.cmx stdlib/genlexp.cmx stdlib/genlex.$O stdlib/genlexp.$O \
 stdlib/hashtbl.cmx stdlib/hashtblp.cmx stdlib/hashtbl.$O stdlib/hashtblp.$O \
 stdlib/int32.cmx stdlib/int32p.cmx stdlib/int32.$O stdlib/int32p.$O \
 stdlib/int64.cmx stdlib/int64p.cmx stdlib/int64.$O stdlib/int64p.$O \
 stdlib/lazy.cmx stdlib/lazyp.cmx stdlib/lazy.$O stdlib/lazyp.$O \
 stdlib/lexing.cmx stdlib/lexingp.cmx stdlib/lexing.$O stdlib/lexingp.$O \
 stdlib/list.cmx stdlib/listp.cmx stdlib/list.$O stdlib/listp.$O \
 stdlib/listLabels.cmx stdlib/listLabelsp.cmx stdlib/listLabels.$O stdlib/listLabelsp.$O \
 stdlib/map.cmx stdlib/mapp.cmx stdlib/map.$O stdlib/mapp.$O \
 stdlib/marshal.cmx stdlib/marshalp.cmx stdlib/marshal.$O stdlib/marshalp.$O \
 stdlib/moreLabels.cmx stdlib/moreLabelsp.cmx stdlib/moreLabels.$O stdlib/moreLabelsp.$O \
 stdlib/nativeint.cmx stdlib/nativeintp.cmx stdlib/nativeint.$O stdlib/nativeintp.$O \
 stdlib/obj.cmx stdlib/objp.cmx stdlib/obj.$O stdlib/objp.$O \
 stdlib/oo.cmx stdlib/oop.cmx stdlib/oo.$O stdlib/oop.$O \
 stdlib/parsing.cmx stdlib/parsingp.cmx stdlib/parsing.$O stdlib/parsingp.$O \
 stdlib/pervasives.cmx stdlib/pervasivesp.cmx stdlib/pervasives.$O stdlib/pervasivesp.$O \
 stdlib/printexc.cmx stdlib/printexcp.cmx stdlib/printexc.$O stdlib/printexcp.$O \
 stdlib/printf.cmx stdlib/printfp.cmx stdlib/printf.$O stdlib/printfp.$O \
 stdlib/queue.cmx stdlib/queuep.cmx stdlib/queue.$O stdlib/queuep.$O \
 stdlib/random.cmx stdlib/randomp.cmx stdlib/random.$O stdlib/randomp.$O \
 stdlib/scanf.cmx stdlib/scanfp.cmx stdlib/scanf.$O stdlib/scanfp.$O \
 stdlib/sort.cmx stdlib/sortp.cmx stdlib/sort.$O stdlib/sortp.$O \
 stdlib/stack.cmx stdlib/stackp.cmx stdlib/stack.$O stdlib/stackp.$O \
 stdlib/stdLabels.cmx stdlib/stdLabelsp.cmx stdlib/stdLabels.$O stdlib/stdLabelsp.$O \
 stdlib/std_exit.cmx stdlib/std_exitp.cmx stdlib/std_exit.$O stdlib/std_exitp.$O \
 stdlib/stream.cmx stdlib/streamp.cmx stdlib/stream.$O stdlib/streamp.$O \
 stdlib/string.cmx stdlib/stringp.cmx stdlib/string.$O stdlib/stringp.$O \
 stdlib/stringLabels.cmx stdlib/stringLabelsp.cmx stdlib/stringLabels.$O stdlib/stringLabelsp.$O \
 stdlib/sys.cmx stdlib/sysp.cmx stdlib/sys.$O stdlib/sysp.$O \
 stdlib/weak.cmx stdlib/weakp.cmx stdlib/weak.$O stdlib/weakp.$O \
 stdlib/$set.cmx stdlib/$setp.cmx stdlib/$set.$O stdlib/$setp.$O \
  $LIBDIR

installlibdir \
  stdlib/stdlib.$A stdlib/stdlibp.$A \
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
  otherlibs/graph/graphics.cma otherlibs/win32/graph/graphics.cma \
  otherlibs/num/nums.cma \
  otherlibs/str/str.cma \
  otherlibs/unix/unix.cma otherlibs/win32unix/unix.cma \
  otherlibs/bigarray/bigarray.cmxa otherlibs/bigarray/bigarray.p.cmxa \
  otherlibs/dbm/dbm.cmxa otherlibs/dbm/dbm.p.cmxa \
  otherlibs/graph/graphics.cmxa otherlibs/graph/graphics.p.cmxa \
  otherlibs/win32/graph/graphics.cmxa otherlibs/win32graph/graphics.p.cmxa \
  otherlibs/num/nums.cmxa otherlibs/num/nums.p.cmxa \
  otherlibs/str/str.cmxa otherlibs/str/str.p.cmxa \
  otherlibs/unix/unix.cmxa otherlibs/unix/unix.p.cmxa \
  otherlibs/win32unix/unix.cmxa otherlibs/win32unix/unix.p.cmxa \
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
  otherlibs/systhreads/threads.p.cmxa \
  otherlibs/systhreads/thread.cmi \
  otherlibs/systhreads/mutex.cmi \
  otherlibs/systhreads/condition.cmi \
  otherlibs/systhreads/event.cmi \
  otherlibs/systhreads/threadUnix.cmi \
  $LIBDIR/threads

installdir \
  otherlibs/bigarray/dllbigarray$EXT_DLL \
  otherlibs/dbm/dllmldbm$EXT_DLL \
  otherlibs/graph/dllgraphics$EXT_DLL \
  otherlibs/win32graph/dllgraphics$EXT_DLL \
  otherlibs/num/dllnums$EXT_DLL \
  otherlibs/str/dllstr$EXT_DLL \
  otherlibs/systhreads/dllthreads$EXT_DLL \
  otherlibs/unix/dllunix$EXT_DLL \
  otherlibs/win32unix/dllunix$EXT_DLL \
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
  otherlibs/bigarray/libbigarray.$A otherlibs/bigarray/libbigarray.p.$A \
  otherlibs/dbm/libmldbm.$A otherlibs/dbm/libmldbm.p.$A \
  otherlibs/graph/libgraphics.$A \
  otherlibs/graph/libgraphics.p.$A \
  otherlibs/win32graph/libgraphics.$A \
  otherlibs/win32graph/libgraphics.p.$A \
  otherlibs/num/libnums.$A \
  otherlibs/num/libnums.p.$A \
  otherlibs/str/libstr.$A \
  otherlibs/str/libstr.p.$A \
  otherlibs/systhreads/libthreads.$A \
  otherlibs/systhreads/libthreads.p.$A \
  otherlibs/systhreads/libthreadsnat.$A \
  otherlibs/systhreads/libthreadsnat.p.$A \
  otherlibs/unix/libunix.$A \
  otherlibs/unix/libunix.p.$A \
  otherlibs/win32unix/libunix.$A \
  otherlibs/win32unix/libunix.p.$A \
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
  otherlibs/win32graph/graphicsX11.cmi \
  otherlibs/dynlink/dynlink.cmi \
  otherlibs/num/arith_status.cmi \
  otherlibs/num/big_int.cmi \
  otherlibs/num/nat.cmi \
  otherlibs/num/num.cmi \
  otherlibs/num/ratio.cmi \
  otherlibs/bigarray/bigarray.cmi \
  otherlibs/dbm/dbm.cmi \
  otherlibs/graph/graphics.cmi \
  otherlibs/win32graph/graphics.cmi \
  otherlibs/str/str.cmi \
  otherlibs/unix/unix.cmi \
  otherlibs/win32unix/unix.cmi \
  otherlibs/unix/unixLabels.cmi \
  otherlibs/win32unix/unixLabels.cmi \
  otherlibs/num/arith_flags.cmx \
  otherlibs/num/arith_flags.$O \
  otherlibs/num/arith_flags.p.cmx \
  otherlibs/num/arith_flags.p.$O \
  otherlibs/num/int_misc.cmx \
  otherlibs/num/int_misc.p.$O \
  otherlibs/num/int_misc.cmx \
  otherlibs/num/int_misc.p.$O \
  otherlibs/num/arith_status.cmx \
  otherlibs/num/arith_status.p.$O \
  otherlibs/num/arith_status.cmx \
  otherlibs/num/arith_status.p.$O \
  otherlibs/num/big_int.cmx \
  otherlibs/num/big_int.p.$O \
  otherlibs/num/big_int.cmx \
  otherlibs/num/big_int.p.$O \
  otherlibs/num/nat.cmx \
  otherlibs/num/nat.p.$O \
  otherlibs/num/nat.cmx \
  otherlibs/num/nat.p.$O \
  otherlibs/num/num.cmx \
  otherlibs/num/num.p.$O \
  otherlibs/num/num.cmx \
  otherlibs/num/num.p.$O \
  otherlibs/num/ratio.cmx \
  otherlibs/num/ratio.p.$O \
  otherlibs/num/ratio.cmx \
  otherlibs/num/ratio.p.$O \
  otherlibs/bigarray/bigarray.cmx \
  otherlibs/bigarray/bigarray.p.$O \
  otherlibs/bigarray/bigarray.cmx \
  otherlibs/bigarray/bigarray.p.$O \
  otherlibs/dbm/dbm.cmx \
  otherlibs/dbm/dbm.p.$O \
  otherlibs/dbm/dbm.cmx \
  otherlibs/dbm/dbm.p.$O \
  otherlibs/graph/graphics.cmx \
  otherlibs/graph/graphics.$O \
  otherlibs/graph/graphics.p.cmx \
  otherlibs/graph/graphics.p.$O \
  otherlibs/win32graph/graphics.cmx \
  otherlibs/win32graph/graphics.$O \
  otherlibs/win32graph/graphics.p.cmx \
  otherlibs/win32graph/graphics.p.$O \
  otherlibs/str/str.cmx \
  otherlibs/str/str.p.$O \
  otherlibs/str/str.cmx \
  otherlibs/str/str.p.$O \
  otherlibs/unix/unix.cmx \
  otherlibs/unix/unix.$O \
  otherlibs/unix/unix.p.cmx \
  otherlibs/unix/unix.p.$O \
  otherlibs/win32unix/unix.cmx \
  otherlibs/win32unix/unix.$O \
  otherlibs/win32unix/unix.p.cmx \
  otherlibs/win32unix/unix.p.$O \
  otherlibs/unix/unixLabels.cmx \
  otherlibs/unix/unixLabels.$O \
  otherlibs/unix/unixLabels.p.cmx \
  otherlibs/unix/unixLabels.p.$O \
  otherlibs/win32unix/unixLabels.cmx \
  otherlibs/win32unix/unixLabels.$O \
  otherlibs/win32unix/unixLabels.p.cmx \
  otherlibs/win32unix/unixLabels.p.$O \
  $LIBDIR

installlibdir \
  otherlibs/bigarray/bigarray.$A \
  otherlibs/bigarray/bigarray.p.$A \
  otherlibs/dbm/dbm.$A \
  otherlibs/dbm/dbm.p.$A \
  otherlibs/graph/graphics.$A \
  otherlibs/graph/graphics.p.$A \
  otherlibs/win32graph/graphics.$A \
  otherlibs/win32graph/graphics.p.$A \
  otherlibs/num/nums.$A \
  otherlibs/num/nums.p.$A \
  otherlibs/str/str.$A \
  otherlibs/str/str.p.$A \
  otherlibs/unix/unix.$A \
  otherlibs/unix/unix.p.$A \
  otherlibs/win32unix/unix.$A \
  otherlibs/win32unix/unix.p.$A \
  stdlib/stdlib.$A \
  stdlib/stdlib.p.$A \
  $LIBDIR

installlibdir \
  otherlibs/systhreads/threads.$A \
  otherlibs/systhreads/threads.p.$A \
  $LIBDIR/threads

echo "Installing manuals..."
(cd ../man && make install)

echo "Installing ocamldoc..."
installbin ocamldoc/ocamldoc$EXE $BINDIR/ocamldoc$EXE
installbin ocamldoc/ocamldoc.opt$EXE $BINDIR/ocamldoc.opt$EXE

installdir \
  ../ocamldoc/ocamldoc.hva \
  ocamldoc/*.cmi \
  ocamldoc/odoc_info.mli ocamldoc/odoc_infor.cm[ia] ocamldoc/odoc_info.cmxa \
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
  $CAMLP4DIR
installlibdir camlp4lib.$A camlp4fulllib.$A $CAMLP4DIR
cd ..

echo "Installing ocamlbuild..."

installbin ocamlbuild/ocamlbuild.byte$EXE $BINDIR/ocamlbuild.byte$EXE
installbin ocamlbuild/ocamlbuild.native$EXE $BINDIR/ocamlbuild.native$EXE
installbestbin ocamlbuild/ocamlbuild.native$EXE ocamlbuild/ocamlbuild.byte$EXE $BINDIR/ocamlbuild$EXE

installlibdir \
  ocamlbuild/ocamlbuildlib.$A ocamlbuild/ocamlbuildlib.p.$A \
  $LIBDIR/ocamlbuild

installdir \
  ocamlbuild/ocamlbuildlib.cmxa ocamlbuild/ocamlbuildlib.p.cmxa \
  ocamlbuild/ocamlbuildlib.cma \
  ocamlbuild/ocamlbuild_plugin.cmi \
  ocamlbuild/ocamlbuild_pack.cmi \
  ocamlbuild/ocamlbuild.cmo \
  ocamlbuild/ocamlbuild.cmx \
  ocamlbuild/ocamlbuild.$O \
  ocamlbuild/ocamlbuild.p.cmx \
  ocamlbuild/ocamlbuild.p.$O \
  $LIBDIR/ocamlbuild

installdir \
  ../ocamlbuild/man/ocamlbuild.1 \
  $MANDIR/man1
