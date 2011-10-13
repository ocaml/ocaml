#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#         Nicolas Pouillard, projet Gallium, INRIA Rocquencourt         #
#                                                                       #
#   Copyright 2008 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

# $Id$

OTHERLIBS_BYTE=""
OTHERLIBS_NATIVE=""
OTHERLIBS_UNIX_NATIVE=""
UNIXDIR="otherlibs/unix"

add_native() {
  for native_file in $@; do
    OTHERLIBS_NATIVE="$OTHERLIBS_NATIVE otherlibs/$lib/$native_file"
    case $lib in
    unix|win32unix)
      OTHERLIBS_UNIX_NATIVE="$OTHERLIBS_UNIX_NATIVE otherlibs/$lib/$native_file";;
    esac
  done
}

add_byte() {
  for byte_file in $@; do
    OTHERLIBS_BYTE="$OTHERLIBS_BYTE otherlibs/$lib/$byte_file"
  done
}

add_file() {
  add_byte $@
  add_native $@
}

add_bin() {
  for bin_file in $@; do
    add_byte $bin_file.byte$EXE
    add_native $bin_file.native$EXE
  done
}

add_c_lib() {
  add_file "lib$1.$A"
}

add_ocaml_lib() {
  add_native "$1.cmxa"
  add_native "$1.$A"
  add_byte "$1.cma"
}

add_dll() {
  add_file "dll$1$EXT_DLL"
}

add() {
  add_c_lib $1
  add_ocaml_lib $1
  add_dll $1
}

THREADS_CMIS="thread.cmi mutex.cmi condition.cmi event.cmi threadUnix.cmi"

for lib in $OTHERLIBRARIES; do
  case $lib in
  num)
    add nums;;
  systhreads)
    add_ocaml_lib threads
    add_dll threads
    add_file $THREADS_CMIS
    add_byte libthreads.$A
    add_native libthreadsnat.$A;;
  graph|win32graph)
    add graphics;;
  threads)
    add_byte pervasives.cmi pervasives.mli \
             $THREADS_CMIS marshal.cmi marshal.mli \
             stdlib.cma unix.cma threads.cma libvmthreads.$A;;
  labltk)
    add_file      support/camltk.h
    add_byte      support/byte.otarget
    add_native    support/native.otarget
    add_file      support/liblabltk.$A
    add_byte      compiler/tkcompiler$EXE compiler/pp$EXE
    add_file      labltk/tk.ml labltk/labltk.ml
    add_byte      labltk/byte.otarget
    add_native    labltk/native.otarget
    add_byte      camltk/byte.otarget
    add_native    camltk/native.otarget
    add_ocaml_lib lib/labltk
    add_byte      lib/labltktop$EXE lib/labltk$EXE
    add_ocaml_lib jpf/jpflib
    add_ocaml_lib frx/frxlib
    add_byte      browser/ocamlbrowser$EXE
    ;;
  dbm)
    add_ocaml_lib dbm
    add_c_lib mldbm;;
  dynlink)
    add_ocaml_lib dynlink
    add_native dynlink.cmx dynlink.$O
    add_file $lib.cmi extract_crc;;
  win32unix)
    UNIXDIR="otherlibs/win32unix"
    add_file unixsupport.h cst2constr.h socketaddr.h
    add unix;;
  unix)
    add_file unixsupport.h
    add unix;;
  *)
    add $lib
  esac
done
