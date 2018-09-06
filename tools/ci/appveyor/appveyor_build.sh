#!/bin/bash
#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*                         Christophe Troestler                           *
#*                                                                        *
#*   Copyright 2015 Christophe Troestler                                  *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

BUILD_PID=0

function run {
    NAME=$1
    shift
    echo "-=-=- $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    $@
    CODE=$?
    if [ $CODE -ne 0 ]; then
        echo "-=-=- $NAME failed! -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
        if [ $BUILD_PID -ne 0 ] ; then
          kill -KILL $BUILD_PID 2>/dev/null
          wait $BUILD_PID 2>/dev/null
        fi
        exit $CODE
    else
        echo "-=-=- End of $NAME -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-"
    fi
}

function set_configuration {
    cp config/m-nt.h runtime/caml/m.h
    cp config/s-nt.h runtime/caml/s.h

    FILE=$(pwd | cygpath -f - -m)/config/Makefile
    echo "Edit $FILE to set PREFIX=$2"
    sed -e "/PREFIX=/s|=.*|=$2|" \
        -e "/RUNTIMED=/s|=.*|=true|" \
        -e "/^ *OC_CFLAGS *=/s/\r\?$/ $3\0/" \
         config/Makefile.$1 > config/Makefile
#    run "Content of $FILE" cat config/Makefile
}

APPVEYOR_BUILD_FOLDER=$(echo $APPVEYOR_BUILD_FOLDER| cygpath -f -)
# These directory names are specified here, because getting UTF-8 correctly
# through appveyor.yml -> Command Script -> Bash is quite painful...
OCAMLROOT=$(echo $PROGRAMFILES/Бактріан🐫| cygpath -f - -m)

# This must be kept in sync with appveyor_build.cmd
BUILD_PREFIX=🐫реализация

export PATH=$(echo $OCAMLROOT| cygpath -f -)/bin/flexdll:$PATH

case "$1" in
  install)
    mkdir -p "$OCAMLROOT/bin/flexdll"
    cd $APPVEYOR_BUILD_FOLDER/../flexdll
    # msvc64 objects need to be compiled with VS2015, so are copied later from
    # a source build.
    for f in flexdll.h flexlink.exe flexdll*_msvc.obj default*.manifest ; do
      cp $f "$OCAMLROOT/bin/flexdll/"
    done
    if [ "$PORT" = "msvc64" ] ; then
      echo 'eval $($APPVEYOR_BUILD_FOLDER/tools/msvs-promote-path)' \
        >> ~/.bash_profile
    fi
    ;;
  msvc32-only)
    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-msvc32

    set_configuration msvc "$OCAMLROOT-msvc32" -WX

    run "make world" make world
    run "make runtimeopt" make runtimeopt
    run "make -C otherlibs/systhreads libthreadsnat.lib" \
         make -C otherlibs/systhreads libthreadsnat.lib

    exit 0
    ;;
  test)
    FULL_BUILD_PREFIX=$APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX
    run "ocamlc.opt -version" $FULL_BUILD_PREFIX-$PORT/ocamlc.opt -version
    run "test $PORT" make -C $FULL_BUILD_PREFIX-$PORT tests
    run "install $PORT" make -C $FULL_BUILD_PREFIX-$PORT install
    if [ "$PORT" = "msvc64" ] ; then
      run "check_all_arches" make -C $FULL_BUILD_PREFIX-$PORT check_all_arches
    fi
    ;;
  *)
    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-$PORT

    if [ "$PORT" = "msvc64" ] ; then
      tar -xzf $APPVEYOR_BUILD_FOLDER/flexdll.tar.gz
      cd flexdll-$FLEXDLL_VERSION
      make MSVC_DETECT=0 CHAINS=msvc64 support
      cp flexdll*_msvc64.obj "$OCAMLROOT/bin/flexdll/"
      cd ..
    fi

    if [ "$PORT" = "msvc64" ] ; then
      set_configuration msvc64 "$OCAMLROOT" -WX
    else
      set_configuration mingw "$OCAMLROOT-mingw32" -Werror
    fi

    cd $APPVEYOR_BUILD_FOLDER/../$BUILD_PREFIX-$PORT

    export TERM=ansi

    if [ "$PORT" = "mingw32" ] ; then
      # For an explanation of the sed command, see
      # https://github.com/appveyor/ci/issues/1824
      script --quiet --return --command \
        "make -C ../$BUILD_PREFIX-mingw32 flexdll world.opt" \
        ../$BUILD_PREFIX-mingw32/build.log |
          sed -e 's/\d027\[K//g' \
              -e 's/\d027\[m/\d027[0m/g' \
              -e 's/\d027\[01\([m;]\)/\d027[1\1/g'
    else
      run "make world" make world
      run "make bootstrap" make bootstrap
      run "make opt" make opt
      run "make opt.opt" make opt.opt
    fi

    ;;
esac
